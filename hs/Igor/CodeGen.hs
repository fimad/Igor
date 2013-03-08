{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Igor.CodeGen
( 
-- * Types
  Variable
, CodeGenState
, MemoryAccessType (..)
, Partial
, Program
, GeneratedCode(..)
-- * Methods
, generate
, makeLabel
, makeLabels
, label
, makeLocal
, makeLocals
, makeInput
, makeInputs
-- ** Params
, Param (..)
, Paramable
, withParam
, asRegister
--, asSavedRegister
, saveAsRegister
, claimRegister
, claimRegisterIfNot
, withTempRegister
, withTempRegister'
, reserveRegisterFor
-- ** Statements
, move
, add
, sub
, xor
, mul
, jump
, noop
, ret
-- *** Jump Reasons
, (-<-)
, (-<=-)
, (->-)
, (->=-)
, (-==-)
, (-!=-)
, always
) where

import              Control.Applicative
import              Control.Monad.State
import              Data.Bits (shiftR)
import qualified    Data.ByteString         as B
import              Data.Either
import              Data.Function
import              Data.Foldable (foldr', foldrM)
import              Data.List
import qualified    Data.Map                as M
import              Data.Maybe
import qualified    Data.Set                as S
import              Data.Random
import              Data.Random.List
import              Data.Random.Sample
import qualified    Igor.Expr               as X
import qualified    Igor.Gadget             as G
import qualified    Igor.Gadget.Discovery   as D
--import              Hdis86
import              System.Random

-- | Variables are the type passed to the user of the library and currently the
-- arguments to the predicate functions. This may change in the future for when
-- constants are added.
newtype Variable        = Variable {unVariable :: Integer}
    deriving (Ord,Eq,Show,Read)

-- | Labels correspond to the 0 indexed position in the sequence of generated
-- bytes.
newtype Label           = Label {unLabel :: Integer}
    deriving (Ord,Eq,Show,Read)

-- | A location pool is the known locations that are free to use as scratch
-- space or new variables.
type LocationPool       = S.Set X.Register

-- | A mapping from Variables to locations.
type VariableMap        = M.Map Variable Param

-- | There is not type difference between Programs and statements, though for
-- clarity sake statements will have type 'Statement' and complete methods
-- (sequences of 'Statement's) should have type Program
type Program            = StateT CodeGenState [] ()
type Statement          = Program

-- | A Partial is a part of a statement, it performs some underlying code
-- generation task and returns a value.
type Partial a          = StateT CodeGenState [] a

-- | The type of memory access that we will be performing. This isn't strictly
-- necessary but is very useful in reducing the search space.
data MemoryAccessType   = R | W

-- | The types of parameters that can be passed to a partial statement
data Param              = Constant  Integer
                        | Register  X.Register
                        | Memory    X.Address

class Paramable a where
    -- | This method takes a 'Paramable' type and a tuple of Partials. The first
    -- modifies the code generation state and returns a Param corresponding to
    -- the value in a. The second handles tearing down the Param in the event
    -- that temporary registers were used.
    --toParam :: a -> (Partial Param, Partial ())
    -- | This method takes a 'Paramable' type and a method that acts on that
    -- as a 'Param'. The result is a Partial that has the same return value as
    -- the method but also handles any set up and tear down associated with
    -- building the 'Param'
    withParam :: Paramable a => a -> (Param -> Partial b) -> Partial b

-- | The result of compilation.
data GeneratedCode       = GeneratedCode {
        byteCode            :: B.ByteString
    ,   localVariableSize   :: Integer
}

-- | The condition upon which the jump depends
data JumpReason         = Always
                        | Because X.Reason Param Param

-- | Because it is not possible to know the byte offsets for code while it is
-- being generated, jumps are first translated as a JumpHolder with a fixed
-- size. In a second pass it is then translated into an appropriate sequence of
-- bytes.
data JumpHolder = JumpHolder {
        jumpFlavor      :: Integer -> G.Gadget -- ^ The type of jump that we should place here
    ,   jumpPosition    :: Integer -- ^ The byte offset of the jump
    ,   jumpLength      :: Integer -- ^ How big is the jump holder
    ,   jumpTarget      :: Label -- ^ The offset into the generated code to jump to
    ,   jumpClobberable :: LocationPool -- ^ A copy of the clobberable locations at the time the jump was made
}

data CodeGenState       = CodeGenState {
        library             :: D.GadgetLibrary
    ,   randomGenerator     :: StdGen
    ,   variableMap         :: VariableMap
    ,   labelMap            :: M.Map Label (Maybe Integer)
    ,   locationPool        :: LocationPool
    ,   generatedCode       :: [Either JumpHolder B.ByteString]
    ,   localVariableOffset :: Integer -- ^ How far from EBP should we assign the next local variable?
    ,   endOfCodeLabel      :: Label -- ^ Marks the end of body. Used for jumping out to return.
    ,   inputVariableOffset :: Integer -- ^ How much room on the stack is reserved for the input variables
    -- | Map from offset to maps from the base register to value registers
    ,   validOffsetReads    :: M.Map X.Value (M.Map X.Register [X.Register])
    ,   validOffsetWrites   :: M.Map X.Value (M.Map X.Register [X.Register])
    -- | Map from (scale,offset) to maps from the base register to maps from the index to value registers
    ,   validIndexedReads   :: M.Map (X.Value,X.Value) (M.Map X.Register (M.Map X.Register [X.Register]))
    ,   validIndexedWrites  :: M.Map (X.Value,X.Value) (M.Map X.Register (M.Map X.Register [X.Register]))
    }

initialState :: D.GadgetLibrary -> StdGen -> CodeGenState
initialState library gen = CodeGenState {
        library             = library
    ,   randomGenerator     = gen
    ,   variableMap         = M.empty
    ,   labelMap            = M.empty
    ,   locationPool        = shuffledPool
    ,   generatedCode       = []
    ,   localVariableOffset = 0
     -- | NOTE: This is dependent on the number of registers saved before moving
     -- ebp to esp, which is dependent on the backend scaffolding ... ugh....
    ,   inputVariableOffset = 16
    ,   endOfCodeLabel      = Label 1 -- ^ The end of code label will always be the first label made.
    ,   validOffsetReads    = foldl' getValidOffsetReads M.empty $ M.keys $ D.gadgetMap library
    ,   validOffsetWrites   = foldl' getValidOffsetWrites M.empty $ M.keys $ D.gadgetMap library
    ,   validIndexedReads    = foldl' getValidIndexedReads M.empty $ M.keys $ D.gadgetMap library
    ,   validIndexedWrites    = foldl' getValidIndexedWrites M.empty $ M.keys $ D.gadgetMap library
    }
    where
        --pool                    = map X.RegisterLocation X.generalRegisters
        --(shuffledPool, gen')    = sampleState (shuffle pool) gen
        shuffledPool    = S.fromList X.generalRegisters
        
        getValidOffsetReads theMap (G.LoadMemReg valReg (X.OffsetAddress baseReg offset)) = 
            M.insert offset (M.insert baseReg (valReg:valueRegs) offsetMap) theMap
            where
                offsetMap   = fromMaybe M.empty $ offset `M.lookup` theMap
                valueRegs   = fromMaybe [] $ baseReg `M.lookup` offsetMap
        getValidOffsetReads theMap _  = theMap
        
        getValidOffsetWrites theMap (G.StoreMemReg (X.OffsetAddress baseReg offset) valReg) = 
            M.insert offset (M.insert baseReg (valReg:valueRegs) offsetMap) theMap
            where
                offsetMap   = fromMaybe M.empty $ offset `M.lookup` theMap
                valueRegs   = fromMaybe [] $ baseReg `M.lookup` offsetMap
        getValidOffsetWrites theMap _  = theMap
        
        getValidIndexedReads theMap (G.LoadMemReg valReg (X.IndexedAddress baseReg indexReg scale offset)) = 
            M.insert (scale,offset) (M.insert baseReg (M.insert indexReg (valReg:valueRegs) baseMap) offsetMap) theMap
            where
                offsetMap   = fromMaybe M.empty $ (scale,offset) `M.lookup` theMap
                baseMap     = fromMaybe M.empty $ baseReg `M.lookup` offsetMap
                valueRegs   = fromMaybe [] $ indexReg `M.lookup` baseMap
        getValidIndexedReads theMap _  = theMap
        
        getValidIndexedWrites theMap (G.StoreMemReg (X.IndexedAddress baseReg indexReg scale offset) valReg) = 
            M.insert (scale,offset) (M.insert baseReg (M.insert indexReg (valReg:valueRegs) baseMap) offsetMap) theMap
            where
                offsetMap   = fromMaybe M.empty $ (scale,offset) `M.lookup` theMap
                baseMap     = fromMaybe M.empty $ baseReg `M.lookup` offsetMap
                valueRegs   = fromMaybe [] $ indexReg `M.lookup` baseMap
        getValidIndexedWrites theMap _  = theMap
        

--------------------------------------------------------------------------------
-- Param Handling
--------------------------------------------------------------------------------

instance Paramable Integer where
    withParam a = ($ Constant a)

instance Paramable Variable where
    withParam var method = do
        CodeGenState {..}   <- get
        param               <- lift $ maybeToList $ M.lookup var variableMap
        method param

instance Paramable X.Location where
    withParam (X.MemoryLocation addr)   = ($ Memory addr)
    withParam (X.RegisterLocation reg)  = ($ Register reg)

instance Paramable Param where
    withParam = flip ($)

-- | An offset memory dereference.
instance (Paramable a, Integral b) => Paramable (MemoryAccessType,a,b) where
    -- | By immediately loading the value into a register it prevents us from
    -- holding many temporary registers that can be used elsewhere.
    withParam (R,baseParam,offset) method = do
        valueReg <-
            asRegister baseParam $ \baseReg ->
                asRegister (Memory $ X.OffsetAddress baseReg (fromIntegral offset)) $ \valueReg ->
                    return valueReg
        reserveRegisterFor valueReg $ method (Register valueReg)

    withParam (W,baseParam,offset) method = do
        CodeGenState{..}    <-  get
        baseMap             <-  lift $ maybeToList $ (fromIntegral offset) `M.lookup` validOffsetWrites
        claimRegister (M.keys baseMap) $ \tmpBaseReg -> do
            asRegister baseParam $ \baseReg ->
                compileGadget $ G.LoadReg tmpBaseReg baseReg
            let valueRegs = (join $ maybeToList $ tmpBaseReg `M.lookup` baseMap) 
            claimRegister valueRegs $ \valueReg -> do
                result <- method (Register valueReg)
                compileGadget $ G.StoreMemReg (X.OffsetAddress tmpBaseReg (fromIntegral offset) ) valueReg 
                return result

-- | An indexed memory dereference.
instance (Paramable a, Paramable b, Integral c, Integral d) => Paramable (MemoryAccessType,a,b,c,d) where
    withParam (R,baseParam,indexParam,scale,offset) method = do
        valueReg <-
            asRegister baseParam $ \baseReg ->
            asRegister indexParam $ \indexReg ->
                asRegister (Memory $ X.IndexedAddress baseReg indexReg (fromIntegral scale) (fromIntegral offset)) $ \valueReg ->
                    return valueReg
        reserveRegisterFor valueReg $ method (Register valueReg)

-- | Returns a shuffled version of location pool wrapped in a partial.
randomLocationPool :: Partial [X.Register]
randomLocationPool = do
    state@CodeGenState{..}      <- get
    let (shuffledPool, gen')    = sampleState (shuffle $ S.toList locationPool) randomGenerator
    put $ state { randomGenerator = gen' }
    return shuffledPool

-- | If the given register is in the location pool it is pulled for the duration
-- of the given partial.
reserveRegisterFor :: X.Register -> Partial b -> Partial b
reserveRegisterFor reg method = do
    state@CodeGenState{..}  <- get
    put                     $! state { locationPool = locationPool S.\\ S.singleton reg }
    result                  <- method
    -- Restore temp to the location pool if it was originally free
    state                   <- get
    put                     $! state { locationPool = locationPool }
    --if reg `S.member` locationPool
    --    then do
    --        state@CodeGenState{..}  <- get
    --        put $ state {
    --            locationPool = reg `S.insert` locationPool
    --        }
    --    else return ()
    return result

-- | Allocates one of the specified registers from the location pool. Will not
-- succeed if one of the desired registers is not available.
claimRegister :: [X.Register] -> (X.Register -> Partial b) -> Partial b
claimRegister desired method = do
    state@CodeGenState{..}  <-  get
    let available           =   S.fromList desired `S.intersection` locationPool
    chosen                  <-  lift $ S.toList available
    reserveRegisterFor chosen $ method chosen

-- | Similar to 'claimRegister' except that if the chosen register is equal to
-- the first parameter it will not attempt to claim the register (typically
-- because it has already been claimed).
claimRegisterIfNot :: X.Register -> [X.Register] -> (X.Register -> Partial b) -> Partial b
claimRegisterIfNot thisRegister otherRegisters method = do
    chosenRegister <- lift $ otherRegisters
    if chosenRegister == thisRegister
        then method thisRegister
        else claimRegister [chosenRegister] method
    

-- | Allocates and a temporary register for use with a method and then
-- subsequently frees it once the method is finished. The first parameter is a
-- list of nonFreeRegs that also qualify as temporary registers in this
-- particular instance (e.g. if they will be overwritten after whatever
-- operation is complete)
withTempRegister :: [X.Register] -> (X.Register -> Partial b) -> Partial b
withTempRegister nonFreeRegs method = do
    state@CodeGenState{..}  <- get
    --tempReg                 <- lift $ nonFreeRegs ++ S.toList locationPool
    tempReg                 <- lift =<< (++) <$> return nonFreeRegs <*> randomLocationPool
    reserveRegisterFor tempReg $ method tempReg

-- | Same as 'withTempRegister' except that the first argument has been
-- pre-applied as []
withTempRegister' :: (X.Register -> Partial b) -> Partial b
withTempRegister' = withTempRegister []

-- | Create's a temporary register passes it to method and then once method is
-- complete, stores the value in param.
saveAsRegister :: Paramable p => p -> (X.Register -> Partial b) -> Partial b
saveAsRegister paramable method =
    withParam paramable $ savingMethod method
    where
        savingMethod method (Register dstReg) =
            withTempRegister [dstReg] $ \reg -> do
                result <- method reg
                compileGadget $ G.LoadReg dstReg reg
                return result

        savingMethod method (Memory (X.OffsetAddress baseReg offset)) = do
            CodeGenState{..}    <-  get
            baseMap             <-  lift $ maybeToList $ offset `M.lookup` validOffsetWrites
            claimRegisterIfNot baseReg (baseReg : M.keys baseMap) $ \tmpBaseReg -> do
                compileGadget $ G.LoadReg tmpBaseReg baseReg
                let valueRegs = (join $ maybeToList $ tmpBaseReg `M.lookup` baseMap) 
                claimRegister valueRegs $ \valueReg -> do
                    result <- method valueReg
                    compileGadget $ G.StoreMemReg (X.OffsetAddress tmpBaseReg offset) valueReg 
                    return result

        savingMethod method (Memory (X.IndexedAddress baseReg indexReg scale offset)) = do
            CodeGenState{..}    <-  get
            baseMap             <-  lift $ maybeToList $ (scale,offset) `M.lookup` validIndexedWrites
            claimRegisterIfNot baseReg (baseReg : M.keys baseMap) $ \tmpBaseReg -> do
                indexMap        <-  lift $ maybeToList $ tmpBaseReg `M.lookup` baseMap
                compileGadget $ G.LoadReg tmpBaseReg baseReg
                claimRegisterIfNot indexReg (indexReg : M.keys indexMap) $ \tmpIndexReg -> do
                    compileGadget $ G.LoadReg tmpIndexReg indexReg
                    let valueRegs = (join $ maybeToList $ tmpIndexReg `M.lookup` indexMap) 
                    claimRegister valueRegs $ \valueReg -> do
                        result <- method valueReg
                        compileGadget $ G.StoreMemReg (X.IndexedAddress tmpBaseReg tmpIndexReg scale offset) valueReg 
                        return result

        savingMethod _                  reg = error "Attempting to save to a non-location."

-- | Moves the value described by a 'Paramable' into a temporary register that
-- will be freed at the end of the method. Note that this method will fail if
-- used on 'Location's this is because it is not possible to determine their
-- values at compile time.
asRegister :: Paramable p => p -> (X.Register -> Partial b) -> Partial b
asRegister paramable method =
    withParam paramable $ flip asRegister' method
    where
        asRegister' :: Param -> (X.Register -> Partial b) -> Partial b
        asRegister' (Register reg)     method =
            let
                -- | Allow reg to be used as a scratch register if it is a general
                -- purpose register, otherwise we have to allocate a new one.
                possiblyReg = if reg `elem` X.generalRegisters
                                then [reg]
                                else []
            in
                withTempRegister possiblyReg $ \tempReg -> do
                    reserveRegisterFor reg $
                        compileGadget         $ G.LoadReg tempReg reg
                    method tempReg

        asRegister' (Memory addr@(X.OffsetAddress baseReg offset)) method = do
            CodeGenState{..}    <-  get
            baseMap             <-  lift $ maybeToList $ offset `M.lookup` validOffsetReads
            valueReg            <- 
                claimRegisterIfNot baseReg (baseReg : M.keys baseMap) $ \tmpBaseReg -> do
                    compileGadget $ G.LoadReg tmpBaseReg baseReg
                    let valueRegs = (join $ maybeToList $ tmpBaseReg `M.lookup` baseMap) 
                    claimRegister valueRegs $ \valueReg -> do
                        compileGadget $ G.LoadMemReg valueReg (X.OffsetAddress tmpBaseReg offset)
                        return valueReg
            reserveRegisterFor valueReg $ method valueReg

        asRegister' (Memory addr@(X.IndexedAddress baseReg indexReg scale offset)) method = do
            CodeGenState{..}    <-  get
            baseMap             <-  lift $ maybeToList $ (scale,offset) `M.lookup` validIndexedWrites
            valueReg            <-
                claimRegisterIfNot baseReg (baseReg : M.keys baseMap) $ \tmpBaseReg -> do
                    indexMap        <-  lift $ maybeToList $ tmpBaseReg `M.lookup` baseMap
                    compileGadget $ G.LoadReg tmpBaseReg baseReg
                    claimRegisterIfNot indexReg (indexReg : M.keys indexMap) $ \tmpIndexReg -> do
                        compileGadget $ G.LoadReg tmpIndexReg indexReg
                        let valueRegs = (join $ maybeToList $ tmpIndexReg `M.lookup` indexMap) 
                        claimRegister valueRegs $ \valueReg -> do
                            compileGadget $ G.StoreMemReg (X.IndexedAddress tmpBaseReg tmpIndexReg scale offset) valueReg 
                            return valueReg
            reserveRegisterFor valueReg $ method valueReg


        asRegister' (Constant value)   method = 
            withTempRegister' $ \valueReg -> do
                withTempRegister' $ \savedConstant ->
                    withTempRegister' $ \savedShift -> do
                        CodeGenState{..}                <- get
                        let poolSet                     = locationPool
                        let gmap                        = D.gadgetMap library
                        (constantGadget, constantLoc)   <- lift $ mapMaybe (getConstant poolSet) $ M.keys gmap
                        (shiftGadget, shiftLoc)         <- lift $ mapMaybe (getShift poolSet) $ M.keys gmap
                        withTempRegister [shiftLoc] $ \tempShiftReg -> do
                            -- Save the constant and shift registers in case they are special
                            unless (valueReg == constantLoc) $
                                compileGadget $ G.LoadReg savedConstant constantLoc
                            unless (valueReg == shiftLoc) $
                                compileGadget $ G.LoadReg savedShift shiftLoc
                            -- Actually perform the constant load and the shift
                            compileGadget $ constantGadget
                            compileGadget $ G.LoadReg tempShiftReg constantLoc
                            compileGadget $ G.LoadReg shiftLoc tempShiftReg
                            compileGadget $ shiftGadget
                            -- Move the constant into a temporary register
                            compileGadget $ G.LoadReg valueReg shiftLoc
                            -- Restore the constant and shift registers
                            unless (valueReg == constantLoc) $
                                compileGadget $ G.LoadReg constantLoc savedConstant
                            unless (valueReg == shiftLoc) $
                                compileGadget $ G.LoadReg shiftLoc savedShift
                -- Release the other temp registers and perform the desired method on
                -- the valueReg which now holds the desired constant!
                method valueReg

            where
                possibly :: X.Register -> Partial [X.Register]
                possibly reg = do
                    CodeGenState{..} <- get
                    if reg `S.member` locationPool
                                then return [reg]
                                else return []

                numBits             = ceiling $ logBase 2 (fromIntegral value+1)
                shiftSize           = 31 - numBits

                getConstant :: S.Set X.Register -> G.Gadget -> Maybe (G.Gadget, X.Register)
                getConstant poolSet g@(G.LoadConst reg val)
                    -- | not $ (X.RegisterLocation reg) `S.member` poolSet = Nothing
                    | shiftR val shiftSize == fromIntegral value        = Just (g, reg)
                    | otherwise                                         = Nothing
                getConstant _ _                                         = Nothing

                getShift :: S.Set X.Register -> G.Gadget -> Maybe (G.Gadget, X.Register)
                getShift poolSet g@(G.RightShift reg val)
                    -- | not $ (X.RegisterLocation reg) `S.member` poolSet = Nothing
                    | shiftSize == fromIntegral val                     = Just (g, reg)
                    | otherwise                                         = Nothing
                getShift _ _                                            = Nothing
                --

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Creates a randomized list of 'ByteString's for a specific gadget that
-- conform to a certain predicate. 
translateGadgetThat :: (B.ByteString -> Bool) -> CodeGenState -> G.Gadget -> RVarT [] B.ByteString
translateGadgetThat predicate state@CodeGenState{..} gadget = do
    !gadgetSet                  <- lift $ maybeToList $ D.libraryLookup gadget library
    shuffled                    <- shuffleT $! S.toList $ S.filter isAllowed gadgetSet
    (bytes,_)                   <- lift $ shuffled
    return bytes
    where
        isAllowed (bs,clobber) = predicate bs && doesNotClobber locationPool gadget (bs,clobber)

-- | Same as 'translateGadgetThat' with the predicate parameter pre-applied as
-- always being true.
translateGadget :: CodeGenState -> G.Gadget -> RVarT [] B.ByteString
translateGadget = translateGadgetThat (return True)

-- | Compiles a gadget to bytecode and places it in the sequence of generated
-- code.
compileGadget :: G.Gadget -> Partial ()
compileGadget gadget = do
    state@(CodeGenState{..})    <- get
    (bytes,gen)                 <- lift $ sampleStateT (translateGadget state gadget) randomGenerator
    let newCode                 = generatedCode ++ [Right bytes]
    put $! state {
            randomGenerator = gen
        ,   generatedCode   = newCode
        }

-- | Ensures that a gadget only clobbers the unallocated locations described in
-- the locationPool.
doesNotClobber locationPool _                   (_,[])         = True
doesNotClobber locationPool (G.Jump X.Always _) (_,clobber)    = doesNotClobber' (X.EFLAG `S.insert` locationPool) clobber
doesNotClobber locationPool (G.Jump _ _)        (_,clobber)    = doesNotClobber' locationPool clobber
doesNotClobber locationPool _                   (_,clobber)    = doesNotClobber' (X.EFLAG `S.insert` locationPool) clobber

doesNotClobber' locationPool clobber =
    let
        clobberSet              = S.fromList clobber
        clobberAbleLocations    = S.map X.RegisterLocation locationPool
    in
        clobberSet `S.union` clobberAbleLocations == clobberAbleLocations

-- | Returns the current byte offset
currentByteOffset :: Partial Integer
currentByteOffset = do
    CodeGenState{..}    <- get
    return $ sum $ map toSize generatedCode
    where
        toSize (Right bytes)            = fromIntegral $ B.length bytes
        toSize (Left (JumpHolder{..}))  = jumpLength

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

noop :: Statement
noop = do
    noops   <- lift $ drop 1 $ inits $ repeat G.NoOp
    mapM_ compileGadget noops

move :: (Paramable a, Paramable b) => a -> b -> Statement
move dst src =
    saveAsRegister dst $ \dstReg ->
    asRegister src $ \srcReg ->
        compileGadget $ G.LoadReg dstReg srcReg

add :: (Paramable a, Paramable b, Paramable c) => a -> b -> c -> Statement
add dst val1 val2 = 
    saveAsRegister dst $ \dstReg ->
    asRegister val1 $ \val1Reg ->
    asRegister val2 $ \val2Reg -> do
        compileGadget $ G.LoadReg dstReg val1Reg
        compileGadget $ G.Plus dstReg $ S.fromList [dstReg, val2Reg]

mul :: (Paramable a, Paramable b, Paramable c) => a -> b -> c -> Statement
mul dst val1 val2 = 
    claimRegister [X.EAX] $ \dstReg -> do
        asRegister val1 $ \val1Reg ->
            asRegister val2 $ \val2Reg -> do
                compileGadget $ G.LoadReg dstReg val1Reg
                compileGadget $ G.Times dstReg $ S.fromList [dstReg, val2Reg]
        saveAsRegister dst $ \dstReg ->
            compileGadget $ G.LoadReg dstReg X.EAX

xor :: (Paramable a, Paramable b, Paramable c) => a -> b -> c -> Statement
xor dst val1 val2 = 
    saveAsRegister dst $ \dstReg ->
    asRegister val1 $ \val1Reg ->
    asRegister val2 $ \val2Reg -> do
        compileGadget $ G.LoadReg dstReg val1Reg
        compileGadget $ G.Xor dstReg $ S.fromList [dstReg, val2Reg]

sub :: (Paramable a, Paramable b, Paramable c) => a -> b -> c -> Statement
sub dst val1 val2 = 
    saveAsRegister dst $ \dstReg ->
    asRegister val1 $ \val1Reg ->
    asRegister val2 $ \val2Reg -> do
        compileGadget $ G.LoadReg dstReg val1Reg
        compileGadget $ G.Minus dstReg dstReg val2Reg

jump :: Label -> Partial JumpReason -> Statement
jump offsetLabel partialReason = do
    reason <- partialReason
    case reason of
        Always                  ->
            buildJump (G.Jump X.Always) offsetLabel
        Because relation a b    -> do
            asRegister a $ \aReg ->
                asRegister b $ \bReg ->
                compileGadget $ G.Compare aReg bReg
            buildJump (G.Jump relation) offsetLabel

buildJump :: (Integer -> G.Gadget) -> Label -> Statement
buildJump jumpFlavor target = do
    state@CodeGenState{..}  <- get
    currentOffset           <- currentByteOffset
    maybeTargetOffset       <- lift $ maybeToList $ target `M.lookup` labelMap
    jumpLength              <- lift $ 0:[2..16]
    case maybeTargetOffset of
        Just targetOffset   -> do
            -- We can't jump backward with a 0 length jump...
            guard $ jumpLength > 0
            let randomBytes =  translateGadgetThat
                                    (isRightSize jumpLength)
                                    state
                                    (jumpFlavor (targetOffset - currentOffset - jumpLength))
            (bytes,gen)     <- lift $ sampleStateT randomBytes randomGenerator
            get >>= \state -> put $ state {
                    generatedCode   = generatedCode ++ [Right bytes]
                ,   randomGenerator = gen
            }
        Nothing             -> do
            -- This is probably not ideal as it will cause generate cases for jumps that
            -- do not exist, but it is _MUCH_ cleaner than acquiring a list of all valid
            -- jump sizes.
            let jumpHolder          = JumpHolder {
                jumpFlavor      = jumpFlavor
            ,   jumpPosition    = currentOffset
            ,   jumpLength      = jumpLength
            ,   jumpTarget      = target
            ,   jumpClobberable = locationPool
            }
            put $ state {
                generatedCode = generatedCode ++ [Left jumpHolder]
            }
    where
        isRightSize jumpLength = (==jumpLength) . fromIntegral . B.length


---- | "Returns" from the method. This is done by jumping to the end of a method
---- and moving the given variable to EAX.
ret :: (Paramable p) => p -> Statement
ret value = do
    CodeGenState{..} <- get
    asRegister value $ \valueReg ->
        compileGadget $ G.LoadReg X.EAX valueReg
    reserveRegisterFor X.EAX $
        jump endOfCodeLabel always


always :: Partial JumpReason
always = return Always

buildJumpReason :: (Paramable a, Paramable b) => X.Reason -> a -> b -> Partial JumpReason
buildJumpReason relation a b =
    withParam a $ \aParam ->
    withParam b $ \bParam ->
    return $ Because relation aParam bParam

(->-) :: (Paramable a, Paramable b) => a -> b -> Partial JumpReason
(->-) = buildJumpReason X.Greater

(->=-) :: (Paramable a, Paramable b) => a -> b -> Partial JumpReason
(->=-) = buildJumpReason X.GreaterEqual

(-<-) :: (Paramable a, Paramable b) => a -> b -> Partial JumpReason
(-<-) = buildJumpReason X.Less

(-<=-) :: (Paramable a, Paramable b) => a -> b -> Partial JumpReason
(-<=-) = buildJumpReason X.LessEqual

(-==-) :: (Paramable a, Paramable b) => a -> b -> Partial JumpReason
(-==-) = buildJumpReason X.Equal

(-!=-) :: (Paramable a, Paramable b) => a -> b -> Partial JumpReason
(-!=-) = buildJumpReason X.NotEqual

----------------------------------------------------------------------------------
---- External API
----------------------------------------------------------------------------------

---- | Create a variable for use with the predicate functions.
makeLocal :: Partial Variable
makeLocal = do
    state@CodeGenState{..}      <- get
    -- Find the next available offset from EBP to use as the local stack
    -- variable
    let (readable,writeable)    = M.foldrWithKey' findValidOffsets (S.empty, S.empty) $ D.gadgetMap library
    let validOffsets            = map fromIntegral $ reverse $ S.toAscList $ readable `S.intersection` writeable
    stackOffset                 <- lift $ take 1 $ filter (<=localVariableOffset - 4) validOffsets
    let stackAddress            = X.OffsetAddress X.EBP $ fromIntegral stackOffset
    -- Allocate an integer id for the new variable
    let variableId              = Variable $ (maximum $ 0 : (map unVariable $ M.keys variableMap)) + 1
    --variableLocation            <- lift $ locationPool++[stackVariable]
    put $! state {
            variableMap         = M.insert variableId (Memory stackAddress) variableMap
--        ,   locationPool        = locationPool \\ [X.MemoryLocation stackAddress]
        ,   localVariableOffset = stackOffset
        }
    return variableId
    where
        -- | Places a Memory location offset into one a readable or writeable
        -- list. If folded over a list of gadgets, it will produces a tuple of
        -- two lists that contain all of the offsets that are readable and
        -- writeable respectively.
        findValidOffsets (G.LoadMemReg _ (X.OffsetAddress _ offset))  _ (readable,writeable)  =
            (offset `S.insert` readable,writeable)
        findValidOffsets (G.StoreMemReg (X.OffsetAddress _ offset) _) _ (readable,writeable)  =
            (readable,offset `S.insert` writeable)
        findValidOffsets _                                            _ sets                  = sets

-- | Create n variables for use with the predicate functions.
makeLocals :: Int -> Partial [Variable]
makeLocals n = sequence $ replicate n makeLocal

-- | Create a variable for use with the predicate functions.
makeInput :: Partial Variable
makeInput = do
    state@CodeGenState{..}      <- get
    -- Find the next available offset from EBP to use as the local stack
    -- variable
    let stackOffset             = inputVariableOffset + 4
    let stackVariable           = Memory $ X.OffsetAddress X.EBP $ fromIntegral stackOffset
    -- Allocate an integer id for the new variable
    let variableId              = Variable $ (maximum $ 0 : (map unVariable $ M.keys variableMap)) + 1
    put $! state {
            variableMap         = M.insert variableId stackVariable variableMap
        ,   inputVariableOffset = stackOffset
        }
    return variableId

-- | Create n variables for use with the predicate functions.
makeInputs :: Int -> Partial [Variable]
makeInputs n = sequence $ replicate n makeInput

-- | Create's a 'Label' which can be passed to jump instructions to specify
-- locations in the byte stream to jump to.
makeLabel ::Partial Label
makeLabel = do
    state@CodeGenState{..}  <- get
    let indexLabel          = Label $ 1 + (maximum $ 0 : (map unLabel $ M.keys labelMap))
    currentOffset           <- currentByteOffset
    put $ state {
        labelMap        = M.insert indexLabel Nothing labelMap 
    }
    return indexLabel
    
-- | Create's a list of n 'Label's.
makeLabels :: (Integral i) => i -> Partial [Label]
makeLabels n = sequence $ replicate (fromIntegral n) makeLabel

-- | Mark the current position as the given label.
label :: Label -> Statement
label l = do
    CodeGenState{..}        <-  get
    currentOffset           <-  currentByteOffset
    newCode                 <-  mapM (replaceJumpHolders currentOffset) generatedCode
    state                   <-  get
    put state {
            labelMap        = M.insert l (Just currentOffset) labelMap 
        ,   generatedCode   = newCode
    }
    where
        replaceJumpHolders :: Integer -> Either JumpHolder B.ByteString -> Partial (Either JumpHolder B.ByteString)
        replaceJumpHolders currentOffset v@(Right bytes)    = return v
        replaceJumpHolders currentOffset v@(Left (JumpHolder{..}))
            |   jumpTarget /= l     =   return v
            |   otherwise           =   do
                state@CodeGenState{..}  <-  get
                let jumpOffset          =   currentOffset - jumpPosition - jumpLength
                let jumpGadget          =   jumpFlavor jumpOffset
                let isRightSize         =   (==jumpLength) . fromIntegral . B.length
                (bytes,gen)             <-   lift $ sampleStateT (translateGadgetThat isRightSize state jumpGadget) randomGenerator
                --let bytes               =   B.pack [0xde,0xad,fromIntegral jumpOffset, fromIntegral jumpLength,0xbe,0xef]
                put state {
                    randomGenerator     =   gen
                }
                return $ Right bytes

generate :: D.GadgetLibrary -> StdGen -> Program -> Maybe GeneratedCode
generate library gen program = listToMaybe $ do
    let program'    = do
        eoc <- makeLabel
        program
        label eoc
    (_,solution)        <- runStateT program' $ initialState library gen
    --let eitherCode  = replaceAllJumpHolders solution -- Turn solution into [Either Meta Jump]
    --let eitherCode  = generatedCode solution -- Turn solution into [Either Meta Jump]
    --let byteCode    = B.concat $ rights eitherCode -- Turn the Either values into Meta lists and concats
    ([], byteCode)      <- return $ partitionEithers $ generatedCode solution
    return GeneratedCode {
            byteCode            = B.concat byteCode
        ,   localVariableSize   = 0 - localVariableOffset solution
    }
