{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Igor.CodeGen
( 
-- * Types
  Variable
, CodeGenState
, Partial
, Program
, GeneratedCode(..)
-- * Methods
, generate
--, makeLabel
--, makeLabels
--, label
, makeLocal
, makeLocals
, makeInput
, makeInputs
-- ** Params
, Param (..)
, Paramable
, toParam
, withParam
, withParamAsRegister
, withParamAsSavedRegister
, withSavedRegister
, withRegister
, withTempRegister
, withTempRegister'
-- ** Statements
, move
, add
, sub
, xor
, mul
--, jump
, noop
--, ret
-- *** Jump Reasons
--, (-<-)
--, (-<=-)
--, (->-)
--, (->=-)
--, (-==-)
--, (-!=-)
--, always
) where

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
type LocationPool       = [X.Location]

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

-- | The types of parameters that can be passed to a partial statement
data Param              = Constant  Integer
                        | Location  Label
                        | Register  X.Register
                        | Memory    X.Address

class Paramable a where
    -- | This method takes a 'Paramable' type and a tuple of Partials. The first
    -- modifies the code generation state and returns a Param corresponding to
    -- the value in a. The second handles tearing down the Param in the event
    -- that temporary registers were used.
    toParam :: a -> (Partial Param, Partial ())

-- | The result of compilation.
data GeneratedCode       = GeneratedCode {
        byteCode            :: B.ByteString
    ,   localVariableSize   :: Integer
}

-- | The condition upon which the jump depends
data JumpReason         = Always
                        | Because X.Reason X.Location X.Location

-- | A place holder for a jump. In the event that a predicate attempts to jump
-- ahead to code that has not been generated yet, it will instead drop a
-- JumpHolder which contains enough information so that a second pass through
-- the stream can replace the JumpHolder with an appropriate Gadget realization. 
data JumpHolder = JumpHolder {
        jumpFlavor      :: Integer -> G.Gadget -- ^ The type of jump that we should place here
    ,   jumpIndex       :: Label -- ^ The predicate that contains the jump
    ,   jumpByteOffset  :: Integer -- ^ The byte offset of the jump
    ,   jumpLength      :: Integer -- ^ How big is the jump holder
    ,   jumpTarget      :: Label -- ^ The index of the of the predicate we would like to jump to
    ,   jumpPrefix      :: B.ByteString
}

data CodeGenState       = CodeGenState {
        library             :: D.GadgetLibrary
    ,   randomGenerator     :: StdGen
    ,   variableMap         :: VariableMap
    ,   labelMap            :: M.Map Label Integer 
    ,   locationPool        :: LocationPool
    ,   generatedCode       :: [Either JumpHolder B.ByteString]
    ,   localVariableOffset :: Integer -- ^ How far from EBP should we assign the next local variable?
    ,   endOfCodeLabel      :: Label -- ^ Marks the end of body. Used for jumping out to return.
    ,   inputVariableOffset :: Integer -- ^ How much room on the stack is reserved for the input variables
    }

initialState :: D.GadgetLibrary -> StdGen -> CodeGenState
initialState library gen = CodeGenState {
        library             = library
    ,   randomGenerator     = gen'
    ,   variableMap         = M.empty
    ,   labelMap            = M.empty
    ,   locationPool        = shuffledPool
    ,   generatedCode       = []
    ,   localVariableOffset = 0
     -- | NOTE: This is dependent on the number of registers saved before moving
     -- ebp to esp, which is dependent on the backend scaffolding ... ugh....
    ,   inputVariableOffset = 16
    ,   endOfCodeLabel      = Label 1 -- ^ The end of code label will always be the first label made.
    }
    where
        pool                    = map X.RegisterLocation X.generalRegisters
        (shuffledPool, gen')    = sampleState (shuffle pool) gen
        

--------------------------------------------------------------------------------
-- Param Handling
--------------------------------------------------------------------------------

instance Paramable Integer where
    toParam a = (return $ Constant a, return ())

instance Paramable Variable where
    toParam var = 
        (
            do
                CodeGenState {..}  <- get
                lift $ maybeToList $ M.lookup var variableMap
        , 
            return ()
        )

instance Paramable Label where
    toParam label = (return $ Location label, return ())

instance Paramable X.Location where
    toParam (X.MemoryLocation addr)   = (return $ Memory addr, return ())
    toParam (X.RegisterLocation reg)  = (return $ Register reg, return ())

-- TODO: Write instances of Paramable for memory locations

-- | Automaticall handles initializing and cleaning up the predicate. It is
-- recommended to use withParam over toParam as setup and tear down are
-- guaranteed.
withParam :: Paramable a => a -> (Param -> Partial b) -> Partial b
withParam paramable method = do
    let (initialize,finish) = toParam paramable
    param                   <- initialize
    result                  <- method param
    finish
    return result

-- | Allocates and a temporary register for use with a method and then
-- subsequently frees it once the method is finished. The first parameter is a
-- list of nonFreeRegs that also qualify as temporary registers in this
-- particular instance (e.g. if they will be overwritten after whatever
-- operation is complete)
withTempRegister :: [X.Register] -> (X.Register -> Partial b) -> Partial b
withTempRegister nonFreeRegs method = do
    state@CodeGenState{..}  <- get
    tempReg                 <- lift $ nonFreeRegs ++ map (\(X.RegisterLocation r) -> r) locationPool
    put                     $! state { locationPool = locationPool \\ [X.RegisterLocation tempReg] }
    result                  <- method tempReg
    -- Restore temp to the location pool if it was originally free
    if (X.RegisterLocation tempReg) `elem` locationPool
        then do
            state@CodeGenState{..}  <- get
            put $ state {
                locationPool = (X.RegisterLocation tempReg):locationPool
            }
        else return ()
    return result

-- | Same as 'withTempRegister' except that the first argument has been
-- pre-applied as []
withTempRegister' :: (X.Register -> Partial b) -> Partial b
withTempRegister' = withTempRegister []

saveParamAsRegister :: Paramable p => p -> (X.Register -> Partial b) -> Partial b
saveParamAsRegister paramable method =
    withParam paramable $ flip saveAsRegister method

-- | Create's a temporary register passes it to method and then once method is
-- complete, stores the value in param.
saveAsRegister :: Param -> (X.Register -> Partial b) -> Partial b
saveAsRegister param method =
    withTempRegister' (savingMethod param)
    where
        savingMethod (Register dstReg)  reg = do
            result <- method reg
            translateGadget $ G.LoadReg dstReg reg
            return result
        savingMethod (Memory dstAddr)   reg = do
            result <- method reg
            withTempRegister [reg] $ \tmpReg -> do
                translateGadget $ G.LoadReg tmpReg reg
                translateGadget $ G.StoreMemReg dstAddr tmpReg
                return result
        savingMethod _                  reg = fail "Attempting to save to a non-location."

-- | Allows you to pull a Paramable value directly into a register. Basically
-- syntactic sugar for withParam p $ \p' -> withRegister p' $ method.
withParamAsRegister :: Paramable p => p -> (X.Register -> Partial b) -> Partial b
withParamAsRegister paramable method =
    withParam paramable $ flip withRegister method

-- | Allows you to pull a Paramable value directly into a register and then save
-- it. Basically syntactic sugar for withParam p $ \p' -> withSavedRegister p' $
-- method.
withParamAsSavedRegister :: Paramable p => p -> (X.Register -> Partial b) -> Partial b
withParamAsSavedRegister paramable method =
    withParam paramable $ flip withSavedRegister method

-- | Same as 'withRegister' except that the value of register at the end of the
-- method call is moved to the Param's original location.
withSavedRegister :: Param -> (X.Register -> Partial b) -> Partial b
withSavedRegister param method = 
    withRegister param (savingMethod param)
    where
        savingMethod (Register dstReg)  reg = do
            result <- method reg
            translateGadget $ G.LoadReg dstReg reg
            return result
        savingMethod (Memory dstAddr)   reg = do
            result <- method reg
            withTempRegister [reg] $ \tmpReg -> do
                translateGadget $ G.LoadReg tmpReg reg
                translateGadget $ G.StoreMemReg dstAddr tmpReg
                return result
        savingMethod _                  reg = fail "Attempting to save to a non-location."

-- | Moves the value described by Param into a temporary register that will be
-- freed at the end of the method. Note that this method will fail if used on
-- 'Location's this is because it is not possible to determine their values at
-- compile time.
withRegister :: Param -> (X.Register -> Partial b) -> Partial b
withRegister (Location _)       method = fail "Cannot turn a location into a register."
withRegister (Register reg)     method =
    let
        -- | Allow reg to be used as a scratch register if it is a general
        -- purpose register, otherwise we have to allocate a new one.
        possiblyReg = if reg `elem` X.generalRegisters
                        then [reg]
                        else []
    in
        withTempRegister possiblyReg $ \tempReg -> do
            translateGadget         $ G.LoadReg tempReg reg
            method tempReg

withRegister (Memory address)   method = 
    withTempRegister' $ \tempReg -> do
        possibleTranslations tempReg address
        method tempReg
    where
    -- Tries every possible way of accessing a given memory address
        possibleTranslations tempReg addr@(X.OffsetAddress baseReg offset) = 
            withTempRegister [tempReg] $ \tempDstReg ->
            withTempRegister [baseReg] $ \tempBaseReg -> do
                    translateGadget $ G.LoadReg tempBaseReg baseReg
                    translateGadget $ G.LoadMemReg tempDstReg (X.OffsetAddress tempBaseReg offset)
                    translateGadget $ G.LoadReg tempReg tempDstReg
        possibleTranslations tempReg addr@(X.IndexedAddress baseReg indexReg scale offset) = 
            withTempRegister [tempReg] $ \tempDstReg ->
            withTempRegister [baseReg] $ \tempBaseReg -> 
            withTempRegister [indexReg] $ \tempIndexReg -> do
                    translateGadget $ G.LoadReg tempBaseReg baseReg
                    translateGadget $ G.LoadReg tempIndexReg indexReg
                    translateGadget $ G.LoadMemReg tempDstReg (X.IndexedAddress tempBaseReg tempIndexReg scale offset)
                    translateGadget $ G.LoadReg tempReg tempDstReg

withRegister (Constant value)   method = 
    withTempRegister' $ \valueReg -> do
        withTempRegister' $ \savedConstant ->
            withTempRegister' $ \savedShift -> do
                CodeGenState{..}                <- get
                let poolSet                     = S.fromList locationPool
                let gmap                        = D.gadgetMap library
                (constantGadget, constantLoc)   <- lift $ mapMaybe (getConstant poolSet) $ M.keys gmap
                (shiftGadget, shiftLoc)         <- lift $ mapMaybe (getShift poolSet) $ M.keys gmap
                withTempRegister [shiftLoc] $ \tempShiftReg -> do
                    -- Save the constant and shift registers in case they are special
                    translateGadget $ G.LoadReg savedConstant constantLoc
                    translateGadget $ G.LoadReg savedShift shiftLoc
                    -- Actually perform the constant load and the shift
                    translateGadget $ constantGadget
                    translateGadget $ G.LoadReg tempShiftReg constantLoc
                    translateGadget $ shiftGadget
                    -- Move the constant into a temporary register
                    translateGadget $ G.LoadReg valueReg shiftLoc
                    -- Restore the constant and shift registers
                    translateGadget $ G.LoadReg constantLoc savedConstant
                    translateGadget $ G.LoadReg shiftLoc savedShift
        -- Release the other temp registers and perform the desired method on
        -- the valueReg which now holds the desired constant!
        method valueReg

    where
        numBits             = ceiling $ logBase 2 (fromIntegral value+1)
        shiftSize           = 31 - numBits

        getConstant :: S.Set X.Location -> G.Gadget -> Maybe (G.Gadget, X.Register)
        getConstant poolSet g@(G.LoadConst reg val)
            -- | not $ (X.RegisterLocation reg) `S.member` poolSet = Nothing
            | shiftR val shiftSize == fromIntegral value        = Just (g, reg)
            | otherwise                                         = Nothing
        getConstant _ _                                         = Nothing

        getShift :: S.Set X.Location -> G.Gadget -> Maybe (G.Gadget, X.Register)
        getShift poolSet g@(G.RightShift reg val)
            -- | not $ (X.RegisterLocation reg) `S.member` poolSet = Nothing
            | shiftSize == fromIntegral val                     = Just (g, reg)
            | otherwise                                         = Nothing
        getShift _ _                                            = Nothing
        --

-- | Compiles a gadget to bytecode and places it in the sequence of generated
-- code.
translateGadget :: G.Gadget -> Partial ()
translateGadget gadget = do
    state@(CodeGenState{..})    <- get
    !gadgetSet                   <- lift $ maybeToList $ D.libraryLookup gadget library
    let (shuffled,gen)          = sampleState (shuffle $! S.toList gadgetSet) randomGenerator
    (bytes,_)                   <- lift $ filter (doesNotClobber locationPool gadget) $ shuffled
    let newCode                 = generatedCode ++ [Right bytes]
    -- put the generator in the state so that the jump replacement code
    -- and future gadgets can be random as well
    put $! state {
            randomGenerator = gen
        ,   generatedCode   = newCode
        }
    return ()

-- | Ensures that a gadget only clobbers the unallocated locations described in
-- the locationPool.
doesNotClobber locationPool _            (_,[])         = True
doesNotClobber locationPool (G.Jump _ _) (_,clobber)    = doesNotClobber' (locationPool) clobber
doesNotClobber locationPool _            (_,clobber)    = doesNotClobber' ((X.RegisterLocation X.EFLAG):locationPool) clobber

doesNotClobber' locationPool clobber =
    let
        clobberSet              = S.fromList clobber
        clobberAbleLocations    = S.fromList $ (X.RegisterLocation X.EFLAG):locationPool
    in
        clobberSet `S.union` clobberAbleLocations == clobberAbleLocations

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

noop :: Statement
noop = translateGadget G.NoOp

move :: (Paramable a, Paramable b) => a -> b -> Statement
move dst src = withParam dst moveToDst
    where
        moveToDst (Register dstReg) = 
            withParamAsRegister src $ \srcReg ->
            translateGadget $ G.LoadReg dstReg srcReg
        moveToDst (Memory dstAddr)  = 
            withParamAsRegister src $ \srcReg ->
            translateGadget $ G.StoreMemReg dstAddr srcReg
        moveToDst _                 = fail "Attempting to move to a non-location."

add :: (Paramable a, Paramable b, Paramable c) => a -> b -> c -> Statement
add dst val1 val2 = 
    saveParamAsRegister dst $ \dstReg ->
    withParamAsRegister val1 $ \val1Reg ->
    withParamAsRegister val2 $ \val2Reg -> do
        translateGadget $ G.LoadReg dstReg val1Reg
        translateGadget $ G.Plus dstReg $ S.fromList [dstReg, val2Reg]

mul :: (Paramable a, Paramable b, Paramable c) => a -> b -> c -> Statement
mul dst val1 val2 = 
    saveParamAsRegister dst $ \dstReg ->
    withParamAsRegister val1 $ \val1Reg ->
    withParamAsRegister val2 $ \val2Reg -> do
        translateGadget $ G.LoadReg dstReg val1Reg
        translateGadget $ G.Times dstReg $ S.fromList [dstReg, val2Reg]

xor :: (Paramable a, Paramable b, Paramable c) => a -> b -> c -> Statement
xor dst val1 val2 = 
    saveParamAsRegister dst $ \dstReg ->
    withParamAsRegister val1 $ \val1Reg ->
    withParamAsRegister val2 $ \val2Reg -> do
        translateGadget $ G.LoadReg dstReg val1Reg
        translateGadget $ G.Xor dstReg $ S.fromList [dstReg, val2Reg]

sub :: (Paramable a, Paramable b, Paramable c) => a -> b -> c -> Statement
sub dst val1 val2 = 
    saveParamAsRegister dst $ \dstReg ->
    withParamAsRegister val1 $ \val1Reg ->
    withParamAsRegister val2 $ \val2Reg -> do
        translateGadget $ G.LoadReg dstReg val1Reg
        translateGadget $ G.Minus dstReg dstReg val2Reg

---- | "Returns" from the method. This is done by jumping to the end of a method
---- and moving the given variable to EAX.
--ret :: Variable -> Predicate ()
--ret a = do
--    CodeGenState{..}    <- get
--    aLoc                <- varToLoc' a
--    let moveAToEAX = moveHelper locationPool (X.RegisterLocation X.EAX) aLoc
--    calculateJump moveAToEAX (G.Jump X.Always) endOfCodeLabel
--
--jump :: Label -> Predicate JumpReason -> Predicate ()
--jump indexLabel reason = do
--    reason' <- reason
--    case reason' of
--        Always                  -> calculateJump (inits $ repeat G.NoOp) (G.Jump X.Always) indexLabel
--        (Because reason a b)    -> do
--            CodeGenState{..} <- get
--            -- A list of list of gadgets that will compare the required
--            -- locations
--            let compare' = [ 
--                        moveA ++  moveB ++  [G.Compare aReg bReg]
--                        --noops ++ moveA ++  moveB ++  [G.Compare aReg bReg]
--                    | 
--                        -- Include noops so to vary the length of the jump
--                        -- instruction
--                        --noops                           <- (inits $ repeat G.NoOp)
--                        aLoc@(X.RegisterLocation aReg)  <- a:locationPool
--                    ,   bLoc@(X.RegisterLocation bReg)  <- (b:locationPool) \\ [aLoc]
--                    ,   moveA <- moveHelper locationPool aLoc a -- Move b into x
--                    ,   moveB <- moveHelper locationPool bLoc b -- Move c into y
--                    ]
--            calculateJump compare' (G.Jump reason) indexLabel
--
--always :: Predicate JumpReason
--always = return Always
--
--(->-) :: Variable -> Variable -> Predicate JumpReason
--a ->- b = do
--    a' <- varToLoc' a
--    b' <- varToLoc' b
--    return $ Because X.Greater a' b'
--
--(->=-) :: Variable -> Variable -> Predicate JumpReason
--a ->=- b = do
--    a' <- varToLoc' a
--    b' <- varToLoc' b
--    return $ Because X.GreaterEqual a' b'
--
--(-<-) :: Variable -> Variable -> Predicate JumpReason
--a -<- b = do
--    a' <- varToLoc' a
--    b' <- varToLoc' b
--    return $ Because X.Less a' b'
--
--(-<=-) :: Variable -> Variable -> Predicate JumpReason
--a -<=- b = do
--    a' <- varToLoc' a
--    b' <- varToLoc' b
--    return $ Because X.LessEqual a' b'
--
--(-==-) :: Variable -> Variable -> Predicate JumpReason
--a -==- b = do
--    a' <- varToLoc' a
--    b' <- varToLoc' b
--    return $ Because X.Equal a' b'
--
--(-!=-) :: Variable -> Variable -> Predicate JumpReason
--a -!=- b = do
--    a' <- varToLoc' a
--    b' <- varToLoc' b
--    return $ Because X.NotEqual a' b'
--
----------------------------------------------------------------------------------
---- Predicate Helper Functions
----------------------------------------------------------------------------------
--
---- | Common jump calculation code. It takes a jump constructor a predicate
---- offset and returns a predicate that fulfills the jump. Backward jumps are
---- generated by the 'makePredicate' function immediately, while forward jumps are
---- translated to 'JumpHolder's of each possible size which later become fulfilled
---- by subsequent calls to 'makePredicate'.
--calculateJump :: [[G.Gadget]] -> (Integer -> G.Gadget) -> Label -> Predicate ()
--calculateJump allGadgetStreams jumpFlavor target = do
--        state@CodeGenState{..}  <- get
--        gadgetStream            <- lift allGadgetStreams -- grab a sequence of gadgets that do what we want
--        byteStream              <- liftM B.concat $ mapM translateGadget gadgetStream -- translate all of the gadgets to instructions
--        let jumpGadgets         = filter isJump $ M.keys $ D.gadgetMap library :: [G.Gadget]
--        -- | This is pretty ugly and there is probably a better way to do
--        -- this, but this will find all of the possible lengths that a jump
--        -- instruction can be.
--        currentOffset           <- lift $ maybeToList $ M.lookup (currentPredicate) predicateToByte
--        let prefixLength        = fromIntegral $ B.length byteStream
--        jumpLength              <- lift  
--                                    . S.toList 
--                                    . S.fromList
--                                    . map (fromIntegral . B.length . fst) 
--                                    . concatMap S.elems
--                                    =<< (   lift
--                                        $   maybeToList
--                                        $   mapM (flip D.libraryLookup library) jumpGadgets
--                                        )
--        let jumpHolder          = JumpHolder {
--                jumpFlavor      = jumpFlavor
--            ,   jumpByteOffset  = currentOffset
--            ,   jumpIndex       = currentPredicate
--            ,   jumpLength      = jumpLength
--            ,   jumpTarget      = target
--            ,   jumpPrefix      = byteStream
--            }
--        put $! state{
--                generatedCode       = generatedCode ++ [Left jumpHolder]
--            ,   predicateToByte     = M.insert (currentPredicate+1) (currentOffset+jumpLength+prefixLength) predicateToByte
--            ,   currentPredicate    = currentPredicate + 1 
--            ,   locationPool        = locationPool -- We called translateGadget which modifies the pool, so we must restore it.
--            }
--    where
--        isJump :: G.Gadget -> Bool
--        isJump (G.Jump _ _) = True
--        isJump _            = False
--
---- | A helper function that will generate a sequence of gadgets that will move
---- one location to another regardless of the type of location.
--moveHelper  :: LocationPool -- ^ Locations that we are allowed to use as scratch
--            -> X.Location   -- ^ The destination location
--            -> X.Location   -- ^ The source location
--            -> [[G.Gadget]]   -- ^ All the possible sequences of gadgets that will fulfill the move
--moveHelper pool (X.RegisterLocation a) (X.RegisterLocation b)       
--    | a == b    = [[]]
--    | otherwise = [[G.LoadReg a b]]
--
--moveHelper pool (X.MemoryLocation a offset) bLoc@(X.RegisterLocation b)  =
--        [G.StoreMemReg a offset b]
--    :   [
--            [
--                G.LoadReg taReg a
--            ,   G.LoadReg tbReg b
--            ,   G.StoreMemReg taReg offset tbReg
--            ]
--        |
--                taLoc@(X.RegisterLocation taReg) <- (X.RegisterLocation a):pool
--            ,   tbLoc@(X.RegisterLocation tbReg) <- ((bLoc:pool) \\ [taLoc])
--        ]
--
--moveHelper pool aLoc@(X.RegisterLocation a) (X.MemoryLocation b offset)  =
--        [G.LoadMemReg a b offset]
--    :   [
--            [
--                G.LoadReg tb b
--            ,   G.LoadMemReg ta tb offset
--            ,   G.LoadReg a ta
--            ]
--        |
--                (X.RegisterLocation ta) <- aLoc:pool
--            ,   (X.RegisterLocation tb) <- pool
--        ]
--
--moveHelper pool a@(X.MemoryLocation aReg aOffset) b@(X.MemoryLocation bReg bOffset) 
--    | a == b    = [[]]
--    | otherwise = 
--        [
--            [
--                G.LoadReg taReg aReg
--            ,   G.LoadMemReg tReg taReg aOffset
--            ,   G.LoadReg tbReg bReg
--            ,   G.StoreMemReg tbReg bOffset tReg
--            ] 
--        | 
--                taLoc@(X.RegisterLocation taReg) <- (X.RegisterLocation aReg):pool
--            ,   tbLoc@(X.RegisterLocation tbReg) <- (X.RegisterLocation bReg):pool
--            ,   tLoc@(X.RegisterLocation tReg)   <- (pool \\ [tbLoc])
--        ]
--
----moveHelper _ a b
----    | a == b    = [[]]
----    | otherwise = []
--
----------------------------------------------------------------------------------
---- External API
----------------------------------------------------------------------------------
--
--generate :: D.GadgetLibrary -> StdGen -> PredicateProgram -> Maybe GeneratedCode
--generate library gen program = listToMaybe $ do
--    let program'    = do
--        eoc <- makeLabel
--        program
--        label eoc
--    (_,solution)    <- runStateT program' $ initialState library gen
--    let eitherCode  = replaceAllJumpHolders solution -- Turn solution into [Either Meta Jump]
--    let byteCode    = B.concat $ rights eitherCode -- Turn the Either values into Meta lists and concats
--    -- Remove solutions with hanging jumps
--    guard $ (null . lefts) eitherCode
--    return GeneratedCode {
--            byteCode            = byteCode
--        ,   localVariableSize   = 0 - localVariableOffset solution
--    }
--    where
--        -- | Attempts to replace forward jump statements for a given element in
--        -- generatedCode. Because a jumpHolder may be replaced by several
--        -- instructions it is necessary to return a list of lists and then
--        -- concat the results.
--        replaceAllJumpHolders :: CodeGenState -> [Either JumpHolder B.ByteString]
--        replaceAllJumpHolders state@CodeGenState{..} = 
--            let
--                jumpReplacers   = map ((uncurry $ replaceJumpHolders state)) $ M.assocs predicateToByte
--                replacedCode    = foldrM mapM generatedCode jumpReplacers
--            in
--                fst $ sampleState (replacedCode :: RVar [Either JumpHolder B.ByteString]) randomGenerator
--
--        replaceJumpHolders :: CodeGenState -> Integer -> Integer -> Either JumpHolder B.ByteString -> RVar (Either JumpHolder B.ByteString)
--        replaceJumpHolders state@CodeGenState{..} predIndex byteOffset j@(Left (JumpHolder {..}))
--            | predIndex == labelToIndex state jumpTarget   = do
----                let jumpGarbageSize         = if (predIndex-jumpIndex) <= 0
----                                                then jumpLength + (fromIntegral $ B.length jumpPrefix)
----                                                else jumpLength
--                let jumpGarbageSize         = jumpLength + (fromIntegral $ B.length jumpPrefix)
--                let jumpGadget              = (jumpFlavor (byteOffset - jumpByteOffset - jumpGarbageSize))
--                let gadgets                 = filter ((jumpLength==) . fromIntegral . B.length . fst)
--                                                $ concatMap S.toList
--                                                $ maybeToList
--                                                $ D.libraryLookup jumpGadget library
--                shuffled                    <- shuffle gadgets
--                case listToMaybe $ filter (doesNotClobber locationPool jumpGadget) $ shuffled of --shuffled
--                    Nothing         -> return j
--                    Just (meta,_)   -> return $ Right $ jumpPrefix `B.append` meta
--            | otherwise             = return j
--        replaceJumpHolders _ _ _ meta = return meta
--
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
        ,   locationPool        = locationPool \\ [X.MemoryLocation stackAddress]
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

---- | Creates a 'label' which is just an integer which maps to a position in the
---- predicate stream.
--makeLabel ::Partial Label
--makeLabel = do
--    state@CodeGenState{..}  <- get
--    let indexLabel          = Label $ 1 + (maximum $ 0 : (map unLabel $ M.keys labelMap))
--    put $ state {
--        labelMap        = M.insert indexLabel currentPredicate labelMap 
--    }
--    return indexLabel
--    
--makeLabels :: (Integral i) => i -> Partial [Label]
--makeLabels n = sequence $ replicate (fromIntegral n) makeLabel

---- | Mark the current position as the given label.
--label :: Label -> Predicate ()
--label l = do
--    state@CodeGenState{..}    <- get
--    put $ state {
--        labelMap        = M.insert l currentPredicate labelMap 
--    }
--    return ()
--

generate :: D.GadgetLibrary -> StdGen -> Program -> Maybe GeneratedCode
generate library gen program = listToMaybe $ do
    let program'    = do
        --eoc <- makeLabel
        program
        --label eoc
    (_,solution)    <- runStateT program' $ initialState library gen
    --let eitherCode  = replaceAllJumpHolders solution -- Turn solution into [Either Meta Jump]
    let eitherCode  = generatedCode solution -- Turn solution into [Either Meta Jump]
    let byteCode    = B.concat $ rights eitherCode -- Turn the Either values into Meta lists and concats
    -- Remove solutions with hanging jumps
    guard $ (null . lefts) eitherCode
    return GeneratedCode {
            byteCode            = byteCode
        ,   localVariableSize   = 0 - localVariableOffset solution
    }

----------------------------------------------------------------------------------
---- Predicate to Gadget translation
----------------------------------------------------------------------------------
--
--makePredicate1 :: (X.Location -> [[G.Gadget]]) -> Variable -> Predicate ()
--makePredicate1 generator a = do
--    state <- get
--    makePredicate (generator $ varToLoc state a)
--
--makePredicate2 :: (X.Location -> X.Location -> [[G.Gadget]]) -> Variable -> Variable -> Predicate ()
--makePredicate2 generator a b = do
--    state <- get
--    makePredicate ((generator `on` varToLoc state) a b)
--
--makePredicate3 :: (X.Location -> X.Location -> X.Location -> [[G.Gadget]]) -> Variable -> Variable -> Variable -> Predicate ()
--makePredicate3 generator a b c = do
--    state <- get
--    makePredicate ((generator `on` varToLoc state) a b $ varToLoc state c)
--
---- | A helper function that takes a list of lists of gadgets that fulfill a
---- given computation. It attempts to generate an instruction sequence for each
---- gadget in each possible sequence of gadgets. It also fills in any Jump
---- placeholders that it is able to.
--makePredicate :: [[G.Gadget]] -> Predicate ()
--makePredicate allGadgetStreams = do
--    state@(CodeGenState{..})    <- get
--    gadgetStream                <- lift allGadgetStreams -- grab a sequence of gadgets that do what we want
--    --noops                       <- lift (inits $ repeat G.NoOp)
--    --metaStream                  <- mapM translateGadget (gadgetStream++noops) -- translate all of the gadgets to instructions
--    metaStream                  <- mapM translateGadget (gadgetStream) -- translate all of the gadgets to instructions
--    byteIndex                   <- lift $ maybeToList $ M.lookup currentPredicate predicateToByte
--    let nextByteIndex           = byteIndex + (foldr' (+) 0 $ map (fromIntegral . B.length) metaStream)
--    let newCode                 = generatedCode ++ [(Right $ B.concat metaStream)]
--    put $! state{
--            generatedCode       = newCode
--        ,   predicateToByte     = M.insert (currentPredicate+1) nextByteIndex predicateToByte
--        ,   currentPredicate    = currentPredicate + 1 
--        ,   locationPool        = locationPool -- Restore the pool that may have been altered by gadgets
--        }
--
---- | Turns a gadget into a sequence of assembly instructions. It also
---- removes from the location pool any location that is defined by the
---- gadget. This prevents a sequence of gadgets from clobbering
---- intermediate values.
--translateGadget :: G.Gadget -> Predicate B.ByteString
--translateGadget gadget = do
--    state@(CodeGenState{..})    <- get
--    gadgetSet                   <- lift $ maybeToList $ D.libraryLookup gadget library
--    let (shuffled,gen)          = sampleState (shuffle $! S.toList gadgetSet) randomGenerator
--    let newLocationPool         = locationPool \\ (G.defines gadget)
--    (bytes,clobbering)          <- lift $ filter (doesNotClobber newLocationPool gadget) $ shuffled
--    -- put the generator in the state so that the jump replacement code
--    -- and future gadgets can be random as well
--    put $! state {
--            randomGenerator = gen
--            -- Remove any intermediate values from the location pool
--        ,   locationPool    = newLocationPool
--        }
--    return $! bytes
--
--   
---- | Ensures that a gadget realization does not clobber locations that
---- should not be clobbered.
--doesNotClobber locationPool _            (_,[])         = True
--doesNotClobber locationPool (G.Jump _ _) (_,clobber)    = doesNotClobber' (locationPool) clobber
--doesNotClobber locationPool _            (_,clobber)    = doesNotClobber' ((X.RegisterLocation X.EFLAG):locationPool) clobber
--
--doesNotClobber' locationPool clobber =
--    let
--        clobberSet              = S.fromList clobber
--        clobberAbleLocations    = S.fromList $ (X.RegisterLocation X.EFLAG):locationPool
--    in
--        clobberSet `S.union` clobberAbleLocations == clobberAbleLocations
--
---- | Translates a variable into an expression location.
--varToLoc :: CodeGenState -> Variable -> X.Location
--varToLoc (CodeGenState {..}) var = fromJust $ M.lookup var variableMap
--
--varToLoc' :: Variable -> Predicate X.Location
--varToLoc' var = do
--    (CodeGenState {..})  <- get
--    return $ fromJust $ M.lookup var variableMap
--
--labelToIndex :: CodeGenState -> Label -> Integer
--labelToIndex state label = fromJust $ M.lookup label $ labelMap state
--
----locToVar :: CodeGenState -> X.Location -> Maybe Variable
----locToVar (_,vars,_) targetLoc = M.foldWithKey findLoc Nothing vars 
----    where
----        findLoc var loc res = if loc == targetLoc   then Just var
----                                                    else res
--
