{-# LANGUAGE RecordWildCards #-}
module Igor.CodeGen
( 
-- * Types
  Variable
, CodeGenState
, Predicate
, PredicateProgram
-- * Methods
, generate
, makeLabel
, makeLabels
, label
, makeVariable
, makeVariables
-- ** Predicates
, move
, add
, sub
, jump
, noop
, set
-- *** Jump Reasons
, (-<-)
, (-<=-)
, (->-)
, (->=-)
, (-==-)
, (-!=-)
, always
) where

import              Control.Monad.State
import              Data.Bits
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
import              Hdis86
import              System.Random

-- TODO:
--
-- 1) Constants
--      - Currently there is like a 0% likely hood that a needed constant will
--      be in the library. Therefore the set constant predicate (which also
--      needs to be made) needs to be able to generate predicates from what is
--      in the library. The most straight forward way I can think of doing this
--      is choosing a sufficiently small right shift and then combing the
--      available constants in the library for an appropriate one and then
--      combining the loadConst and RightShift gadgets.
--
--      - This will also likely cause clobber conflicts with variables in the
--      registers so will likely depend on moving variables to memory.

-- | Variables are the type passed to the user of the library and currently the
-- arguments to the predicate functions. This may change in the future for when
-- constants are added.
type Variable           = Integer

-- | Labels correspond to the 0 indexed position in the predicate sequence
type Label              = Integer

-- | A location pool is the known locations that are free to use as scratch
-- space or new variables.
type LocationPool       = [X.Location]

-- | A mapping from Variables to locations.
type VariableMap        = M.Map Variable X.Location

-- | At the moment 'PredicateProgram's and 'Predicate's are the same thing, this
-- may not always be the case so there are two different types exported so any
-- internal changes should not cause problems in user code.
type PredicateProgram   = StateT CodeGenState [] ()
type Predicate a        = StateT CodeGenState [] a

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
    ,   currentPredicate    :: Integer
    ,   predicateToByte     :: M.Map Integer Integer -- ^ Maps from predicate indices to byte indices
    ,   localVariableOffset :: Integer -- ^ How far from EBP should we assign the next local variable?
    }

initialState :: D.GadgetLibrary -> StdGen -> CodeGenState
initialState library gen = CodeGenState {
        library             = library
    ,   randomGenerator     = gen'
    ,   variableMap         = M.empty
    ,   labelMap            = M.empty
    ,   locationPool        = shuffledPool
    ,   generatedCode       = []
    ,   currentPredicate    = 0
    ,   predicateToByte     = M.insert 0 0 M.empty
    ,   localVariableOffset = 0
    }
    where
        pool                    = map X.RegisterLocation X.generalRegisters
        (shuffledPool, gen')    = sampleState (shuffle pool) gen
        

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

noop :: Predicate ()
noop = makePredicate [[G.NoOp]]

move :: Variable -> Variable -> Predicate ()
move a b = do
    CodeGenState{..} <- get
    makePredicate2 (moveHelper locationPool) a b

-- | Loads a constant into a variable
set :: Variable -> Integer -> Predicate ()
set var value = do
    CodeGenState{..}            <- get
    let poolSet                 = S.fromList locationPool
    let gmap                    = D.gadgetMap library
    let constantGadgets         = mapMaybe (getConstant poolSet) $ M.keys gmap
    let shiftGadgets            = mapMaybe (getShift poolSet) $ M.keys gmap
    makePredicate1 (set' locationPool constantGadgets shiftGadgets) var

    where
        set' pool constants shifts var = 
            [ 
                    constantGadget
                :   moveShift
                ++  shiftGadget
                :   moveVar
            | 
                    (constantGadget,cLoc)   <- constants
                ,   (shiftGadget,sLoc)      <- shifts
                ,   moveShift               <- moveHelper pool sLoc cLoc 
                ,   moveVar                 <- moveHelper pool var sLoc 
            ]

        numBits             = ceiling $ logBase 2 (fromIntegral value+1)
        shiftSize           = 31 - numBits

        getConstant :: S.Set X.Location -> G.Gadget -> Maybe (G.Gadget, X.Location)
        getConstant poolSet g@(G.LoadConst reg val)
            | not $ (X.RegisterLocation reg) `S.member` poolSet = Nothing
            | shiftR val shiftSize == fromIntegral value        = Just (g, X.RegisterLocation reg)
            | otherwise                                         = Nothing
        getConstant _ _                                         = Nothing

        getShift :: S.Set X.Location -> G.Gadget -> Maybe (G.Gadget, X.Location)
        getShift poolSet g@(G.RightShift reg val)
            | not $ (X.RegisterLocation reg) `S.member` poolSet = Nothing
            | shiftSize == fromIntegral val                     = Just (g, X.RegisterLocation reg)
            | otherwise                                         = Nothing
        getShift _ _                                            = Nothing


sub :: Variable -> Variable -> Variable -> Predicate ()
sub a b c = do
    CodeGenState{..} <- get
    makePredicate3 (add' locationPool) a b c
    where
        add' pool a b c = 
            [ 
                    moveB -- Move u to a temporary register
                ++  moveC -- Move v to a temporary register
                ++  [G.Minus bReg bReg cReg] -- We are more likely to find an minus involving 2 registers
                ++  moveA -- Move the result into a
            | 
                    bLoc@(X.RegisterLocation bReg) <- a:b:pool
                ,   cLoc@(X.RegisterLocation cReg) <- (c:pool) \\ [bLoc]
                ,   moveB <- moveHelper pool bLoc b -- Move b into x
                ,   moveC <- moveHelper pool cLoc c -- Move c into y
                ,   moveA <- moveHelper pool a bLoc -- Move the result into a
            ]

add :: Variable -> Variable -> Variable -> Predicate ()
add a b c = do
    CodeGenState{..} <- get
    makePredicate3 (add' locationPool) a b c
    where
        add' pool a b c = 
            [ 
                    moveX -- Move u to a temporary register
                ++  moveY -- Move v to a temporary register
                ++  [G.Plus xReg $ S.fromList [xReg,yReg]] -- We are more likely to find an add involving 2 registers
                ++  moveA -- Move the result into a
            | 
                    -- Try swapping b and c also. This is useful if one of the
                    -- locations is a register and the other is not, it allows
                    -- the search to find solutions that move the non-register
                    -- location into the destination (if it is a register) and
                    -- keep the register location in the same. Basically it
                    -- makes the below search symmetric.
                    (x,y) <- [(b,c), (c,b)]
                    -- Because a is going to be overwritten with the result, we
                    -- can consider it in the location pool and use it as
                    -- scratch space for the computation. 
                ,   xLoc@(X.RegisterLocation xReg) <- a:x:pool
                    -- Because we aren't possibly overwriting v we can use v if
                    -- it is a register, otherwise choose a temp register for it
                ,   yLoc@(X.RegisterLocation yReg) <- (y:pool) \\ [xLoc]
                    -- Shuffle locations around into temporary registers
                ,   moveX <- moveHelper pool xLoc x -- Move b into x
                ,   moveY <- moveHelper pool yLoc y -- Move c into y
                ,   moveA <- moveHelper pool a xLoc -- Move the result into a
            ]

jump :: Label -> Predicate JumpReason -> Predicate ()
jump indexLabel reason = do
    reason' <- reason
    case reason' of
        Always                  -> calculateJump [[]] (G.Jump X.Always) indexLabel
        (Because reason a b)    -> do
            CodeGenState{..} <- get
            -- A list of list of gadgets that will compare the required
            -- locations
            let compare' = [ 
                        moveA ++  moveB ++  [G.Compare aReg bReg] | 
                        aLoc@(X.RegisterLocation aReg) <- a:locationPool
                    ,   bLoc@(X.RegisterLocation bReg) <- (b:locationPool) \\ [aLoc]
                    ,   moveA <- moveHelper locationPool aLoc a -- Move b into x
                    ,   moveB <- moveHelper locationPool bLoc b -- Move c into y
                    ]
            calculateJump compare' (G.Jump reason) indexLabel

always :: Predicate JumpReason
always = return Always

(->-) :: Variable -> Variable -> Predicate JumpReason
a ->- b = do
    a' <- varToLoc' a
    b' <- varToLoc' b
    return $ Because X.Greater a' b'

(->=-) :: Variable -> Variable -> Predicate JumpReason
a ->=- b = do
    a' <- varToLoc' a
    b' <- varToLoc' b
    return $ Because X.GreaterEqual a' b'

(-<-) :: Variable -> Variable -> Predicate JumpReason
a -<- b = do
    a' <- varToLoc' a
    b' <- varToLoc' b
    return $ Because X.Less a' b'

(-<=-) :: Variable -> Variable -> Predicate JumpReason
a -<=- b = do
    a' <- varToLoc' a
    b' <- varToLoc' b
    return $ Because X.LessEqual a' b'

(-==-) :: Variable -> Variable -> Predicate JumpReason
a -==- b = do
    a' <- varToLoc' a
    b' <- varToLoc' b
    return $ Because X.Equal a' b'

(-!=-) :: Variable -> Variable -> Predicate JumpReason
a -!=- b = do
    a' <- varToLoc' a
    b' <- varToLoc' b
    return $ Because X.NotEqual a' b'

--------------------------------------------------------------------------------
-- Predicate Helper Functions
--------------------------------------------------------------------------------

-- | Common jump calculation code. It takes a jump constructor a predicate
-- offset and returns a predicate that fulfills the jump. Backward jumps are
-- generated by the 'makePredicate' function immediately, while forward jumps are
-- translated to 'JumpHolder's of each possible size which later become fulfilled
-- by subsequent calls to 'makePredicate'.
calculateJump :: [[G.Gadget]] -> (Integer -> G.Gadget) -> Label -> Predicate ()
calculateJump allGadgetStreams jumpFlavor target = do
        state@CodeGenState{..}  <- get
        gadgetStream            <- lift allGadgetStreams -- grab a sequence of gadgets that do what we want
        byteStream              <- liftM B.concat $ mapM translateGadget gadgetStream -- translate all of the gadgets to instructions
        let jumpGadgets         = filter isJump $ M.keys $ D.gadgetMap library :: [G.Gadget]
        -- | This is pretty ugly and there is probably a better way to do
        -- this, but this will find all of the possible lengths that a jump
        -- instruction can be.
        currentOffset           <- lift $ maybeToList $ M.lookup (currentPredicate) predicateToByte
        let prefixLength        = fromIntegral $ B.length byteStream
        jumpLength              <- lift  
                                    . S.toList 
                                    . S.fromList
                                    . map (fromIntegral . B.length . fst) 
                                    . concatMap S.elems
                                    =<< (   lift
                                        $   maybeToList
                                        $   mapM (flip D.libraryLookup library) jumpGadgets
                                        )
        let jumpHolder          = JumpHolder {
                jumpFlavor      = jumpFlavor
            ,   jumpByteOffset  = currentOffset
            ,   jumpIndex       = currentPredicate
            ,   jumpLength      = jumpLength
            ,   jumpTarget      = target
            ,   jumpPrefix      = byteStream
            }
        put $! state{
                generatedCode       = generatedCode ++ [Left jumpHolder]
            ,   predicateToByte     = M.insert (currentPredicate+1) (currentOffset+jumpLength+prefixLength) predicateToByte
            ,   currentPredicate    = currentPredicate + 1 
            ,   locationPool        = locationPool -- We called translateGadget which modifies the pool, so we must restore it.
            }
    where
        isJump :: G.Gadget -> Bool
        isJump (G.Jump _ _) = True
        isJump _            = False

-- | A helper function that will generate a sequence of gadgets that will move
-- one location to another regardless of the type of location.
moveHelper  :: LocationPool -- ^ Locations that we are allowed to use as scratch
            -> X.Location   -- ^ The destination location
            -> X.Location   -- ^ The source location
            -> [[G.Gadget]]   -- ^ All the possible sequences of gadgets that will fulfill the move
moveHelper pool (X.RegisterLocation a) (X.RegisterLocation b)       
    | a == b    = [[]]
    | otherwise = [[G.LoadReg a b]]

moveHelper pool (X.MemoryLocation a offset) bLoc@(X.RegisterLocation b)  =
        [G.StoreMemReg a offset b]
    :   [
            [
                G.LoadReg taReg a
            ,   G.LoadReg tbReg b
            ,   G.StoreMemReg taReg offset tbReg
            ]
        |
                taLoc@(X.RegisterLocation taReg) <- (X.RegisterLocation a):pool
            ,   tbLoc@(X.RegisterLocation tbReg) <- ((bLoc:pool) \\ [taLoc])
        ]

moveHelper pool aLoc@(X.RegisterLocation a) (X.MemoryLocation b offset)  =
        [G.LoadMemReg a b offset]
    :   [
            [
                G.LoadReg tb b
            ,   G.LoadMemReg ta tb offset
            ,   G.LoadReg a ta
            ]
        |
                (X.RegisterLocation ta) <- aLoc:pool
            ,   (X.RegisterLocation tb) <- pool
        ]

moveHelper pool a@(X.MemoryLocation aReg aOffset) b@(X.MemoryLocation bReg bOffset) 
    | a == b    = [[]]
    | otherwise = 
        [
            [
                G.LoadReg taReg aReg
            ,   G.LoadMemReg tReg taReg aOffset
            ,   G.LoadReg tbReg bReg
            ,   G.StoreMemReg tbReg bOffset tReg
            ] 
        | 
                taLoc@(X.RegisterLocation taReg) <- (X.RegisterLocation aReg):pool
            ,   tbLoc@(X.RegisterLocation tbReg) <- (X.RegisterLocation bReg):pool
            ,   tLoc@(X.RegisterLocation tReg)   <- (pool \\ [tbLoc])
        ]

--moveHelper _ a b
--    | a == b    = [[]]
--    | otherwise = []

--------------------------------------------------------------------------------
-- External API
--------------------------------------------------------------------------------

generate :: D.GadgetLibrary -> StdGen -> PredicateProgram -> Maybe B.ByteString
generate library gen program = 
        listToMaybe -- If there are any solutions return Just the first
    $   map B.concat
    $   map rights -- Turn the Either values into Meta lists
    $   filter (null . lefts) -- Remove solutions with hanging jumps
    $   map (replaceAllJumpHolders . snd) -- Turn solutions into [Either Meta Jump]
    $   runStateT program -- Create a really long list of solutions
    $   initialState library gen 
    where
        -- | Attempts to replace forward jump statements for a given element in
        -- generatedCode. Because a jumpHolder may be replaced by several
        -- instructions it is necessary to return a list of lists and then
        -- concat the results.
        replaceAllJumpHolders :: CodeGenState -> [Either JumpHolder B.ByteString]
        replaceAllJumpHolders state@CodeGenState{..} = 
            let
                jumpReplacers   = map ((uncurry $ replaceJumpHolders state)) $ M.assocs predicateToByte
                replacedCode    = foldrM mapM generatedCode jumpReplacers
            in
                fst $ sampleState (replacedCode :: RVar [Either JumpHolder B.ByteString]) randomGenerator

        replaceJumpHolders :: CodeGenState -> Integer -> Integer -> Either JumpHolder B.ByteString -> RVar (Either JumpHolder B.ByteString)
        replaceJumpHolders state@CodeGenState{..} predIndex byteOffset j@(Left (JumpHolder {..}))
            | predIndex == labelToIndex state jumpTarget   = do
                let jumpGarbageSize         = if (predIndex-jumpIndex) <= 0
                                                then jumpLength + (fromIntegral $ B.length jumpPrefix)
                                                else 0
                let jumpGadget              = (jumpFlavor (byteOffset - jumpByteOffset - jumpGarbageSize))
                let gadgets                 = filter ((jumpLength==) . fromIntegral . B.length . fst)
                                                $ concatMap S.toList
                                                $ maybeToList
                                                $ D.libraryLookup jumpGadget library
                shuffled                    <- shuffle gadgets
                case listToMaybe $ filter (doesNotClobber locationPool jumpGadget) $ shuffled of --shuffled
                    Nothing         -> return j
                    Just (meta,_)   -> return $ Right $ jumpPrefix `B.append` meta
            | otherwise             = return j
        replaceJumpHolders _ _ _ meta = return meta

-- | Create a variable for use with the predicate functions.
makeVariable :: Predicate Variable
makeVariable = do
    state@CodeGenState{..}      <- get
    -- Find the next available offset from EBP to use as the local stack
    -- variable
    let (readable,writeable)    = M.foldrWithKey' findValidOffsets ([],[]) $ D.gadgetMap library
    let validOffsets            = map fromIntegral $ reverse $ sort $ readable `intersect` writeable
    stackOffset                 <- lift $ take 1 $ filter (<=localVariableOffset - 4) validOffsets
    let stackVariable           = X.MemoryLocation X.EBP $ fromIntegral stackOffset
    -- Allocate an integer id for the new variable
    let variableId              = (maximum $ 0 : M.keys variableMap) + 1
    variableLocation            <- lift $ locationPool++[stackVariable]
    put $! state {
            variableMap         = M.insert variableId variableLocation variableMap
        ,   locationPool        = locationPool \\ [variableLocation]
        ,   localVariableOffset = stackOffset
        }
    return variableId
    where
        -- | Places a Memory location offset into one a readable or writeable
        -- list. If folded over a list of gadgets, it will produces a tuple of
        -- two lists that contain all of the offsets that are readable and
        -- writeable respectively.
        findValidOffsets (G.LoadMemReg _ _ offset)  _ (readable,writeable)  = (offset:readable,writeable)
        findValidOffsets (G.StoreMemReg _ offset _) _ (readable,writeable)  = (readable,offset:writeable)
        findValidOffsets _                          _ lists                 = lists

-- | Create n variables for use with the predicate functions.
makeVariables :: Int -> Predicate [Variable]
makeVariables n = sequence $ replicate n makeVariable

-- | Creates a 'label' which is just an integer which maps to a position in the
-- predicate stream.
makeLabel ::Predicate Label
makeLabel = do
    state@CodeGenState{..}    <- get
    let indexLabel = 1 + (maximum $ 0 : M.keys labelMap)
    put $ state {
        labelMap        = M.insert indexLabel currentPredicate labelMap 
    }
    return indexLabel
    
makeLabels :: (Integral i) => i -> Predicate [Label]
makeLabels n = sequence $ replicate (fromIntegral n) makeLabel

-- | Mark the current position as the given label.
label :: Label -> Predicate ()
label l = do
    state@CodeGenState{..}    <- get
    put $ state {
        labelMap        = M.insert l currentPredicate labelMap 
    }
    return ()

--------------------------------------------------------------------------------
-- Predicate to Gadget translation
--------------------------------------------------------------------------------

makePredicate1 :: (X.Location -> [[G.Gadget]]) -> Variable -> Predicate ()
makePredicate1 generator a = do
    state <- get
    makePredicate (generator $ varToLoc state a)

makePredicate2 :: (X.Location -> X.Location -> [[G.Gadget]]) -> Variable -> Variable -> Predicate ()
makePredicate2 generator a b = do
    state <- get
    makePredicate ((generator `on` varToLoc state) a b)

makePredicate3 :: (X.Location -> X.Location -> X.Location -> [[G.Gadget]]) -> Variable -> Variable -> Variable -> Predicate ()
makePredicate3 generator a b c = do
    state <- get
    makePredicate ((generator `on` varToLoc state) a b $ varToLoc state c)

-- | A helper function that takes a list of lists of gadgets that fulfill a
-- given computation. It attempts to generate an instruction sequence for each
-- gadget in each possible sequence of gadgets. It also fills in any Jump
-- placeholders that it is able to.
makePredicate :: [[G.Gadget]] -> Predicate ()
makePredicate allGadgetStreams = do
    state@(CodeGenState{..})    <- get
    gadgetStream                <- lift allGadgetStreams -- grab a sequence of gadgets that do what we want
    metaStream                  <- mapM translateGadget gadgetStream -- translate all of the gadgets to instructions
    byteIndex                   <- lift $ maybeToList $ M.lookup currentPredicate predicateToByte
    let nextByteIndex           = byteIndex + (foldr' (+) 0 $ map (fromIntegral . B.length) metaStream)
    let newCode                 = generatedCode ++ [(Right $ B.concat metaStream)]
    put $! state{
            generatedCode       = newCode
        ,   predicateToByte     = M.insert (currentPredicate+1) nextByteIndex predicateToByte
        ,   currentPredicate    = currentPredicate + 1 
        ,   locationPool        = locationPool -- Restore the pool that may have been altered by gadgets
        }

-- | Turns a gadget into a sequence of assembly instructions. It also
-- removes from the location pool any location that is defined by the
-- gadget. This prevents a sequence of gadgets from clobbering
-- intermediate values.
translateGadget :: G.Gadget -> Predicate B.ByteString
translateGadget gadget = do
    state@(CodeGenState{..})    <- get
    gadgetSet                   <- lift $ maybeToList $ D.libraryLookup gadget library
    let (shuffled,gen)          = sampleState (shuffle $! S.toList gadgetSet) randomGenerator
    let newLocationPool         = locationPool \\ G.defines gadget
    -- put the generator in the state so that the jump replacement code
    -- and future gadgets can be random as well
    put $! state {
            randomGenerator = gen
            -- Remove any intermediate values from the location pool
        ,   locationPool    = newLocationPool
        }
    meta                        <- lift $ map fst $ filter (doesNotClobber newLocationPool gadget) $ shuffled
    return $! meta

   
-- | Ensures that a gadget realization does not clobber locations that
-- should not be clobbered.
doesNotClobber locationPool _            (_,[])         = True
doesNotClobber locationPool (G.Jump _ _) (_,clobber)    = doesNotClobber' (locationPool) clobber
doesNotClobber locationPool _            (_,clobber)    = doesNotClobber' ((X.RegisterLocation X.EFLAG):locationPool) clobber

doesNotClobber' locationPool clobber =
    let
        clobberSet              = S.fromList clobber
        clobberAbleLocations    = S.fromList $ (X.RegisterLocation X.EFLAG):locationPool
    in
        clobberSet `S.union` clobberAbleLocations == clobberAbleLocations

-- | Translates a variable into an expression location.
varToLoc :: CodeGenState -> Variable -> X.Location
varToLoc (CodeGenState {..}) var = fromJust $ M.lookup var variableMap

varToLoc' :: Variable -> Predicate X.Location
varToLoc' var = do
    (CodeGenState {..})  <- get
    return $ fromJust $ M.lookup var variableMap

labelToIndex :: CodeGenState -> Label -> Integer
labelToIndex state label = fromJust $ M.lookup label $ labelMap state

--locToVar :: CodeGenState -> X.Location -> Maybe Variable
--locToVar (_,vars,_) targetLoc = M.foldWithKey findLoc Nothing vars 
--    where
--        findLoc var loc res = if loc == targetLoc   then Just var
--                                                    else res

