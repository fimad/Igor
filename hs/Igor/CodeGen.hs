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
, makeVariable
, makeVariables
-- ** Predicates
, move
, add
, jump
, noop
) where

import              Control.Monad.State
import              Data.Either
import              Data.Function
import              Data.Foldable (foldr')
import              Data.List
import qualified    Data.Map                as M
import              Data.Maybe
import qualified    Data.Set                as S
import              Data.Random
import              Data.Random.List
--import              Data.Random.Sample
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
--
-- 2) Variable shuffling
--      - It just occurred to me that shuffling the locations of variables in
--      programs that jump around in the code may have inconsistent variable
--      mappings and clobberings. A solution to this would be to only store
--      variables as EBP offsets. The difficulty with this is that there will
--      likely only be a couple registers that read and write to that location
--      so there will likely be many layers of indirection before a gadget can
--      actually be used.
--
--      - Because we have back tracking... it seems that we may be able to
--      assign variables to multiple locations. First we can try the next
--      available register (and possible all available registers...), and next
--      we can try adding a memory location. That way for small programs all of
--      the variables will fit into registers, and for large programs some or
--      all will be on the stack.
--
-- 3) Conditionals
--      - This is fairly trivial compared to the previous milestones. This
--      involves the addition of flag locations and modifying the expression
--      datatype to include conditionals, and the eval function to add flag
--      modifications to the opcodes that need it along with the 'cmp' opcode
--      and the various conditional branch instructions. Likely all straight
--      forward but it will take a possibly substantial amount of code.

-- | Variables are the type passed to the user of the library and currently the
-- arguments to the predicate functions. This may change in the future for when
-- constants are added.
type Variable           = Int

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

-- | A place holder for a jump. In the event that a predicate attempts to jump
-- ahead to code that has not been generated yet, it will instead drop a
-- JumpHolder which contains enough information so that a second pass through
-- the stream can replace the JumpHolder with an appropriate Gadget realization. 
data JumpHolder = JumpHolder {
        jumpFlavor  :: Integer -> G.Gadget -- ^ The type of jump that we should place here
    ,   jumpOffset  :: Integer
    ,   length      :: Integer
    ,   target      :: Integer
}

data CodeGenState       = CodeGenState {
        library             :: D.GadgetLibrary
    ,   randomGenerator     :: StdGen
    ,   variableMap         :: VariableMap
    ,   locationPool        :: LocationPool
    ,   generatedCode       :: [Either Metadata JumpHolder]
    ,   currentPredicate    :: Integer
    ,   predicateToByte     :: M.Map Integer Integer -- ^ Maps from predicate indices to byte indices
    ,   localVariableOffset :: Integer -- ^ How far from EBP should we assign the next local variable?
    }

initialState :: D.GadgetLibrary -> StdGen -> CodeGenState
initialState library gen = CodeGenState {
        library             = library
    ,   randomGenerator     = gen
    ,   variableMap         = M.empty
    ,   locationPool        = map X.RegisterLocation X.generalRegisters
    ,   generatedCode       = []
    ,   currentPredicate    = 0
    ,   predicateToByte     = M.insert 0 0 M.empty
    ,   localVariableOffset = 0
    }

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

noop :: Predicate ()
noop = makePredicate [[G.NoOp]]

move :: Variable -> Variable -> Predicate ()
move a b = do
    CodeGenState{..} <- get
    makePredicate2 (moveHelper locationPool) a b

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

jump :: Integer -> Predicate ()
jump = calculateJump G.Jump

--------------------------------------------------------------------------------
-- Predicate Helper Functions
--------------------------------------------------------------------------------

-- | Common jump calculation code. It takes a jump constructor a predicate
-- offset and returns a predicate that fulfills the jump. Backward jumps are
-- generated by the 'makePredicate' function immediately, while forward jumps are
-- translated to 'JumpHolder's of each possible size which later become fulfilled
-- by subsequent calls to 'makePredicate'.
calculateJump :: (Integer -> G.Gadget) -> Integer -> Predicate ()
calculateJump jumpFlavor offset =
    if offset <= 0
        then do 
            state@CodeGenState{..}  <- get
            byteIndex               <- lift $ maybeToList $ M.lookup (currentPredicate) predicateToByte
            jumpByteIndex           <- lift $ maybeToList $ M.lookup (currentPredicate+offset) predicateToByte
            makePredicate [[jumpFlavor (jumpByteIndex - byteIndex)]]
        else do
            state@CodeGenState{..}  <- get
            let jumpGadgets         = filter isJump $ M.keys $ D.gadgetMap library :: [G.Gadget]
            -- | This is pretty ugly and there is probably a better way to do
            -- this, but this will find all of the possible lengths that a jump
            -- instruction can be.
            currentOffset           <- lift $ maybeToList $ M.lookup (currentPredicate) predicateToByte
            jumpLength              <- lift  
                                        . S.toList 
                                        . S.fromList
                                        . map (sum . map (fromIntegral . mdLength) . fst) 
                                        . concatMap S.elems
                                        =<< (   lift
                                            $   maybeToList
                                            $   mapM (flip D.libraryLookup library) jumpGadgets
                                            )
            currentOffset           <- lift $ maybeToList $ M.lookup (currentPredicate) predicateToByte
            let jumpHolder          = JumpHolder {
                    jumpFlavor  = jumpFlavor
                ,   jumpOffset  = currentOffset
                ,   length      = jumpLength 
                ,   target      = (currentPredicate + offset)
                }
            put $! state{
                    generatedCode       = generatedCode ++ [Right jumpHolder]
                ,   predicateToByte     = M.insert (currentPredicate+1) (currentOffset+jumpLength) predicateToByte
                ,   currentPredicate    = currentPredicate + 1 
                }
    where
        isJump :: G.Gadget -> Bool
        isJump (G.Jump _)   = True
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

moveHelper _ a b
    | a == b    = [[]]
    | otherwise = []

--------------------------------------------------------------------------------
-- External API
--------------------------------------------------------------------------------

generate :: D.GadgetLibrary -> StdGen -> PredicateProgram -> Maybe [Metadata]
generate library gen program = 
        listToMaybe -- If there are any solutions return Just the first
    $   map lefts -- Turn the Either values into Meta lists
    $   filter (null . rights) -- Remove solutions with hanging jumps
    $   map (generatedCode . snd) -- Turn solutions into [Either Meta Jump]
    $   runStateT program -- Create a really long list of solutions
    $   initialState library gen 

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
    --variableLocation            <- lift $ (take 1 locationPool)++[stackVariable]++(drop 1 locationPool)
--    variableLocation            <- lift $ locationPool++[stackVariable]
    variableLocation            <- lift $ [stackVariable]
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
    metaStream                  <- liftM concat $ mapM translateGadget gadgetStream -- translate all of the gadgets to instructions
    byteIndex                   <- lift $ maybeToList $ M.lookup currentPredicate predicateToByte
    let nextByteIndex           = byteIndex + (foldr' (+) 0 $ map (fromIntegral . mdLength) metaStream)
    jumpReplacedCode            <- liftM concat $ mapM (replaceJumpHolders currentPredicate byteIndex) generatedCode
    let newCode                 = jumpReplacedCode ++ (map Left metaStream)
    put $! state{
            generatedCode       = newCode
        ,   predicateToByte     = M.insert (currentPredicate+1) nextByteIndex predicateToByte
        ,   currentPredicate    = currentPredicate + 1 
        ,   locationPool        = locationPool -- Restore the pool that may have been altered by gadgets
        }
    where
        -- | Turns a gadget into a sequence of assembly instructions. It also
        -- removes from the location pool any location that is defined by the
        -- gadget. This prevents a sequence of gadgets from clobbering
        -- intermediate values.
        translateGadget :: G.Gadget -> Predicate [Metadata]
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
            meta                        <- lift $ map fst $ filter (doesNotClobber newLocationPool) $ shuffled
            return $! meta

        -- | Attempts to replace forward jump statements for a given element in
        -- generatedCode. Because a jumpHolder may be replaced by several
        -- instructions it is necessary to return a list of lists and then
        -- concat the results.
        replaceJumpHolders :: Integer -> Integer -> (Either Metadata JumpHolder) -> Predicate ([Either Metadata JumpHolder])
        replaceJumpHolders predIndex byteOffset j@(Right (JumpHolder {..}))
            | predIndex == target   = do
                state@(CodeGenState{..})    <- get
                let gadgets                 = filter ((length==) . foldr' (+) 0 . map (fromIntegral . mdLength) . fst)
                                                $ concatMap S.toList
                                                $ maybeToList
                                                $ D.libraryLookup (jumpFlavor (byteOffset-jumpOffset)) library
                let (shuffled,gen)          = sampleState (shuffle gadgets) randomGenerator
                (meta, _)                   <- lift $ filter (doesNotClobber locationPool) $ shuffled
                put $! state{ randomGenerator = gen }
                return $! map Left meta
            | otherwise             = return [j]
        replaceJumpHolders _ _ meta = return [meta]

   
-- | Ensures that a gadget realization does not clobber locations that
-- should not be clobbered.
doesNotClobber locationPool (_,[])         = True
doesNotClobber locationPool (_,clobber)    = 
    let
        clobberSet              = S.fromList clobber
        clobberAbleLocations    = S.fromList locationPool
    in
        clobberSet `S.union` clobberAbleLocations == clobberAbleLocations

-- | Translates a variable into an expression location.
varToLoc :: CodeGenState -> Variable -> X.Location
varToLoc (CodeGenState {..}) var = fromJust $ M.lookup var variableMap

--locToVar :: CodeGenState -> X.Location -> Maybe Variable
--locToVar (_,vars,_) targetLoc = M.foldWithKey findLoc Nothing vars 
--    where
--        findLoc var loc res = if loc == targetLoc   then Just var
--                                                    else res

