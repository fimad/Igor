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
, makeVariables
--, makePredicate
--, makePredicate1
--, makePredicate2
--, makePredicate3
-- ** Predicates
, move
, add
, jump
, noop
) where

import              Control.Monad.State
import              Data.Either
import              Data.Function
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
--      - The easiest implementation I can think of for this would be to choose
--      the smallest non-overlapping x such that [EBP+x] points to free memory
--      and assigning that to the variable. The state would need to keep track
--      of how much memory has been allocated for local variables, and then at
--      the end of generation prepend a stack modification that moves ESP at
--      least that far beyond the end of the stack.
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
    }

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

noop :: Predicate ()
noop = makePredicate [[G.NoOp]]

move :: Variable -> Variable -> Predicate ()
move a b = makePredicate2 move' a b
    where
        move' (X.RegisterLocation a) (X.RegisterLocation b) = [[G.LoadReg a b]]
        move' _ _ = []

add :: Variable -> Variable -> Variable -> Predicate ()
add a b c = makePredicate3 add' a b c
    where
        add' (X.RegisterLocation a) (X.RegisterLocation b) (X.RegisterLocation c) = [[G.Plus a $ S.fromList [b,c]]]
        add' _ _ _ = []

jump :: Integer -> Predicate ()
jump = calculateJump G.Jump

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
            let jumpGadgets         = M.filterWithKey (\k _ -> isJump k) library
            -- | This is pretty ugly and there is probably a better way to do
            -- this, but this will find all of the possible lengths that a jump
            -- instruction can be.
            currentOffset           <- lift $ maybeToList $ M.lookup (currentPredicate) predicateToByte
            jumpLength              <- lift
                                        $ S.toList 
                                        $ S.fromList
                                        $ map (sum . map (fromIntegral . mdLength) . fst) 
                                        $ concatMap S.elems
                                        $ M.elems jumpGadgets
            currentOffset           <- lift $ maybeToList $ M.lookup (currentPredicate) predicateToByte
            let jumpHolder          = JumpHolder {
                    jumpFlavor  = jumpFlavor
                ,   jumpOffset  = currentOffset
                ,   length      = jumpLength 
                ,   target      = (currentPredicate + offset)
                }
            put state{
                    generatedCode       = generatedCode ++ [Right jumpHolder]
                ,   predicateToByte     = M.insert (currentPredicate+1) (currentOffset+jumpLength) predicateToByte
                ,   currentPredicate    = currentPredicate + 1 
                }
    where
        isJump :: G.Gadget -> Bool
        isJump (G.Jump _)   = True
        isJump _            = False

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

makeVariables :: Int -> Predicate [Variable]
makeVariables n = do
    state@CodeGenState{..}  <- get
    let varList             = zip [((maximum $ 0 : M.keys variableMap)+1) .. ] (take n locationPool)
    let newPool             = drop n locationPool
    let newVars             = variableMap `M.union` (M.fromList varList)
    let newState            = state{variableMap = newVars, locationPool = newPool}
    put newState
    return $ map fst varList

--------------------------------------------------------------------------------
-- Predicate Generation
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
    let nextByteIndex           = byteIndex + (sum $ map (fromIntegral . mdLength) metaStream)
    jumpReplacedCode            <- liftM concat $ mapM (replaceJumpHolders currentPredicate byteIndex) generatedCode
    let newCode                 = jumpReplacedCode ++ (map Left metaStream)
    put state{
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
            gadgetSet                   <- lift $ maybeToList $ M.lookup gadget library
            let (shuffled,gen)          = sampleState (shuffle $ S.toList gadgetSet) randomGenerator
            -- put the generator in the state so that the jump replacement code
            -- and future gadgets can be random as well
            put state {
                    randomGenerator = gen
                    -- Remove any intermediate values from the location pool
                ,   locationPool    = locationPool \\ G.defines gadget
                }
            lift $ map fst $ filter (doesNotClobber variableMap) $ shuffled

        -- | Attempts to replace forward jump statements for a given element in
        -- generatedCode. Because a jumpHolder may be replaced by several
        -- instructions it is necessary to return a list of lists and then
        -- concat the results.
        replaceJumpHolders :: Integer -> Integer -> (Either Metadata JumpHolder) -> Predicate ([Either Metadata JumpHolder])
        replaceJumpHolders predIndex byteOffset j@(Right (JumpHolder {..}))
            | predIndex == target   = do
                state@(CodeGenState{..})    <- get
                let gadgets                 = filter ((length==) . sum . map (fromIntegral . mdLength) . fst)
                                                $ concatMap S.toList
                                                $ maybeToList
                                                $ M.lookup (jumpFlavor (byteOffset-jumpOffset)) library
                let (shuffled,gen)          = sampleState (shuffle gadgets) randomGenerator
                (meta, _)                   <- lift $ filter (doesNotClobber variableMap) $ shuffled
                put state{ randomGenerator = gen }
                return $ map Left meta
            | otherwise             = return [j]
        replaceJumpHolders _ _ meta = return [meta]

   
-- | Ensures that a gadget realization does not clobber locations that
-- should not be clobbered.
doesNotClobber variables (_,clobber) = 
    let
        clobberSet              = (S.fromList clobber)
        usedLocations           = (S.fromList $ M.elems $ variables)
        untouchableLocations    = usedLocations `S.union` (S.fromList $ map X.RegisterLocation X.specialRegisters)
        clobbersUsedLocations   = clobberSet `S.intersection` untouchableLocations /= S.empty
        isMemoryLocation a      = case a of
            X.MemoryLocation _ _    -> True
            _                       -> False
        clobbersMemoryLocations = any isMemoryLocation clobber 
    in
        not clobbersMemoryLocations && not clobbersUsedLocations

-- | Translates a variable into an expression location.
varToLoc :: CodeGenState -> Variable -> X.Location
varToLoc (CodeGenState {..}) var = fromJust $ M.lookup var variableMap

--locToVar :: CodeGenState -> X.Location -> Maybe Variable
--locToVar (_,vars,_) targetLoc = M.foldWithKey findLoc Nothing vars 
--    where
--        findLoc var loc res = if loc == targetLoc   then Just var
--                                                    else res

