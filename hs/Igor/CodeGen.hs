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
import              Data.Function
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
-- 1) Jumps
--      - Unconditional branches need to be handled by the eval
--          - code after an unconditional branch should be treated as garbage
--          and not evaluated
--
--      - Forward jumping needs to be implemented. This will likely require the
--      code generator to be extended to perform a search over the list of
--      gadgets. Once the code generator is capable of backtracking, forward
--      jumping can be implemented by picking a size for the jump, computing the
--      remainder of the predicate, and then filling in the jump instruction. If
--      no suitable jump instruction of the appropriate size is found (unlikely
--      if the eval has been modifed to append various length garbage to after
--      the jump) then another size is tried until an appropriate jump and size
--      is found.
--
--      - It will also be prudent to extract the majority of the jump code into
--      an gadget agnostic method so that the various other flavors of jump to
--      come can use the same logic.
--
-- 2) Constants
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
-- 3) Variable shuffling
--      - For any non trivial program that has more variables than general
--      registers there needs to be a mechanism for moving variables from
--      register locations to memory locations.
--
--      - The easiest implementation I can think of for this would be to choose
--      the smallest non-overlapping x such that [EBP+x] points to free memory
--      and assigning that to the variable. The state would need to keep track
--      of how much memory has been allocated for local variables, and then at
--      the end of generation prepend a stack modification that moves ESP at
--      least that far beyond the end of the stack.
--
-- 4) Non-deterministic and backtracking code generation
--      - The first aspect of this is rather trivial and that is to add a degree
--      of non determinism to the code generator, either by passing in a random
--      generator or making the result an IO object.
--
--      - Backtracking will likely be more difficult. My intuition is that I'll
--      be able to leverage the list monad and 'guards' for computing many
--      non-deterministic solutions, and then taking the head of the result.
--
-- 5) Conditionals
--      - This is fairly trivial compared to the previous milestones. This
--      involves the addition of flag locations and modifying the expression
--      datatype to include conditionals, and the eval function to add flag
--      modifications to the opcodes that need it along with the 'cmp' opcode
--      and the various conditional branch instructions. Likely all straight
--      forward but it will take a possibly substantial amount of code.

type Variable           = Int
type LocationPool       = [X.Location]
type VariableMap        = M.Map Variable X.Location
type PredicateProgram   = StateT CodeGenState [] ()
type Predicate          = StateT CodeGenState [] ()

data CodeGenState       = CodeGenState {
        library             :: D.GadgetLibrary
    ,   randomGenerator     :: StdGen
    ,   variableMap         :: VariableMap
    ,   locationPool        :: LocationPool
    ,   generatedCode       :: Maybe [Metadata]
    ,   currentPredicate    :: Integer
    ,   predicateToByte     :: M.Map Integer Integer -- ^ Maps from predicate indices to byte indices
    }

initialState :: D.GadgetLibrary -> StdGen -> CodeGenState
initialState library gen = CodeGenState {
        library             = library
    ,   randomGenerator     = gen
    ,   variableMap         = M.empty
    ,   locationPool        = map X.RegisterLocation X.generalRegisters
    ,   generatedCode       = Just []
    ,   currentPredicate    = 0
    ,   predicateToByte     = M.insert 0 0 M.empty
    }

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

noop :: Predicate
noop = makePredicate [G.NoOp]

move :: Variable -> Variable -> Predicate
move a b = makePredicate2 move' a b
    where
        move' (X.RegisterLocation a) (X.RegisterLocation b) = [G.LoadReg a b]
        move' _ _ = []

add :: Variable -> Variable -> Variable -> Predicate
add a b c = makePredicate3 add' a b c
    where
        add' (X.RegisterLocation a) (X.RegisterLocation b) (X.RegisterLocation c) = [G.Plus a $ S.fromList [b,c]]
        add' _ _ _ = []

jump :: Integer -> Predicate
jump = calculateJump G.Jump

calculateJump :: (Integer -> G.Gadget) -> Integer -> Predicate
calculateJump jumpFlavor offset =
    if offset <= 0
        then do 
            state@CodeGenState{..}  <- get
            currentOffset           <- lift $ maybeToList $ M.lookup (currentPredicate) predicateToByte
            jumpOffset              <- lift $ maybeToList $ M.lookup (currentPredicate+offset) predicateToByte
            makePredicate [jumpFlavor (jumpOffset - currentOffset)] >> return ()
        else failure --do
--            state@CodeGenState{..}  <- get
--            let jumpGadgets         = M.filterWithKey (\k _ -> iJump k) library
--            let jumpLengths         = S.toList $ S.fromList $ map (mdLength . fst) $ M.elems jumpGadgets
--            let maybeNewState = do
--                currentOffset       <- M.lookup (currentPredicate) predicateToByte
--                futureState         <- listToMaybe $ do
--                    trialSize       <- jumpLengths
--                    let tralState   = state{
--                                            generatedCode       = Just []
--                                        ,   predicateToByte     = M.insert (currentPredicate+1) (currentOffset+trialSize) predicateToByte
--                                        ,   currentPredicate    = currentPredicate + 1 
--                                        }
--                    return $ runState s
--                let futureState     <- snd $ runState state
--                jumpOffset          <- M.lookup (currentPredicate+offset) predicateToByte
--                return state
--
--            if isJust maybeNewState then put $ fromJust maybeNewState
--                                    else failure
    where

        failure :: Predicate
        failure = mzero

--------------------------------------------------------------------------------
-- External API
--------------------------------------------------------------------------------

generate :: D.GadgetLibrary -> StdGen -> PredicateProgram -> Maybe [Metadata]
generate library gen program = (listToMaybe $ runStateT program $ initialState library gen) >>= (generatedCode . snd)

makeVariables :: Int -> StateT CodeGenState [] [Variable]
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

makePredicate1 :: (X.Location -> [G.Gadget]) -> Variable -> Predicate
makePredicate1 generator a = do
    state <- get
    makePredicate (generator $ varToLoc state a)

makePredicate2 :: (X.Location -> X.Location -> [G.Gadget]) -> Variable -> Variable -> Predicate
makePredicate2 generator a b = do
    state <- get
    makePredicate ((generator `on` varToLoc state) a b)

makePredicate3 :: (X.Location -> X.Location -> X.Location -> [G.Gadget]) -> Variable -> Variable -> Variable -> Predicate
makePredicate3 generator a b c = do
    state <- get
    makePredicate ((generator `on` varToLoc state) a b $ varToLoc state c)

makePredicate :: [G.Gadget] -> Predicate
makePredicate (g:_) = do
    state@(CodeGenState{..})    <- get
    gadgets                     <- lift $ maybeToList $ M.lookup g library
    let (shuffled,gen)          = sampleState (shuffle $ S.toList gadgets) randomGenerator
    (meta, _)                   <- lift $ filter (doesNotClobber variableMap) $ shuffled
    (meta, _)                   <- lift $ filter (doesNotClobber variableMap) $ shuffled
    prevIndex                   <- lift $ maybeToList $ M.lookup currentPredicate predicateToByte
    let codeIndex               = prevIndex + (sum $ map (fromIntegral . mdLength) meta)
    let newCode                 = liftM2 (++) generatedCode $ return meta
    put state{
            generatedCode       = newCode
        ,   predicateToByte     = M.insert (currentPredicate+1) codeIndex predicateToByte
        ,   currentPredicate    = currentPredicate + 1 
        ,   randomGenerator     = gen
        }
   
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

varToLoc :: CodeGenState -> Variable -> X.Location
varToLoc (CodeGenState {..}) var = fromJust $ M.lookup var variableMap

--locToVar :: CodeGenState -> X.Location -> Maybe Variable
--locToVar (_,vars,_) targetLoc = M.foldWithKey findLoc Nothing vars 
--    where
--        findLoc var loc res = if loc == targetLoc   then Just var
--                                                    else res

