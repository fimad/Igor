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
import              Data.Random.List
import              Data.Random.Sample
import qualified    Igor.Expr               as X
import qualified    Igor.Gadget             as G
import qualified    Igor.Gadget.Discovery   as D
import              Hdis86

type Variable           = Int
type LocationPool       = [X.Location]
type VariableMap        = M.Map Variable X.Location
type PredicateProgram   = State CodeGenState ()
type Predicate          = State CodeGenState ()

data CodeGenState       = CodeGenState {
        library             :: D.GadgetLibrary
    ,   variableMap         :: VariableMap
    ,   locationPool        :: LocationPool
    ,   generatedCode       :: Maybe [Metadata]
    ,   currentPredicate    :: Integer
    ,   predicateToByte     :: M.Map Integer Integer -- ^ Maps from predicate indices to byte indices
    }

initialState :: CodeGenState
initialState = CodeGenState {
        library             = D.emptyLibrary
    ,   variableMap         = M.empty
    ,   locationPool        = map X.RegisterLocation [minBound .. maxBound]
    ,   generatedCode       = Just []
    ,   currentPredicate    = 1
    ,   predicateToByte     = M.insert 1 0 M.empty
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
jump offset = if offset <= 0
    then do
        state@CodeGenState{..}  <- get
        let maybeCurrentOffset  = M.lookup (currentPredicate) predicateToByte
        let maybeJumpOffset     = M.lookup (currentPredicate+offset) predicateToByte
        case (maybeCurrentOffset, maybeJumpOffset) of
            (Just currentOffset, Just jumpOffset)   -> makePredicate [G.Jump (jumpOffset - currentOffset)]
            _                                       -> failure
    -- | TODO: We need a way of handling jumps into future portions of the
    -- code, right now it will just die a most painful death
    else failure
    where
        failure :: Predicate
        failure = do
            state@CodeGenState{..} <- get
            put state{generatedCode = Nothing}
            return ()

generate :: D.GadgetLibrary -> PredicateProgram -> Maybe [Metadata]
generate library state = generatedCode $ snd $ runState state $ initialState{library=library}

--makeVariables :: Int -> CodeGenState -> ([Variable],CodeGenState)
makeVariables :: Int -> State CodeGenState [Variable]
makeVariables n = do
    state@CodeGenState{..}  <- get
    let varList             = zip [((maximum $ 0 : M.keys variableMap)+1) .. ] (take n locationPool)
    let newPool             = drop n locationPool
    let newVars             = variableMap `M.union` (M.fromList varList)
    put state{variableMap = newVars, locationPool = newPool}
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
    state@(CodeGenState{..}) <- get
    -- Attempt to find a gadget in a Maybe monad
    let result = do
        gadgets         <- M.lookup g library
        let (meta, _)   = head $ reverse $ filter (doesNotClobber variableMap) $ S.toList gadgets
        prevIndex       <- M.lookup currentPredicate predicateToByte
        let codeIndex   = prevIndex + (sum $ map (fromIntegral . mdLength) meta)
        let newCode     = liftM2 (++) generatedCode $ return meta
        return (newCode, codeIndex)
    case result of 
        Just (newCode, index)   -> do
                                    put state{
                                            generatedCode       = newCode
                                        ,   predicateToByte     = M.insert (currentPredicate+1) index predicateToByte
                                        ,   currentPredicate    = currentPredicate + 1 
                                        }
                                    return ()
        Nothing                 -> do
                                    put state{
                                            generatedCode       = Nothing
                                        }
                                    return ()
    where
        doesNotClobber variables (_,clobber) = (S.fromList clobber) `S.intersection` (S.fromList $ M.elems $ variables) == S.empty

varToLoc :: CodeGenState -> Variable -> X.Location
varToLoc (CodeGenState {..}) var = fromJust $ M.lookup var variableMap

--locToVar :: CodeGenState -> X.Location -> Maybe Variable
--locToVar (_,vars,_) targetLoc = M.foldWithKey findLoc Nothing vars 
--    where
--        findLoc var loc res = if loc == targetLoc   then Just var
--                                                    else res

