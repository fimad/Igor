module Igor.CodeGen
( 
-- * Types
  Variable
, CodeGenState
, Predicate
-- * Methods
, makePredicate
, makePredicate1
, makePredicate2
, makePredicate3
-- ** Predicates
, move
, noop
) where

import qualified    Data.Map    as M
import              Data.Maybe
import qualified    Data.Set    as S
import              Data.Random.List
import              Data.Random.Sample
import qualified    Igor.Expr   as X
import qualified    Igor.Gadget as G

-- So for predicates, if we keep location pools for unused locations, and a
-- mapping of variables to locations. 
--
-- Predicates should be non-deterministic functions that return a list of gadget
-- realizations and a modified list of location pools and maps (the state). Each
-- predicate will likely need a function that will take the variables and return
-- a list or set (most likely set, it looks like it will be easier/faster to
-- randomly pick from sets) instantiated gadgets that would fulfill that
-- predicate.
--
-- The code generation will need to handle several things:
-- 1) If the predicate returns an empty list... I think it is safe to fail in
--    this case with the expectation that a sufficiently large GadgetLibrary is
--    generated.
-- 2) Choosing an appropriate gadget instantiation that does not clobber
--    the set of mapped locations.
-- 3) In the event that there is no gadget that does not clobber the location,
--    pick one gadget that does clobber and rearrange the variable mappings such
--    that no mapped locations are clobbered.
--
-- I think then it is safe to start with just that, though it may be beneficial
-- to investigate randomly choosing between 2 and 3 as only falling back to 2 in
-- the event of failure will bias towards gadgets of a single instruction which
-- will lead to less variety in the resulting code.
--
-- Another possibility would to always use 3 and only use 2 for the rearranging
-- of variable mappings.

type Variable       = Int
type LocationPool   = S.Set X.Location
type VariableMap    = M.Map Variable X.Location
type CodeGenState   = (G.GadgetLibrary, VariableMap, LocationPool)

type Predicate      = CodeGenState -> (CodeGenState,Maybe [Metadata])

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

noop :: Predicate
noop = makePredicate [G.NoOp]

move :: Variable -> Variable -> Predicate
move a b = makePredicate2 move' a b
    where
        move' (RegisterLocation a) (RegisterLocation b) = [G.LoadReg a b]
        move' _ _ = []

add :: Variable -> Variable -> Predicate
add a b c = makePredicate3 add' a b c
    where
        add' (RegisterLocation a)(RegisterLocation b) (RegisterLocation c)
            | a == b    = [G.Plus a $ S.fromList [a,c]]
            | a == c    = [G.Plus a $ S.fromList [a,b]]
            | otherwise = 
        add' _ _ _ = []

--------------------------------------------------------------------------------
-- Predicate Generation
--------------------------------------------------------------------------------

makePredicate1 :: (X.Location -> [G.Gadget]) -> Variable -> Predicate
makePredicate1 generator a state = makePredicate (generator $ varToLoc state a) state

makePredicate2 :: (X.Location -> X.Location -> [G.Gadget]) -> Variable -> Variable -> Predicate
makePredicate2 generator a b state = makePredicate ((varToLoc state `on` generator) a b) state

makePredicate3 :: (X.Location -> X.Location -> X.Location -> [G.Gadget]) -> Variable -> Variable -> Variable -> Predicate
makePredicate3 generator a b c state = makePredicate ((varToLoc state `on` generator) a b $ varToLoc state c) state

makePredicate :: [G.Gadget] -> Predicate
makePredicate (g::_) state@(library,variables,freeLocations) = 
    -- Attempt to find a gadget in a Maybe monad
    let result = do
        gadgets                                 <- M.lookup g library
        let (meta, clobbered)                   = head $ S.toList gadgets
        (metaShuffle, newVariables, newFree)    <- shuffleVariables variables freeLocations
        return $ ((library,newVariables,newFree), Just metaShuffle ++ meta)
    -- If it is unsuccessful we still have to return a state, but we can return
    -- Nothing for the result of the computation.
    in case result of
        Nothing -> (state,Nothing)
        Just a -> a
    where
        shuffleVariables :: VariableMap -> LocationPool -> Maybe ([Metadata],VariableMap,LocationPool)
        shuffleVariables variables freeLocations
            | clobbered `S.intersection` variables == S.empty   =
                return ([], variables, freeLocations)
            | freeRegisters S.\\ clobbered /= S.empty           = do
                let clobberedLoc        = head $ S.toList $ clobbered
                let freeLoc             = head $ S.toList $ freeRegisters S.\\ clobbered
                let (RegisterLocation freeReg) = freeLoc
                gadget                  <- case clobberedLoc of
                                            (RegisterLocation locReg)   -> Just $ G.loadReg freeReg locReg
                                            _                           -> Nothing
                (meta,_)                <- listToMaybe $ filter ... $ S.toList $ M.lookup g library
                clobberedVar            <- locToVar clobberedVar
                (nextMeta,varMap,free)  <- shuffleVariables 
                return (meta++nextMeta, varMap, free)
            -- Ignore this case for now....
            | otherwise                                         =
                Nothing
        where
            isRegister (RegisterLocation _) = True
            isRegister _                    = False
            freeRegisters = S.map isRegister freeLocations

varToLoc :: CodeGenState -> Variable -> Maybe Location
varToLoc (_,vars,_) var = M.lookup var vars

locToVar :: CodeGenState -> Location -> Maybe Variable
locToVar (_,vars,_) targetLoc = M.foldWithKey findLoc Nothing vars 
    where
        findLoc var loc res = if loc == targetLoc   then Just var
                                                    else res

