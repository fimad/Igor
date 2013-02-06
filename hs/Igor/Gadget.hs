module Igor.Gadget
( 
-- * Types
  Gadget (..)
, ClobberList
, Match (..)
-- * Methods
, match
) where

import qualified    Data.Set   as S
import qualified    Data.Map   as M
import              Data.Maybe
import              Igor.Eval
import qualified    Igor.Expr  as X

-- | The types of basic computational units that are paramaterized
data Gadget = NoOp
            | LoadReg X.Register X.Register
            | LoadConst X.Register X.Value
            | Plus X.Register (S.Set X.Register)
            | Minus X.Register X.Register X.Register
    deriving (Ord, Eq, Show, Read)

type ClobberList = [X.Location]
-- | A match is an instantiated gadget and a list of clobbered locations.
type Match = (Gadget, ClobberList)
  
-- | Takes a 'State' and returns a set of all of the 'Match'es for the gadgets
-- that correspond to the given execution state.
match :: State -> [Match]
match state = do
    (location,expression) <- (M.assocs state)
    (gadget,nonClobbered) <- matchGadgets location expression
    return (gadget, M.keys $ foldl (flip M.delete) state nonClobbered)

-- | Create a list of all of the
matchGadgets :: X.Location -- ^ The source of the current expression
             -> X.Expression -- ^ The expression to test against
             -> [Match] -- ^ A list of pairs of gadgets and their used locations that match the expression
matchGadgets source expression = catMaybes $ map ($expression) gadgetMatchers
    where
        gadgetMatchers = map ($source) [
              matchNoOp
            , matchLoadReg
            , matchLoadConst
            , matchPlus
            , matchMinus
            ]

        matchNoOp _ _ = Just (NoOp, [])

        matchLoadReg
            srcLoc@(X.RegisterLocation srcReg)
            (X.InitialValue (X.RegisterLocation dstReg)) =
                Just (LoadReg srcReg dstReg, [srcLoc])
        matchLoadReg _ _ =
                Nothing

        matchLoadConst srcLoc@(X.RegisterLocation srcReg) (X.Constant value) =
                Just (LoadConst srcReg value, [srcLoc])
        matchLoadConst _ _ =
                Nothing

        matchPlus
            srcLoc@(X.RegisterLocation srcReg)
            (X.Plus (X.InitialValue (X.RegisterLocation reg1)) (X.InitialValue (X.RegisterLocation reg2))) =
                Just (Plus srcReg (S.fromList [reg1, reg2]), [srcLoc])
        matchPlus _ _ =
                Nothing

        matchMinus
            srcLoc@(X.RegisterLocation srcReg)
            (X.Minus (X.InitialValue (X.RegisterLocation reg1)) (X.InitialValue (X.RegisterLocation reg2))) =
                Just (Minus srcReg reg1 reg2, [srcLoc])
        matchMinus _ _ =
                Nothing

