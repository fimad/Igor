{-# LANGUAGE TemplateHaskell #-}
module Igor.Gadget
( 
-- * Types
  Gadget (..)
, ClobberList
, Match (..)
-- * Methods
, match
, defines
) where

import              Control.DeepSeq
import              Control.Monad
import              Data.Binary
import              Data.DeriveTH
import qualified    Data.Set    as S
import qualified    Data.Map    as M
import              Data.Maybe
import              Igor.Eval
import qualified    Igor.Expr   as X

-- | The types of basic computational units that are paramaterized. The
-- convention for multi-parameter gadgets is that the destination location(s)
-- is/are the first parameter(s).
data Gadget = NoOp
            | LoadReg X.Register X.Register
            | LoadConst X.Register X.Value
            | LoadMemReg X.Register X.Register X.Value
            | StoreMemReg X.Register X.Value X.Register
            | Plus X.Register (S.Set X.Register)
            | Minus X.Register X.Register X.Register
            | RightShift X.Register Integer -- arithmetic shift
            | Compare X.Register X.Register
            | Jump X.Reason Integer
    deriving (Ord, Eq, Show, Read)
$( derive makeBinary ''Gadget )
$( derive makeNFData ''Gadget )

type ClobberList = [X.Location]
-- | A match is an instantiated gadget and a list of clobbered locations.
type Match = (Gadget, ClobberList)
  
-- | Returns all of the locations (if any) that are defined by this gadget
defines :: Gadget -> [X.Location]
defines (LoadReg        r _)    = [X.RegisterLocation r]
defines (LoadConst      r _)    = [X.RegisterLocation r]
defines (LoadMemReg     r _ _)  = [X.RegisterLocation r]
defines (StoreMemReg    r v _)  = [X.MemoryLocation r v]
defines (Plus           r _)    = [X.RegisterLocation r]
defines (Minus          r _ _)  = [X.RegisterLocation r]
defines (RightShift     r _)    = [X.RegisterLocation r]
defines _                       = []

-- | Takes a 'State' and returns a set of all of the 'Match'es for the gadgets
-- that correspond to the given execution state.
match :: State -> [Match]
match state = do
    (location,expression)   <- (M.assocs state)
    let allOtherExpressions = location `M.delete` state
    guard $ not $ any isIllegalExpression $ M.elems allOtherExpressions
    (gadget,nonClobbered)   <- matchGadgets location expression
    return (gadget, M.keys $ foldl (flip M.delete) state nonClobbered)
    where
        -- | Make sure that no expression touches a memory location. If this is
        -- not done, it would be possible for a junk expression read from an out
        -- of bounds memory location causing a segfault. Certainly undesirable.
        isIllegalExpression (X.InitialValue (X.MemoryLocation _ _)) = True
        isIllegalExpression (X.Plus a b)                            = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.Minus a b)                           = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.RightShift a b)                      = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.Comparison a b)                      = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.Conditional _ a)                     = isIllegalExpression a
        isIllegalExpression _                                       = False

-- | Create a list of all of the
matchGadgets :: X.Location -- ^ The source of the current expression
             -> X.Expression -- ^ The expression to test against
             -> [Match] -- ^ A list of pairs of gadgets and their used locations that match the expression
matchGadgets source expression = catMaybes $ map ($ expression) gadgetMatchers
    where
        gadgetMatchers = map ($ source) [
              matchNoOp
            , matchLoadReg
            , matchLoadConst
            , matchLoadMemReg
            , matchStoreMemReg
            , matchPlus
            , matchMinus
            , matchRightShift
            , matchCompare
            , matchJump
            ]

        --matchNoOp _ _ = Just (NoOp, [])
        matchNoOp _ _ = Nothing

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

        matchLoadMemReg
            srcLoc@(X.RegisterLocation srcReg)
            (X.InitialValue (X.MemoryLocation dstReg offset)) =
                Just (LoadMemReg srcReg dstReg offset, [srcLoc])
        matchLoadMemReg _ _ =
                Nothing

        matchStoreMemReg
            srcLoc@(X.MemoryLocation srcReg offset)
            (X.InitialValue (X.RegisterLocation dstReg)) =
                Just (StoreMemReg srcReg offset dstReg, [srcLoc])
        matchStoreMemReg _ _ =
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

        -- | There seems to be a peculiar case of the shr opcode that shr 1
        -- passes in a 0 value for the constant.
        matchRightShift
            srcLoc@(X.RegisterLocation srcReg)
            (X.RightShift (X.InitialValue (X.RegisterLocation reg)) (X.Constant 0)) =
                regEquiv srcReg reg >> Just (RightShift reg 1, [srcLoc])
        matchRightShift
            srcLoc@(X.RegisterLocation srcReg)
            (X.RightShift (X.InitialValue (X.RegisterLocation reg)) (X.Constant amount)) =
                regEquiv srcReg reg >> Just (RightShift reg $ fromIntegral amount, [srcLoc])
        matchRightShift _ _ =
                Nothing

        matchCompare
            srcLoc@(X.RegisterLocation X.EFLAG)
            (X.Comparison (X.InitialValue (X.RegisterLocation a)) (X.InitialValue (X.RegisterLocation b))) =
                Just (Compare a b, [X.RegisterLocation X.EFLAG])
        matchCompare _ _ =
                Nothing

        matchJump
            srcLoc@(X.RegisterLocation X.EIP)
            (X.Constant offset) =
                Just (Jump X.Always $ fromIntegral offset, [X.RegisterLocation X.EIP])
        matchJump
            srcLoc@(X.RegisterLocation X.EIP)
            (X.Conditional reason (X.Constant offset)) =
                Just (Jump reason $ fromIntegral offset, [X.RegisterLocation X.EIP])
        matchJump _ _ =
                Nothing

        regEquiv :: X.Register -> X.Register -> Maybe ()
        regEquiv r1 r2 = if r1 == r2 then Just () else Nothing

