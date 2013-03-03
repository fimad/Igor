{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
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
            | LoadMemReg X.Register X.Address
            | StoreMemReg X.Address X.Register
            | Plus X.Register (S.Set X.Register)
            | Xor X.Register (S.Set X.Register)
            | Times X.Register (S.Set X.Register)
            | Minus X.Register X.Register X.Register
            | RightShift X.Register Integer -- arithmetic shift
            | Compare X.Register X.Register
            | Jump X.Reason Integer
    deriving (Ord, Eq, Show, Read)

instance Binary Gadget where
    get = do
        !byte <- getWord8
        case byte of
            0   ->  return NoOp
            1   ->  do
                !r1  <- get
                !r2  <- get
                return $! LoadReg r1 r2
            2   ->  do
                !r1  <- get
                !val <- get
                return $! LoadConst r1 val
            3   ->  do
                !r   <- get
                !a   <- get
                return $! LoadMemReg r a
            4   ->  do
                !a   <- get
                !r   <- get
                return $! StoreMemReg a r
            5   ->  do
                !r   <- get
                !s   <- get
                return $! Plus r s
            6   ->  do
                !r  <- get
                !s  <- get
                return $! Xor r s
            7   ->  do
                !r  <- get
                !s  <- get
                return $! Times r s
            8   ->  do
                !r1 <- get
                !r2 <- get
                !r3 <- get
                return $! Minus r1 r2 r3
            9   ->  do
                !r  <- get
                !i  <- get
                return $! RightShift r i
            10  ->  do
                !r1 <- get
                !r2 <- get
                return $! Compare r1 r2
            11  ->  do
                !r  <- get
                !i  <- get
                return $! Jump r i

    put NoOp                = putWord8 0
    put (LoadReg r1 r2)     = do
                                putWord8 1
                                put r1
                                put r2
    put (LoadConst r v)     = do
                                putWord8 2
                                put r
                                put v
    put (LoadMemReg r a)    = do
                                putWord8 3
                                put r
                                put a
    put (StoreMemReg a r)   = do
                                putWord8 4
                                put a
                                put r
    put (Plus r s)          = do
                                putWord8 5
                                put r
                                put s
    put (Xor r s)           = do
                                putWord8 6
                                put r
                                put s
    put (Times r s)         = do
                                putWord8 7
                                put r
                                put s
    put (Minus r1 r2 r3)    = do
                                putWord8 8
                                put r1
                                put r2
                                put r3
    put (RightShift r i)    = do
                                putWord8 9
                                put r
                                put i
    put (Compare r1 r2)     = do
                                putWord8 10
                                put r1
                                put r2
    put (Jump r i)          = do
                                putWord8 11
                                put r
                                put i

$( derive makeNFData ''Gadget )

type ClobberList = [X.Location]
-- | A match is an instantiated gadget and a list of clobbered locations.
type Match = (Gadget, ClobberList)
  
-- | Returns all of the locations (if any) that are defined by this gadget
defines :: Gadget -> [X.Location]
defines (LoadReg        r _)    = [X.RegisterLocation r]
defines (LoadConst      r _)    = [X.RegisterLocation r]
defines (LoadMemReg     r _)  = [X.RegisterLocation r]
defines (StoreMemReg    a _)    = [X.MemoryLocation a]
defines (Plus           r _)    = [X.RegisterLocation r]
defines (Xor            r _)    = [X.RegisterLocation r]
defines (Times          r _)    = [X.RegisterLocation r]
defines (Minus          r _ _)  = [X.RegisterLocation r]
defines (RightShift     r _)    = [X.RegisterLocation r]
defines _                       = []

-- | Takes a 'State' and returns a set of all of the 'Match'es for the gadgets
-- that correspond to the given execution state.
match :: State -> [Match]
match state = do
    (location,expression)   <- (M.assocs state)
    (gadget,nonClobbered)   <- matchGadgets location expression
    let allOtherExpressions = location `M.delete` state
    --let allOtherExpressions = foldl (flip M.delete) state nonClobbered
    --let clobbered           = M.keys $ allOtherExpressions
    let clobbered           = M.keys $ foldl (flip M.delete) state nonClobbered
    -- Ensure that we don't perform any bad reads or clobber any variable memory
    -- location
    guard $ all (not.  isIllegalExpression) $ M.elems allOtherExpressions
    guard $ all (not.  isIllegalExpression) $ map (X.InitialValue) clobbered
    return (gadget, clobbered)
    where
        -- | Make sure that no expression touches a memory location. If this is
        -- not done, it would be possible for a junk expression read from an out
        -- of bounds memory location causing a segfault. Certainly undesirable.
        isIllegalExpression (X.InitialValue (X.RegisterLocation _)) = False
        isIllegalExpression (X.Constant _)                          = False
        isIllegalExpression (X.Clobbered)                           = False
        isIllegalExpression (X.InitialValue _)                      = True
        isIllegalExpression (X.Plus a b)                            = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.Minus a b)                           = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.Xor a b)                             = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.Times a b)                           = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.RightShift a b)                      = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.Comparison a b)                      = isIllegalExpression a || isIllegalExpression b
        isIllegalExpression (X.Conditional _ a)                     = isIllegalExpression a
        isIllegalExpression _                                       = True

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
            , matchXor
            , matchMul
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

        matchLoadConst srcLoc@(X.RegisterLocation srcReg) (X.Constant value)
            | srcReg /= X.EIP   = Just (LoadConst srcReg value, [srcLoc])
            | otherwise         = Nothing
        matchLoadConst _ _      = Nothing

        matchLoadMemReg
            srcLoc@(X.RegisterLocation srcReg)
            (X.InitialValue (X.MemoryLocation address)) =
                Just (LoadMemReg srcReg address, [srcLoc])
        matchLoadMemReg _ _ =
                Nothing

        matchStoreMemReg
            srcLoc@(X.MemoryLocation address)
            (X.InitialValue (X.RegisterLocation dstReg)) =
                Just (StoreMemReg address dstReg, [srcLoc])
        matchStoreMemReg _ _ =
                Nothing

        matchPlus
            srcLoc@(X.RegisterLocation srcReg)
            (X.Plus (X.InitialValue (X.RegisterLocation reg1)) (X.InitialValue (X.RegisterLocation reg2))) =
                Just (Plus srcReg (S.fromList [reg1, reg2]), [srcLoc])
        matchPlus _ _ =
                Nothing

        matchXor
            srcLoc@(X.RegisterLocation srcReg)
            (X.Xor (X.InitialValue (X.RegisterLocation reg1)) (X.InitialValue (X.RegisterLocation reg2))) =
                Just (Xor srcReg (S.fromList [reg1, reg2]), [srcLoc])
        matchXor _ _ =
                Nothing

        matchMul
            srcLoc@(X.RegisterLocation srcReg)
            (X.Times (X.InitialValue (X.RegisterLocation reg1)) (X.InitialValue (X.RegisterLocation reg2))) =
                Just (Times srcReg (S.fromList [reg1, reg2]), [srcLoc])
        matchMul _ _ =
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

