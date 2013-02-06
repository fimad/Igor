module Igor
( 
-- * Types
  Address
, Value
, Register (..)
, Flag (..)
, Location (..)
, State
, ExpressionValue (..)
, Expression (..)
, Match
-- * Methods
, valueOf
-- ** Evaluation
, eval
, evalFold
-- ** Expression Matching
, match
) where
import Control.Monad
import Data.Int
import Data.Maybe
import Data.Word
import System.Random
import Data.Set (Set, fromList)
import qualified Hdis86.Types as H
import qualified Data.Map as M

type Address = Word32

-- | Since we are only considering 32bit intel machines, values are always going
-- to fit inside of a 32bit integer. 
type Value = Word32

data Register = EAX
              | EBX
              | ECX
              | EDX
              | ESP
              | EBP
              | EDI
              | ESI
  deriving (Ord, Eq, Show)

-- | Todo: Find out what some flags are?
data Flag = NoFlag
  deriving (Ord, Eq, Show)

-- | If you think of the state of a machine as a dictionary, Locations are keys,
-- and include things like memory locations, registers and status flags.
data Location = MemoryLocation Address
              -- MemoryExpression
              | RegisterLocation Register 
              | FlagLocation Flag
  deriving (Ord, Eq, Show)

-- | The state of a program is simply a collection Todo: State should also keep
-- track if any "variable location" writes have taken place, that is writes
-- where the destination depends on the initial value of another location. There
-- are special things to take into consideration after such things occur.
type State = M.Map Location Expression

type Variable = String

data ExpressionValue = InitialValue Location
                     | Constant Value
  deriving (Ord, Eq, Show)

data Expression = Immediate ExpressionValue
                | Plus Expression Expression
                | Minus Expression Expression
                | ExpressionVariable Variable
  deriving (Ord, Eq, Show)

-- | Maps from a 'Variable' to an 'ExpressionValue'. Is used as the second
-- element of a 'Match' pair.
type VariableMap = M.Map Variable ExpressionValue

-- | A match is an instantiated gadget and a list of clobbered locations.
type Match = (Gadget, [Location])

-- | The types of basic computational units that are paramaterized
data Gadget = NoOp
            | LoadReg Register Register
            | LoadConst Register Value
            | PlusGadget Register (Set Register)
            | MinusGadget Register Register Register
  deriving (Ord, Eq, Show)

--------------------------------------------------------------------------------
-- Misc methods
--------------------------------------------------------------------------------

-- | A 'State' that corresponds to the beginning of execution.
initialState :: State
initialState = M.empty

-- | Returns the value of a location given a state. If the value has not been
-- assigned since the beginning of the evaluation then it returns the initial
-- value expression.
valueOf :: Maybe Location -> State -> Maybe Expression
valueOf Nothing _             = Nothing
valueOf (Just location) state = case M.lookup location state of
  Just expr -> return expr
  Nothing -> return $ Immediate $ InitialValue location


--------------------------------------------------------------------------------
-- Eval methods
--------------------------------------------------------------------------------

-- | Converts the disassembler's representation of registers to internal
-- representations.
gprToRegister :: H.GPR -> Maybe Register
gprToRegister H.RAX = Just EAX
gprToRegister H.RBX = Just EBX
gprToRegister H.RCX = Just ECX
gprToRegister H.RDX = Just EDX
gprToRegister H.RSP = Just ESP
gprToRegister H.RBP = Just EBP
gprToRegister H.RDI = Just EDI
gprToRegister H.RSI = Just ESI
gprToRegister _ = Nothing

-- | Convert an operand into a location
operandToLocation :: H.Operand -> Maybe Location
--operandToLocation (OpAddr addr _) = MemoryLocation addr
operandToLocation (H.Reg (H.Reg32 reg)) = (gprToRegister reg) >>= return . RegisterLocation
operandToLocation _                     = Nothing

-- | Grab the expression corresponding to an operand given the state of
-- execution.
operandToExpression :: H.Operand -> State -> Maybe Expression
operandToExpression (H.Imm word) _ = Just $ Immediate $ Constant (fromIntegral $ H.iValue word)
operandToExpression location state = valueOf (operandToLocation location) state

-- | Evaluates a single instruction, if we are unable to emulate the instruction
-- eval will return `Nothing`, otherwise it will return `Just` the resulting
-- state.
eval :: State -> H.Instruction -> Maybe State 
eval state instruction@(H.Inst {H.inOpcode = H.Inop}) = Just state
eval state instruction@(H.Inst {H.inOpcode = H.Iinc}) = do
  dst <- listToMaybe $ H.inOperands instruction
  dstLocation <- operandToLocation dst
  dstExpr <- operandToExpression dst state
  let dstValue =  Plus (Immediate $ Constant 1) dstExpr
  return $ M.insert dstLocation dstValue state
eval state instruction@(H.Inst {H.inOpcode = H.Imov}) = do
  dst <- listToMaybe $ H.inOperands instruction
  src <- listToMaybe $ drop 1 $ H.inOperands instruction
  dstLocation <- operandToLocation dst
  srcValue <- operandToExpression src state
  return $ M.insert dstLocation srcValue state
eval state instruction@(H.Inst {H.inOpcode = H.Iadd}) = do
  dst <- listToMaybe $ H.inOperands instruction
  src <- listToMaybe $ drop 1 $ H.inOperands instruction
  dstLocation <- operandToLocation dst
  srcValue <- operandToExpression src state
  return $ M.insert dstLocation (Plus (Immediate $ InitialValue dstLocation) srcValue) state
eval state instruction@(H.Inst {H.inOpcode = H.Isub}) = do
  dst <- listToMaybe $ H.inOperands instruction
  src <- listToMaybe $ drop 1 $ H.inOperands instruction
  dstLocation <- operandToLocation dst
  srcValue <- operandToExpression src state
  return $ M.insert dstLocation (Minus (Immediate $ InitialValue dstLocation) srcValue) state
eval _ _                                              = Nothing

-- | Evaluates a list of instructions starting with an 'initialState' by
-- sequentially applying the 'eval' method.
evalFold :: [H.Instruction] -> Maybe State
evalFold = foldM eval initialState


--------------------------------------------------------------------------------
-- Matching methods
--------------------------------------------------------------------------------
  
-- | Find all of the gadget matches for a given state.
match :: State -> [Match]
match state = do
  (location,expression) <- (M.assocs state)
  (gadget,nonClobbered) <- matchGadgets location expression
  return $ (gadget, M.keys $ foldl (flip M.delete) state nonClobbered)

-- | Match a given 
matchGadgets :: Location -- ^ The source of the current expression
            -> Expression -- ^ The expression to test against
            -> [(Gadget,[Location])] -- ^ A list of pairs of gadgets and their used locations that match the expression
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

    matchLoadReg srcLoc@(RegisterLocation srcReg) (Immediate (InitialValue dstLoc@(RegisterLocation dstReg))) =
      Just (LoadReg srcReg dstReg, [srcLoc])
    matchLoadReg _ _ = Nothing

    matchLoadConst srcLoc@(RegisterLocation srcReg) (Immediate (Constant value)) =
      Just (LoadConst srcReg value, [srcLoc])
    matchLoadConst _ _ = Nothing

    matchPlus srcLoc@(RegisterLocation srcReg) (Plus (Immediate (InitialValue (RegisterLocation reg1))) (Immediate (InitialValue (RegisterLocation reg2)))) =
      Just (PlusGadget srcReg (fromList [reg1, reg2]), [srcLoc])
    matchPlus _ _ = Nothing

    matchMinus srcLoc@(RegisterLocation srcReg) (Minus (Immediate (InitialValue (RegisterLocation reg1))) (Immediate (InitialValue (RegisterLocation reg2)))) =
      Just (MinusGadget srcReg reg1 reg2, [srcLoc])
    matchMinus _ _ = Nothing
