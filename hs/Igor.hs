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
-- * Methods
, valueOf
, eval
, evalFold
) where
import Control.Monad
import Data.Int
import Data.Maybe
import Data.Word
import System.Random
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
              -- | MemoryExpression
              | RegisterLocation Register 
              | FlagLocation Flag
  deriving (Ord, Eq, Show)

-- | The state of a program is simply a collection Todo: State should also keep
-- track if any "variable location" writes have taken place, that is writes
-- where the destination depends on the initial value of another location. There
-- are special things to take into consideration after such things occur.
type State = M.Map Location Expression

initialState :: State
initialState = M.empty

data ExpressionValue = InitialValue Location
                     | Constant Value
                     | ExpressionVariable String
  deriving (Ord, Eq, Show)

data Expression = Immediate ExpressionValue
                | Plus Expression Expression
                | Minus Expression Expression
  deriving (Ord, Eq, Show)

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
operandToLocation _ = Nothing

-- | Grab the expression corresponding to an operand given the state of
-- execution.
operandToExpression :: H.Operand -> State -> Maybe Expression
operandToExpression (H.Imm word) _ = Just $ Immediate $ Constant (fromIntegral $ H.iValue word)
operandToExpression location state = valueOf (operandToLocation location) state

-- | Evaluates a single instruction.
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
eval _ _                                              = Nothing

-- | Evaluates a list of instructions starting with an 'initialState' by
-- sequentially applying the 'eval' method.
evalFold :: [H.Instruction] -> Maybe State
evalFold = foldM eval initialState

