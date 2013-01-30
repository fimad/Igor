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
, MatchTarget
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

-- | Match data is a location which corresponds to the location of the matched
-- expression, a mapping of 'Variable's to 'ExpressionValue's, and a list of
-- clobbered Locations.
type Match = (Location, VariableMap, [Location])

-- | The target expression to 'match' against. 
-- 'Expression' pair where the 'Variable' corresponds to a location mapping to
-- the desired 'Expression' in a given execution 'State'.
type MatchTarget = Expression

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
eval _ _                                              = Nothing

-- | Evaluates a list of instructions starting with an 'initialState' by
-- sequentially applying the 'eval' method.
evalFold :: [H.Instruction] -> Maybe State
evalFold = foldM eval initialState


--------------------------------------------------------------------------------
-- Matching methods
--------------------------------------------------------------------------------

-- | Attempts to union two 'Match'es. It is successful if the two matches either
-- do not contain overlapping variables, or if they do contain overlapping
-- variables, those variables correspond to the exact same expression.
matchUnion :: VariableMap -> VariableMap -> Maybe VariableMap
matchUnion x y =
  -- Check if all of the values in the intersection of the key space of x and y
  -- are equivalent.
  if and $ M.elems $ M.intersectionWith (\a b -> a == b) x y
  then Just $ M.union x y
  else Nothing
  
-- | Match a single 'MatchTarget' with a given 'State'.
match :: MatchTarget -> State -> [Match]
match target state = do
  (location,expression) <- (M.assocs state)
  let maybeVariableMap = matchExpression target expression
  guard (isJust maybeVariableMap)
  let variableMap = fromJust maybeVariableMap
  return $ (location, variableMap, M.keys $ M.delete location state)

-- | Attempt to match an 'Expression' (taken from a 'MatchTarget') with a given
-- 'Expression'.
matchExpression :: Expression -> Expression -> Maybe VariableMap
matchExpression (Immediate val1) (Immediate val2) = Just M.empty
matchExpression (Plus target1 target2) (Plus exp1 exp2)       = do
  match1 <- matchExpression target1 exp1
  match2 <- matchExpression target2 exp2
  matchUnion match1 match2
matchExpression (Minus target1 target2) (Minus exp1 exp2)     = do
  match1 <- matchExpression target1 exp1
  match2 <- matchExpression target2 exp2
  matchUnion match1 match2
matchExpression (ExpressionVariable variable) (Immediate val) = 
  return $ M.insert variable val M.empty
matchExpression _ _ = Nothing
