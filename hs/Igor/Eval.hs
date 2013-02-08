module Igor.Eval
(
-- * Types
  State
-- * Methods
, eval
, eval'
, initialState
, valueOf
) where

import              Control.Monad
import              Data.Maybe
import qualified    Data.Map        as M
import qualified    Hdis86.Types    as H
import              Igor.Expr


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The state of a program is simply a collection Todo: State should also keep
-- track if any "variable location" writes have taken place, that is writes
-- where the destination depends on the initial value of another location. There
-- are special things to take into consideration after such things occur.
type State = M.Map Location Expression

--------------------------------------------------------------------------------
-- Methods
--------------------------------------------------------------------------------

-- | A 'State' that corresponds to the beginning of execution, this will likely
-- only be useful if you are using 'eval''.
initialState :: State
initialState = M.empty

-- | Returns the value of a location given a state. If the value has not been
-- assigned since the beginning of the evaluation then it returns the initial
-- value expression.
valueOf :: Maybe Location -> State -> Maybe Expression
valueOf Nothing _             = Nothing
valueOf (Just location) state = case M.lookup location state of
    Just expr -> return expr
    Nothing   -> return $ InitialValue location


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
gprToRegister _     = Nothing

-- | Convert an operand into a location
operandToLocation :: H.Operand -> Maybe Location
-- TODO: A design decision has been made to only allow relative addressing from
-- the ebp register, this will simplify the side affects of clobbering indirect
-- memory locations addressed against other registers.
operandToLocation (H.Mem (H.Memory _ (H.Reg32 base@H.RBP) (H.RegNone) _ (H.Immediate _ off))) =
    (gprToRegister base) >>= return . (flip MemoryLocation) (fromIntegral off)
--operandToLocation (H.Mem (H.Memory _ (H.RegNone) (H.Reg32 base@H.RBP) 1 (H.Immediate _ off))) =
--    (gprToRegister base) >>= return . (flip MemoryLocation) (fromIntegral off)
operandToLocation (H.Reg (H.Reg32 reg))     = (gprToRegister reg) >>= return . RegisterLocation
operandToLocation _                         = Nothing

-- | Grab the expression corresponding to an operand given the state of
-- execution.
operandToExpression :: H.Operand -> State -> Maybe Expression
operandToExpression (H.Imm word) _      = Just $ Constant (fromIntegral $ H.iValue word)
operandToExpression (H.Jump word) _     = Just $ Constant (fromIntegral $ H.iValue word)
operandToExpression (H.Const word) _    = Just $ Constant (fromIntegral $ H.iValue word)
operandToExpression location state      = valueOf (operandToLocation location) state

-- | Evaluates a list of instructions starting with an 'initialState' by
-- sequentially applying the 'eval' method.
eval :: [H.Metadata] -> Maybe State
eval = foldM (\s m -> eval' s $ H.mdInst m) initialState

-- | Evaluates a single instruction, if we are unable to emulate the instruction
-- eval will return `Nothing`, otherwise it will return `Just` the resulting
-- state.
eval' :: State -> H.Instruction -> Maybe State 
eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Inop})   = Just state
eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ipause}) = Just state

eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Iinc})   = do
    dst         <- listToMaybe $ H.inOperands instruction
    dstLocation <- operandToLocation dst
    dstExpr     <- operandToExpression dst state
    let dstValue =  Plus (Constant 1) dstExpr
    return $ M.insert dstLocation dstValue state

eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Imov})   = do
    dst         <- listToMaybe $ H.inOperands instruction
    src         <- listToMaybe $ drop 1 $ H.inOperands instruction
    dstLocation <- operandToLocation dst
    srcValue    <- operandToExpression src state
    return $ M.insert dstLocation srcValue state

eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Iadd})   = do
    dst         <- listToMaybe $ H.inOperands instruction
    src         <- listToMaybe $ drop 1 $ H.inOperands instruction
    dstLocation <- operandToLocation dst
    srcValue    <- operandToExpression src state
    return $ M.insert dstLocation (Plus (InitialValue dstLocation) srcValue) state

eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Isub})   = do
    dst         <- listToMaybe $ H.inOperands instruction
    src         <- listToMaybe $ drop 1 $ H.inOperands instruction
    dstLocation <- operandToLocation dst
    srcValue    <- operandToExpression src state
    return $ M.insert dstLocation (Minus (InitialValue dstLocation) srcValue) state

eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ipush})  = do
    src         <- listToMaybe $ H.inOperands instruction
    srcLocation <- operandToLocation src
    srcValue    <- operandToExpression src state
    case valueOf (Just $ RegisterLocation ESP) state of
        Just (InitialValue (RegisterLocation ESP))  -> return
                                                        $ M.insert (MemoryLocation ESP (-4)) srcValue
                                                        $ M.insert (RegisterLocation ESP)
                                                            (Minus (InitialValue (RegisterLocation ESP)) (Constant 4))
                                                          state
        _                                           -> Nothing

eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ipop})   = do
    src         <- listToMaybe $ H.inOperands instruction
    srcLocation <- operandToLocation src
    case valueOf (Just $ RegisterLocation ESP) state of
        Just (InitialValue (RegisterLocation ESP))  -> return
                                                        $ M.insert srcLocation (InitialValue $ MemoryLocation ESP 0)
                                                        $ M.insert (RegisterLocation ESP)
                                                            (Plus (InitialValue (RegisterLocation ESP)) (Constant 4))
                                                          state
        _                                           -> Nothing

-- | TODO: As way to get added random instructions, after an unconditional
-- branch we can take the rest of the stream without clobbering any locations
eval' state instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ijmp})   = do
    src         <- listToMaybe $ H.inOperands instruction
    srcValue    <- operandToExpression src state
    case valueOf (Just $ RegisterLocation EIP) state of
        Just (InitialValue (RegisterLocation EIP))  -> return $ M.insert (RegisterLocation EIP) srcValue state
        _                                           -> Nothing

eval' _ _                                                                   = Nothing
