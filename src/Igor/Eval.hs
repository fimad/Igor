{-# LANGUAGE BangPatterns #-}
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
import              Data.Int
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
--
-- | Because we need to prevent random memory reads we need to keep track of
-- every value that a location has taken on. The head of the list represents the
-- final value, with older values as you move through the list.
type State = M.Map Location [Expression]

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
    Just expr -> return $ head expr
    Nothing   -> return $ InitialValue location


-- | A wrapper for map insert that handles appending the most recent value to
-- the list of all values a location has taken on.
insertValue :: Location -> Expression -> State -> State
insertValue location expression state = M.insert location (expression:expressions) state
    where
        expressions = fromMaybe [] $ M.lookup location state


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
operandToLocation (H.Mem (H.Memory _ (H.Reg32 base) (H.RegNone) _ (H.Immediate _ off))) =
    (gprToRegister base) >>= return . MemoryLocation . (flip OffsetAddress) (fromIntegral off)
operandToLocation (H.Mem (H.Memory _ (H.Reg32 base) (H.Reg32 index) scale (H.Immediate _ off))) = do
    baseReg     <- gprToRegister base
    indexReg    <- gprToRegister index
    return $ MemoryLocation $ IndexedAddress baseReg indexReg (fromIntegral scale) (fromIntegral off)
operandToLocation (H.Reg (H.Reg32 reg))     = (gprToRegister reg) >>= return . RegisterLocation
operandToLocation _                         = Nothing

-- | Grab the expression corresponding to an operand given the state of
-- execution.
operandToExpression :: H.Operand -> State -> Maybe Expression
operandToExpression (H.Imm word) _      = Just $ Constant (fromIntegral $ H.iValue word)
operandToExpression (H.Jump word) _     = Just $ Constant (fromIntegral $ H.iValue word)
operandToExpression (H.Const word) _    = Just $ Constant (fromIntegral $ H.iValue word)
operandToExpression location state      = valueOf (operandToLocation location) state

--------------------------------------------------------------------------------
-- Utilities to use in the eval method
--------------------------------------------------------------------------------

-- | For simple opcodes that have a direct mapping to an expression,
-- this function will pull out 2 operands, turn then to expressions and
-- insert the result into the corresponding source location in the
-- state.
buildExpr2 ::  State -> (Expression -> Expression -> Expression) -> [H.Operand] -> Maybe (State, Bool)
buildExpr2 state expr operands = do
    dst                                 <- listToMaybe $ operands
    src                                 <- listToMaybe $ drop 1 $ operands
    dstLocation                         <- operandToLocation dst
    srcValue                            <- operandToExpression src state
    dstValue                            <- operandToExpression dst state
    return (insertValue dstLocation (expr dstValue srcValue) state, False)

-- | Marks that the flags register has been clobbered
clobbersFlags :: State -> State
clobbersFlags state = insertValue (RegisterLocation EFLAG) (Clobbered) state

-- | Marks the flags register as containing the result of a comparison
compareExpr :: State -> [H.Operand] -> Maybe State
compareExpr state operands = do
    dst                                             <- listToMaybe $ operands
    src                                             <- listToMaybe $ drop 1 $ operands
    srcValue@(InitialValue (RegisterLocation _))    <- operandToExpression src state
    dstValue@(InitialValue (RegisterLocation _))    <- operandToExpression dst state
    return $! insertValue (RegisterLocation EFLAG) (Comparison dstValue srcValue) state

buildJump :: State -> Int32 -> (Value -> Expression) -> [H.Operand] -> Maybe (State, Bool)
buildJump state size expr operands = do
    src                 <- listToMaybe operands
    (Constant srcValue) <- operandToExpression src state
    case valueOf (Just $ RegisterLocation EIP) state of
        Just (InitialValue (RegisterLocation EIP))  -> return 
                                                    $ (insertValue (RegisterLocation EIP) (expr $ srcValue) state, True)
        _                                           -> Nothing

-- | A function that marks the first operand of an opcode as clobbered and marks
-- that it reads from the second operand
clobbersFirstAndReads :: State -> Int32 -> H.Instruction -> Maybe (State,Bool)
clobbersFirstAndReads state size instruction =  do
    dst         <- listToMaybe $ H.inOperands instruction
    src         <- listToMaybe $ drop 1 $ H.inOperands instruction
    dstLocation <- operandToLocation dst
    srcValue    <- operandToExpression src state
    return $ (insertValue dstLocation (ClobberedReading srcValue) $ clobbersFlags state, False)

-- | A function that marks the first operand of an opcode as clobbered and
-- either doesn't have other operands or will definitely perform a memory access
-- on them.
clobbersFirst :: State -> Int32 -> H.Instruction -> Maybe (State,Bool)
clobbersFirst state size instruction =  do
    dst         <- listToMaybe $ H.inOperands instruction
    dstLocation <- operandToLocation dst
    return $ (insertValue dstLocation Clobbered $ clobbersFlags state, False)

--------------------------------------------------------------------------------
-- The many cases of eval....
--------------------------------------------------------------------------------

-- | Evaluates a list of instructions starting with an 'initialState' by
-- sequentially applying the 'eval' method.
eval :: [H.Metadata] -> Maybe State
eval = liftM fst . foldM (\s m -> eval'' s (fromIntegral $ H.mdLength m) $ H.mdInst m) (initialState, False)

-- | A hack to stop the evaluation of instructions after an unconditional jump.
eval'' :: (State,Bool) -> Int32 -> H.Instruction -> Maybe (State,Bool)
eval'' (state,True) _ _                 = Nothing
eval'' (state,False) size instruction   = eval' state size instruction

-- | Evaluates a single instruction, if we are unable to emulate the instruction
-- eval will return `Nothing`, otherwise it will return `Just` the resulting
-- state and if the rest of the instructions can be treated as junk.
eval' :: State -> Int32 -> H.Instruction -> Maybe (State, Bool)
eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Inop})     = Just (state,False)
eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ipause})   = Just (state,False)

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Imov})     = do
    dst         <- listToMaybe $ H.inOperands instruction
    src         <- listToMaybe $ drop 1 $ H.inOperands instruction
    dstLocation <- operandToLocation dst
    srcValue    <- operandToExpression src state
    return $ (insertValue dstLocation srcValue state, False)

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Iinc})     = do
    let state'  = clobbersFlags state
    dst                                 <- listToMaybe $ H.inOperands instruction
    dstLocation                         <- operandToLocation dst
    dstExpr                             <- operandToExpression dst state'
    let dstValue                        =  Plus (Constant 1) dstExpr
    return $ (insertValue dstLocation dstValue state', False)

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Iimul})     = do
    let state'  = clobbersFlags state
    src                                 <- listToMaybe $ H.inOperands instruction
    -- There are other weirder versions of imul??
    -- we don't want them
    guard (length (H.inOperands instruction) == 1)
    let clobberedLocation               = RegisterLocation EDX
    let dstLocation                     = RegisterLocation EAX
    dstExpr                             <- valueOf (Just dstLocation) state'
    srcExpr                             <- operandToExpression src state'
    let dstValue                        =  Times dstExpr srcExpr
    return $ (insertValue clobberedLocation Clobbered $ insertValue dstLocation dstValue state', False)

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ixor})     = do
    let state'  = clobbersFlags state
    buildExpr2 state' Xor (H.inOperands instruction)

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Iadd})     = do
    let state'  = clobbersFlags state
    buildExpr2 state' Plus (H.inOperands instruction)

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Isub})     = do
    state'      <- compareExpr state (H.inOperands instruction)
    buildExpr2 state' Minus (H.inOperands instruction)

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Isar})     = do
    let state'  = clobbersFlags state
    buildExpr2 state' RightShift (H.inOperands instruction)

--------------------------------------------------------------------------------
-- Stack
--------------------------------------------------------------------------------

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ipush})    = do
    src         <- listToMaybe $ H.inOperands instruction
    srcLocation <- operandToLocation src
    srcValue    <- operandToExpression src state
    case valueOf (Just $ RegisterLocation ESP) state of
        Just (InitialValue (RegisterLocation ESP))  -> return (
                                                          insertValue (MemoryLocation $ OffsetAddress ESP (-4)) srcValue
                                                        $ insertValue (RegisterLocation ESP)
                                                            (Minus (InitialValue (RegisterLocation ESP)) (Constant 4))
                                                          state
                                                        , False)
        _                                           -> Nothing

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ipop})     = do
    src         <- listToMaybe $ H.inOperands instruction
    srcLocation <- operandToLocation src
    case valueOf (Just $ RegisterLocation ESP) state of
        Just (InitialValue (RegisterLocation ESP))  -> return (
                                                          insertValue srcLocation (InitialValue $ MemoryLocation $ OffsetAddress ESP 0)
                                                        $ insertValue (RegisterLocation ESP)
                                                            (Plus (InitialValue (RegisterLocation ESP)) (Constant 4))
                                                          state
                                                        , False)
        _                                           -> Nothing

--------------------------------------------------------------------------------
-- Branches
--------------------------------------------------------------------------------

eval' state _ instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Icmp})     = do
    state'      <- compareExpr state (H.inOperands instruction)
    return (state', False)

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ijmp})  = do
    buildJump state size Constant (H.inOperands instruction) 

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ijz})  = do
    buildJump state size (\c -> Conditional Equal $ Constant c) (H.inOperands instruction)

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ijnz})  = do
    buildJump state size(\c -> Conditional NotEqual $ Constant c) (H.inOperands instruction) 

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ijge})  = do
    buildJump state size(\c -> Conditional GreaterEqual $ Constant c) (H.inOperands instruction) 

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ijg})  = do
    buildJump state size(\c -> Conditional Greater $ Constant c) (H.inOperands instruction) 

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ijle})  = do
    buildJump state size(\c -> Conditional LessEqual $ Constant c) (H.inOperands instruction) 

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ijl})  = do
    buildJump state size(\c -> Conditional Less $ Constant c) (H.inOperands instruction) 

--------------------------------------------------------------------------------
-- Instructions we just can't deal with
--------------------------------------------------------------------------------

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ilea})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Idec})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ineg})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Inot})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ircl})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ircr})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Irol})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Iror})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Isal})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ishl})  = clobbersFirst state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ishr})  = clobbersFirst state size instruction

eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Iadc})  = clobbersFirstAndReads state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Isbb})  = clobbersFirstAndReads state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Iand})  = clobbersFirstAndReads state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Ior})   = clobbersFirstAndReads state size instruction
eval' state size instruction@(H.Inst {H.inPrefixes = [], H.inOpcode = H.Itest}) = clobbersFirstAndReads state size instruction


eval' _ _ _                                                                     = Nothing

