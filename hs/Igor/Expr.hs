{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Igor.Expr
( 
-- * Types
  Address (..)
, Value
, Register (..)
, Reason (..)
, Location (..)
, Expression (..)
, generalRegisters
, specialRegisters
) where

import              Control.DeepSeq
import              Data.Binary
import              Data.DeriveTH
import              Data.Int
import              Data.List
import              Data.Word
--
-- | Since we are only considering 32bit intel machines, values are always going
-- to fit inside of a 32bit integer. 
type Value = Int32

data Register = EAX
              | EBX
              | ECX
              | EDX
              | EDI
              | ESI
              | ESP
              | EBP
                -- | This kind of a lie, the EIP is only adjusted for jumps,
                -- even though it is actually being adjusted after every
                -- instruction executes...
              | EIP 
                -- | I suppose this is also kind of a lie. The flag register is
                -- sort of treated as a black box, any instruction that modifies
                -- the flag besides cmp is said to clobber the flags. The only
                -- instruction that meaningfully 
              | EFLAG
    deriving (Ord, Eq, Show, Read, Enum, Bounded)

-- | A reason for branching.
data Reason = Always
            | GreaterEqual
            | Greater
            | LessEqual
            | Less
            | Equal
            | NotEqual
    deriving (Ord, Eq, Show, Read)

-- | General purpose registers that can be used as variables or scratch
-- space or what ever.
generalRegisters :: [Register]
generalRegisters = [EAX, EBX, ECX, EDX, EDI, ESI]

-- | These registers cannot be used as variables or scratch space, and should
-- not be clobbered by junk code.
specialRegisters :: [Register]
specialRegisters = [minBound .. maxBound] \\ generalRegisters

data Address  = OffsetAddress Register Value
              | IndexedAddress Register Register Value Value -- base index scale offset
    deriving (Ord, Eq, Show, Read)

-- | If you think of the state of a machine as a dictionary, Locations are keys,
-- and include things like memory locations, registers and status flags.
data Location = --MemoryLocation Address
                --MemoryLocation Register Value
                MemoryLocation Address
              | RegisterLocation Register 
    deriving (Ord, Eq, Show, Read)

data Expression = InitialValue Location
                | Constant Value
                | Plus Expression Expression
                | Minus Expression Expression
                | Xor Expression Expression
                | Times Expression Expression
                | RightShift Expression Expression -- ^ Corresponds to a right arithmetic shift
                -- | An expression that sets the flag bits of the source to be
                -- the result of comparing the two locations.
                | Comparison Expression Expression 
                -- | Corresponds to some value we don't care about
                | Clobbered 
                -- | Corresponds to some value we don't care about that might
                -- have evaluated the given expression. Useful for catching
                -- random memory reads, which in turn lead to segfaults, yuck...
                | ClobberedReading Expression
                -- | If some condition is true, then the expression happens. This
                -- is only really useful for conditional jumps. If you tried to
                -- use it to log effects after conditional jumps then you would
                -- end up overwriting the state they were previously in. You
                -- would need to extend it to include both possible locations
                -- and that is overly complex for what it is actually used for.
                | Conditional Reason Expression
    deriving (Ord, Eq, Show, Read)

instance Binary Register where
    get = do
        !byte   <-  getWord8
        case byte of
            0   -> return EAX
            1   -> return EBX
            2   -> return ECX
            3   -> return EDX
            4   -> return EDI
            5   -> return ESI
            6   -> return ESP
            7   -> return EBP
            8   -> return EIP 
            9   -> return EFLAG

    put EAX     = putWord8 0
    put EBX     = putWord8 1
    put ECX     = putWord8 2
    put EDX     = putWord8 3
    put EDI     = putWord8 4
    put ESI     = putWord8 5
    put ESP     = putWord8 6
    put EBP     = putWord8 7
    put EIP     = putWord8 8
    put EFLAG   = putWord8 9

instance Binary Address where
    get = do
        !byte    <-  getWord8
        case byte of
            0   -> do
                !base        <- get
                !value       <- get
                return $! OffsetAddress base value
            1   -> do
                !base        <- get
                !index       <- get
                !scale       <- get
                !value       <- get
                return $! IndexedAddress base index scale value

    put (OffsetAddress base value) = do
        putWord8 0
        put base
        put value
    put (IndexedAddress base index scale value) = do
        putWord8 1
        put base
        put index
        put scale
        put value

instance Binary Location where
    get = do
        !byte    <-  getWord8
        case byte of
            0   -> do
                !address     <- get
                return $! MemoryLocation address
            1   -> do
                !register    <- get
                return $! RegisterLocation register

    put (MemoryLocation address) = do
        putWord8 0
        put address
    put (RegisterLocation register) = do
        putWord8 1
        put register

instance Binary Reason where
    get = do
        !byte   <-  getWord8
        case byte of
            0   -> return Always
            1   -> return GreaterEqual
            2   -> return Greater
            3   -> return LessEqual
            4   -> return Less
            5   -> return Equal
            6   -> return NotEqual

    put Always          = putWord8 0
    put GreaterEqual    = putWord8 1
    put Greater         = putWord8 2
    put LessEqual       = putWord8 3
    put Less            = putWord8 4
    put Equal           = putWord8 5
    put NotEqual        = putWord8 6

$( derive makeNFData ''Reason )
$( derive makeNFData ''Register )
$( derive makeNFData ''Address )
$( derive makeNFData ''Location )
$( derive makeNFData ''Expression )
