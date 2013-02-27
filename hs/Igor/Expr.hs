{-# LANGUAGE TemplateHaskell #-}
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
                -- | If some condition is true, then the expression happens. This
                -- is only really useful for conditional jumps. If you tried to
                -- use it to log effects after conditional jumps then you would
                -- end up overwriting the state they were previously in. You
                -- would need to extend it to include both possible locations
                -- and that is overly complex for what it is actually used for.
                | Conditional Reason Expression
    deriving (Ord, Eq, Show, Read)

$( derive makeBinary ''Reason )
$( derive makeBinary ''Register )
$( derive makeBinary ''Address )
$( derive makeBinary ''Location )
$( derive makeBinary ''Expression )

$( derive makeNFData ''Reason )
$( derive makeNFData ''Register )
$( derive makeNFData ''Address )
$( derive makeNFData ''Location )
$( derive makeNFData ''Expression )
