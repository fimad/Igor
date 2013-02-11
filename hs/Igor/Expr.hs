{-# LANGUAGE TemplateHaskell #-}
module Igor.Expr
( 
-- * Types
  Address
, Value
, Register (..)
, Flag (..)
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

type Address = Word32

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
    deriving (Ord, Eq, Show, Read, Enum, Bounded)

-- | General purpose registers that can be used as variables or scratch
-- space or what ever.
generalRegisters :: [Register]
generalRegisters = [EAX .. ESI]

-- | These registers cannot be used as variables or scratch space, and should
-- not be clobbered by junk code.
specialRegisters :: [Register]
specialRegisters = [minBound .. maxBound] \\ generalRegisters


-- | Todo: Find out what some flags are?
data Flag = NoFlag
    deriving (Ord, Eq, Show, Read)

-- | If you think of the state of a machine as a dictionary, Locations are keys,
-- and include things like memory locations, registers and status flags.
data Location = --MemoryLocation Address
                MemoryLocation Register Value
              | RegisterLocation Register 
              | FlagLocation Flag
    deriving (Ord, Eq, Show, Read)

data Expression = InitialValue Location
                | Constant Value
                | Plus Expression Expression
                | Minus Expression Expression
                | RightShift Expression Expression -- ^ Corresponds to a right logical shift
    deriving (Ord, Eq, Show, Read)

$( derive makeBinary ''Register )
$( derive makeBinary ''Flag )
$( derive makeBinary ''Location )
$( derive makeBinary ''Expression )

$( derive makeNFData ''Register )
$( derive makeNFData ''Flag )
$( derive makeNFData ''Location )
$( derive makeNFData ''Expression )
