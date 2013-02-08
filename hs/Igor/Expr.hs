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
) where

import              Data.Binary
import              Data.DeriveTH
import              Data.Int
import              Data.Word

type Address = Word32

-- | Since we are only considering 32bit intel machines, values are always going
-- to fit inside of a 32bit integer. 
type Value = Int32

data Register = EAX
              | EBX
              | ECX
              | EDX
              | ESP
              | EBP
              | EDI
              | ESI
                -- | This kind of a lie, the EIP is only adjusted for jumps,
                -- even though it is actually being adjusted after every
                -- instruction executes...
              | EIP 
    deriving (Ord, Eq, Show, Read, Enum, Bounded)

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
    deriving (Ord, Eq, Show, Read)

$( derive makeBinary ''Register )
$( derive makeBinary ''Flag )
$( derive makeBinary ''Location )
$( derive makeBinary ''Expression )