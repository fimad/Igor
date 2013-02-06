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

import Data.Word

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

data Expression = InitialValue Location
                | Constant Value
                | Plus Expression Expression
                | Minus Expression Expression
    deriving (Ord, Eq, Show)

