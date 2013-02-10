module Igor.ByteModel
( 
-- * Types
  Generator
, Model
-- * Methods
, uniform
, generate
) where

import              Codec.Compression.GZip
import qualified    Data.ByteString                     as B
import              Data.Random.Distribution
import qualified    Data.Random.Distribution.Uniform    as U
import              Data.Random.RVar
import              Data.Word
import              Hdis86
import              Hdis86.Types

-- | A model randomly generates a finite list of bytes according to a given byte
-- distribution.
type Model     = RVar [Word8]

-- | A generator will generate a list of instruction 'Metadata'.
type Generator = RVar [Metadata]

-- | Generates a random list of bytes from the uniform distribution.
-- TODO: Implement an actual model...
uniform :: Int -> Model
uniform numBytes = do
    let model = U.stdUniform
    sequence $ replicate numBytes model

-- | Generates a random instruction stream based on the model function that is
-- passed in.
generate :: Model -> RVar [Metadata]
generate model = do
    words       <- model
    -- attempt to disassemble them
    let result  = disassembleMetadata (intel32 {cfgCPUMode = Mode32, cfgSyntax = SyntaxIntel}) $ B.pack words 
    -- if successful return the instruction list, otherwise try again
    case result of
        []        -> generate model
        otherwise -> return result

