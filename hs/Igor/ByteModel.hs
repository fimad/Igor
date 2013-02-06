module Igor.ByteModel
( 
-- * Types
  Generator
, Model
-- * Methods
, uniform
, generate
) where

import              Data.Word
import qualified    Data.ByteString as  B
import              Hdis86
import              Hdis86.Types
import              System.Random

-- | A model randomly generates a finite list of bytes according to a given byte
-- distribution.
type Model     = IO [Word8]

-- | A generator will generate a list of instruction 'Metadata'.
type Generator = IO [Metadata]

-- | Generates a random list of bytes from the uniform distribution.
-- TODO: Implement an actual model...
uniform :: Int -> Model
uniform numBytes = do
    gen <- newStdGen
    return $ take numBytes $ randoms gen

-- | Generates a random instruction stream based on the model function that is
-- passed in.
generate :: Model -> IO [Metadata]
generate model = do
    words <- model
    -- attempt to disassemble them
    let result = disassembleMetadata (intel32 {cfgSyntax = SyntaxIntel}) $ B.pack words 
    -- if successful return the instruction list, otherwise try again
    case result of
        []        -> generate model
        otherwise -> return result

