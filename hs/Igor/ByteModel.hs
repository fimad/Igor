{-# LANGUAGE BangPatterns #-}
module Igor.ByteModel
( 
-- * Types
  Generator
, Model
-- * Methods
, uniform
, generate
) where

import              Control.DeepSeq
import              Codec.Compression.GZip
import qualified    Data.ByteString                     as B
import qualified    Data.Foldable                       as F
import              Data.Random.Distribution
import qualified    Data.Random.Distribution.Uniform    as U
import              Data.Random.RVar
import              Data.Random.Sample
import              Data.Random hiding (uniform)
import              Data.Word
import              Hdis86
import              Hdis86.Incremental
import              Hdis86.Types
import              Igor.Binary
import              System.Random

-- | A model randomly generates a finite list of bytes according to a given byte
-- distribution.
type Model     = IO B.ByteString

-- | A generator will generate a list of instruction 'Metadata'.
type Generator = IO [Metadata]

-- | Generates a random list of bytes from the uniform distribution.
-- TODO: Implement an actual model...
uniform :: Int -> Model
uniform numBytes = do
    let model       = U.stdUniform
    gen             <- newStdGen
    let (result,_)  = sampleState (sequence $ replicate numBytes model) gen
    return $! B.pack (result :: [Word8])

disassembleMetadata' config bytestring = reverse $! disas bytestring []
    where
        disas bs !meta = 
            case disassembleOne config bs of
                Nothing                 -> meta
                Just (newMeta,newBs)    -> newMeta `seq` disas newBs (newMeta:meta)

-- | Generates a random instruction stream based on the model function that is
-- passed in.
generate :: Model -> Generator
generate !model = do
    words       <- model
    -- attempt to disassemble them
    let !result  = disassembleMetadata (intel32 {cfgCPUMode = Mode32, cfgSyntax = SyntaxIntel}) $ words 
    -- if successful return the instruction list, otherwise try again
    case result of
        []        -> generate model
        otherwise -> return $! result

