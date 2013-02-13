{-# LANGUAGE BangPatterns #-}
module Igor.ByteModel
( 
-- * Types
  Generator
, Model
-- * Methods
, hdisConfig
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
import              Data.Word
import              Hdis86
import              Hdis86.Incremental
import              Hdis86.Types
import              Igor.Derive

-- | A model randomly generates a finite list of bytes according to a given byte
-- distribution.
type Model     = RVar B.ByteString

-- | A generator will generate a list of instruction 'Metadata'.
type Generator = RVar [Metadata]

hdisConfig = intel32 {cfgSyntax = SyntaxIntel}

-- | Generates a random list of bytes from the uniform distribution.
-- TODO: Implement an actual model...
uniform :: Int -> Model
uniform numBytes = do
    let !model   = U.stdUniform
    !result      <- sequence $ replicate numBytes model
    return $! B.pack result

disassembleMetadata' config bytestring = reverse $! disas bytestring []
    where
        disas bs !meta = 
            case disassembleOne config bs of
                Nothing                 -> meta
                Just (newMeta,newBs)    -> (newMeta,newBs) `deepseq` disas newBs (newMeta:meta)

-- | Generates a random instruction stream based on the model function that is
-- passed in.
generate :: Model -> Generator
generate !model = do
    !words       <- model
    -- attempt to disassemble them
    let !result  = disassembleMetadata' hdisConfig $ words 
    -- if successful return the instruction list, otherwise try again
    case result of
        []        -> generate model
        otherwise -> return $! result

