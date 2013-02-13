{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Igor.ByteModel
( 
-- * Types
  Generator
, Model
, SampledDistribution (..)
, ByteDistribution (..)
-- * Methods
, hdisConfig
, uniform
, generate
) where

import              Control.DeepSeq
import              Control.Applicative
import              Codec.Compression.GZip
import qualified    Data.ByteString                     as B
import qualified    Data.Foldable                       as F
import qualified    Data.Map                            as M
import              Data.Random.Distribution
import qualified    Data.Random.Distribution.Uniform    as U
import              Data.Random.RVar
import              Data.Random.Sample
import              Data.Random.Source
import              Data.Random hiding (uniform)
import              Data.Word
import              Hdis86
import              Hdis86.Incremental
import              Hdis86.Types
import              System.Random

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

-- | A model randomly generates a finite list of bytes according to a given byte
-- distribution.
type Model     = IO B.ByteString

-- | A generator will generate a list of instruction 'Metadata'.
type Generator = IO [Metadata]

-- | A distribution from a sample
data SampledDistribution a = SampledDistribution {
    frequencies :: M.Map a Double -- The normalized frequencies for each value of type
}

type ByteDistribution = SampledDistribution Word8

instance Distribution SampledDistribution a where
    rvarT dist    = do
        sample <- getRandomDouble
        return $
            ( fst 
            . head
            . dropWhile ((sample>). snd)
            . scanr (\a -> (,) <$> fst <*> (snd a+).snd) (undefined,0)
            . M.assocs
            . frequencies
            ) dist

--------------------------------------------------------------------------------
-- Models for sampling bytecode
--------------------------------------------------------------------------------

-- | Generates a random list of bytes from the uniform distribution.
-- TODO: Implement an actual model...
uniform :: Int -> Model
uniform numBytes = do
    let model       = U.stdUniform
    gen             <- newStdGen
    let !(result,_)  = sampleState (sequence $ replicate numBytes model) gen
    return $! B.pack (result :: [Word8])

fromDistribution :: ByteDistribution -> Int -> Model 
fromDistribution byteDist numBytes = do
    gen             <- newStdGen
    let !(result,_)  = sampleState (sequence $ replicate numBytes $ rvar byteDist) gen
    return $! B.pack (result :: [Word8])

hdisConfig = intel32 {cfgSyntax = SyntaxIntel}

--------------------------------------------------------------------------------
-- The assembly generator
--------------------------------------------------------------------------------

disassembleMetadata' config bytestring = reverse $! disas bytestring []
    where
        disas bs !meta = 
            case disassembleOne config bs of
                Nothing                 -> meta
                Just (newMeta,newBs)    -> (newMeta,newBs) `seq` disas newBs (newMeta:meta)

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

