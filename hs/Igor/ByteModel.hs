{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Igor.ByteModel
( 
-- * Types
  Generator
, Source
, SampledDistribution (..)
, ByteDistribution (..)
-- * Methods
, hdisConfig
, uniform
, fromDistribution
, fromFilePath
, generate
) where

import              Control.Applicative
import              Control.Concurrent.MVar
import              Control.DeepSeq
import              Control.Monad
import              Codec.Compression.GZip
import qualified    Data.ByteString                     as B
import              Data.Binary
import              Data.Char
import qualified    Data.Foldable                       as F
import              Data.IORef
import              Data.List
import qualified    Data.Map                            as M
import qualified    Data.IntervalMap.Interval           as IV
import qualified    Data.IntervalMap.Strict             as IM
import              Data.Ratio
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
import              System.Directory
import              System.FilePath

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

-- | A model randomly generates a finite list of bytes according to a given byte
-- distribution.
type Source     = IO (Maybe B.ByteString)

-- | A generator will generate a list of instruction 'Metadata'.
type Generator = IO (Maybe [Metadata])

-- | A distribution from a sample
data SampledDistribution a = SampledDistribution {
        -- Maybe use ratios instead of doubles here so that we can "guarantee" that
        -- they actually sum up to 1. Looks like the toRational function can take a
        -- double and make it the nearest rational, yay!!
        frequencies :: M.Map a Rational -- The normalized frequencies for each value of type
    ,   total       :: Integer -- Used when merging Samples
    ,   cdfMap      :: IM.IntervalMap Rational a
}

type ByteDistribution = SampledDistribution Word8

instance Distribution SampledDistribution a where
    rvarT dist    = do
        sample <- liftM toRational $ getRandomDouble
        return  $ snd
                $ head
                $ IM.containing (cdfMap dist) sample

instance (Binary a, Ord a) => Binary (SampledDistribution a) where
    put library@SampledDistribution{..} = do
        put total
        put frequencies

    get = do
        total       <- get
        frequencies <- get
        let cdfMap  = IM.fromList
                    $ tail
                    $ scanl ( \(int,_) (key,freq) ->
                                (IM.ClosedInterval (IV.upperBound int) (IV.upperBound int+freq) , key)
                            ) 
                            (IM.ClosedInterval 0 0,undefined)
                    $ M.assocs frequencies
        return $ SampledDistribution {
                frequencies = frequencies
            ,   total       = total
            ,   cdfMap      = cdfMap
            }

--------------------------------------------------------------------------------
-- Sources for sampling bytecode
--------------------------------------------------------------------------------

-- | Generates a random list of bytes from the uniform distribution.
uniform :: Int -> Source
uniform numBytes = do
    let model       = U.stdUniform
    gen             <- newStdGen
    let !(result,_)  = sampleState (sequence $ replicate numBytes model) gen
    return $! Just $ B.pack (result :: [Word8])

-- | Samples bytes from an observed byte frequency. Currently this is pretty
-- slow, at least compared to the uniform sampling.
fromDistribution :: Int -> ByteDistribution -> Source 
fromDistribution numBytes byteDist = do
    gen             <- newStdGen
    let !(result,_)  = sampleState (sequence $ replicate numBytes $ rvar byteDist) gen
    return $! Just $ B.pack (result :: [Word8])

-- | Implements the original authors original gadget discovery method.
fromFilePath :: Int -> FilePath -> IO Source
fromFilePath byteWindow filepath = do
    -- | This will allow us to keep state between calls to this Source. The
    -- state in this case is the remaining bytestring of the last file we read
    -- in and a list of the files that remain to be read in.
    stateRef    <-  newMVar (B.empty ,[filepath])
    return $ fromFilePath' stateRef 
    where
        fromFilePath' stateRef = do
            -- | Find out where we left off
            (bytes, filePaths)  <- takeMVar stateRef
            if not $ B.null bytes
                then do
                    -- | Write the updated state
                    putMVar stateRef (B.drop byteWindow bytes, filePaths)
                    return $ Just $ B.take byteWindow bytes
                else
                    -- | Check if there are remaining files to read in, if there
                    -- aren't return that we have reached the limit of this
                    -- source.
                    if null filePaths
                    then return Nothing
                    -- | Read in the next file and try the method again.
                    else do
                        let (nextFile:remainingPaths)   =   filePaths
                        -- | If the next filepath is a file, read it in and
                        -- recurse with the contents of the file
                        isFile                          <-  doesFileExist nextFile 
                        if isFile
                            then do
                                putStrLn $ "Reading file " ++ nextFile
                                contents    <- B.readFile nextFile
                                putMVar stateRef (contents, remainingPaths)
                                fromFilePath' stateRef
                            -- | If it is not a file it's a directory. So we
                            -- read it's contents, split it's contents into
                            -- files and directories. Filter the files by
                            -- extension and recurse with the new paths
                            else do
                                putStrLn $ "Descending into " ++ nextFile
                                dirContents         <-  mapM (return .(nextFile </>)) =<< getDirectoryContents nextFile
                                (files,dirs)        <-  partitionM doesFileExist dirContents
                                let executables     =   filter isExe files
                                let realDirs        =   filter (((&&) <$> (/=".") <*> (/="..")) . takeFileName) dirs
                                putMVar stateRef (B.empty, executables ++ realDirs ++ remainingPaths)
                                fromFilePath' stateRef

        -- | Does a file path end in '.exe' or ',dll' (case insensitive).
        isExe :: FilePath -> Bool
        isExe   = ((||) <$> (".exe"==) <*> (".dll"==))
                . map toLower
                . snd
                . splitExtension

        -- | There should really be a partitionM in Control.Monad...
        partitionM predicate []     = return ([],[])
        partitionM predicate (x:xs) = do
            (trues,falses)  <-  partitionM predicate xs
            isTrue          <-  predicate x 
            if isTrue   then return $! (x:trues, falses)
                        else return $! (trues, x:falses)
            

hdisConfig = intel32 {cfgSyntax = SyntaxNone}

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
generate :: Source -> Generator
generate !model = do
    !words       <- model
    case words of
        Just words  -> do
            -- attempt to disassemble them
            let !result  = disassembleMetadata hdisConfig $ words 
            -- if successful return the instruction list, otherwise try again
            case result of
                []        -> generate model
                otherwise -> return $! Just result
        Nothing     -> return Nothing

