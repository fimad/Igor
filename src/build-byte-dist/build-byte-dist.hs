{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import              Control.DeepSeq
import              Control.Exception           as EX
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Class
import              Data.Binary
import qualified    Data.ByteString             as B
import              Data.DeriveTH
import              Data.Enumerator             hiding (map, head)
import qualified    Data.Enumerator.List        as EL
import qualified    Data.Foldable               as F
import qualified    Data.Map                    as M
import              Data.Pecoff
import              Data.Ratio
import              Data.String
import              Data.Text (Text)
import              Data.Word
import              Hdis86.Pure
import              Hdis86.Types
import              Filesystem.Enumerator 
import qualified    Filesystem.Path.CurrentOS   as OS
import              Igor.ByteModel
import              System.Environment

$( derive makeNFData ''Opcode )

targetExtensions :: [Text]
targetExtensions = ["exe", "dll"]

type ByteCount = M.Map Word8 Integer
type OpcodeCount = M.Map Opcode Integer

builder :: ByteCount -> OpcodeCount -> Iteratee OS.FilePath IO (ByteCount, OpcodeCount)
builder !byteDist !opcodeDist = do
    !maybePath <- EL.head
    case maybePath of
        Just path   -> do
            (lift $ process path byteDist opcodeDist) >>= uncurry builder
        Nothing     -> return $! (byteDist, opcodeDist)
    where
        process :: OS.FilePath -> ByteCount -> OpcodeCount -> IO (ByteCount, OpcodeCount)
        process path !byteDist !opcodeDist = do
            if or $ map (OS.hasExtension path) targetExtensions
                then do
                    putStrLn $ show path
                    contents        <-  B.readFile $ OS.encodeString path
                    -- The PE/COFF parser doesn't have a nicer way of handling
                    -- unsupported file types...
                    flip EX.catch (\e -> putStrLn (show (e :: IOException)) >> return (byteDist,opcodeDist)) $
                        -- Try to find the .text section of the file
                        case Prelude.filter ((".text"==).psectName) $! pSections $! parsePecoff contents of
                            (!textSection:_)     ->  do   
                                let textBytes    =   psectRawData textSection
                                let opcodes      =   map inOpcode $! disassemble hdisConfig textBytes
                                let opcodeDist'  =   opcodes `deepseq` F.foldr' updateDist opcodeDist opcodes
                                let byteDist'    =   B.foldr' updateDist byteDist textBytes
                                return $! (byteDist',opcodeDist')
                            _                   ->  do
                                return $! (byteDist,opcodeDist)
                    
                else
                    return $! (byteDist,opcodeDist)
        
        updateDist :: (Ord k) => k -> M.Map k Integer -> M.Map k Integer
        updateDist = (M.update (\a -> let b = a+1 in b `seq` Just b))

initialByteCounts = M.fromList $! zip [minBound .. maxBound] (Prelude.repeat 0)
initialOpcodeCounts = M.fromList $! zip [minBound .. maxBound] (Prelude.repeat 0)

main :: IO ()
main = do
    args    <- getArgs
    case args of
        [byteFile, opcodeFile, path]    ->  do
            let enum                =   traverse True (fromString path)
            (byteCount,opcodeCount) <-  run_ $ builder initialByteCounts initialOpcodeCounts >>== enum
            let byteTotal           =   sum $ M.elems byteCount
            let opcodeTotal         =   sum $ M.elems opcodeCount
            let byteDist            =   emptySample { 
                    frequencies     =   M.map (% byteTotal) byteCount
                ,   total           =   byteTotal
                }
            let opcodeDist          =   emptySample { 
                    frequencies     =   M.map (% opcodeTotal) opcodeCount
                ,   total           =   opcodeTotal
                }
            --sequence_ $ Prelude.map (putStrLn . show) $ M.assocs $ frequencies byteDist
            putStrLn "Done!"
            encodeFile byteFile byteDist
            encodeFile opcodeFile opcodeDist
        _               ->
            putStrLn "Usage: build-byte-dist byteFile opcodeFile path/to/crawl"

