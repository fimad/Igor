{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Class
import              Data.Binary
import qualified    Data.ByteString             as B
import              Data.Enumerator             hiding (map, head)
import qualified    Data.Enumerator.List        as EL
import qualified    Data.Foldable               as F
import qualified    Data.Map                    as M
import              Data.Ratio
import              Data.String
import              Data.Text (Text)
import              Data.Word
import              Filesystem.Enumerator 
import qualified    Filesystem.Path.CurrentOS   as OS
import              Igor.ByteModel
import              System.Environment

targetExtensions :: [Text]
targetExtensions = ["exe", "dll"]

type ByteCount = M.Map Word8 Integer

builder :: ByteCount -> Iteratee OS.FilePath IO ByteCount
builder !dist = do
    !maybePath <- EL.head
    case maybePath of
        Just path   -> do
            (lift $ process path dist) >>= builder
        Nothing     -> return $! dist
    where
        process :: OS.FilePath -> ByteCount -> IO ByteCount
        process path !dist = do
            if or $ map (OS.hasExtension path) targetExtensions
                then do
                    putStrLn $ show path
                    !dist   <- (B.readFile $ OS.encodeString path)
                            >>= return . B.foldr' (M.update (\a -> let b = a+1 in b `seq` Just b)) dist
                    return dist
                else
                    return dist

initialCounts = M.fromList $ zip [minBound .. maxBound] (Prelude.repeat 0)

main :: IO ()
main = do
    args    <- getArgs
    case args of
        [file, path]    -> do
            let enum            = traverse True (fromString path)
            byteCount           <- run_ $ builder initialCounts >>== enum
            let total           = sum $ M.elems byteCount
            let byteDist        = emptySample { 
                    frequencies = M.map (% total) byteCount
                ,   total = total
                }
            --sequence_ $ Prelude.map (putStrLn . show) $ M.assocs $ frequencies byteDist
            putStrLn "Done!"
            encodeFile file byteDist
        _               ->
            putStrLn "Usage: build-byte-dist byteFile path/to/crawl"

