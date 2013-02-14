{-# LANGUAGE BangPatterns #-}
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Class
import              Data.Binary
import qualified    Data.ByteString             as B
import              Data.Enumerator
import qualified    Data.Foldable               as F
import qualified    Data.Map                    as M
import              Data.Ratio
import              Data.String
import              Data.Word
import              Filesystem.Enumerator
import qualified    Filesystem.Path             as P
import qualified    Filesystem.Path.CurrentOS   as OS
import              Igor.ByteModel

type ByteCount = M.Map Word8 Integer

builder :: ByteCount -> Iteratee P.FilePath IO ByteCount
builder !dist = do
    !maybePath <- Data.Enumerator.head
    case maybePath of
        Just path   -> do
            (lift $ process path dist) >>= builder
        Nothing     -> return $! dist
    where
        process :: P.FilePath -> ByteCount -> IO ByteCount
        process path !dist = do
            !dist   <- (B.readFile $ OS.encodeString path)
                    >>= return . B.foldr' (M.update (\a -> let b = a+1 in b `seq` Just b)) dist
            return dist

initialCounts = M.fromList $ zip [minBound .. maxBound] (Prelude.repeat 0)

main :: IO ()
main = do
    let enum            = traverse True (fromString "./")
    byteCount           <- run_ $ builder initialCounts >>== enum
    let total           = sum $ M.elems byteCount
    let byteDist        = SampledDistribution $ M.map (% total) byteCount
    sequence_ $ Prelude.map (putStrLn . show) $ M.assocs $ frequencies byteDist
    encodeFile "byte-dist" $ frequencies byteDist
    return ()
