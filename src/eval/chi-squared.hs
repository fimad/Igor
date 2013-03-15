{-# LANGUAGE TupleSections #-}
import              Control.Arrow
import              Control.Monad
import              Data.Binary
import qualified    Data.ByteString     as B
import              Data.Foldable (foldr')
import qualified    Data.Map            as M
import              Data.Ratio
import              Igor.ByteModel
import              System.Environment
import              Text.Printf

type Dist = (Integer,M.Map Word8 Integer)

main :: IO ()
main = do
    args        <- getArgs
    progName    <- getProgName
    case args of
        (byteModelFile:x:xs)    -> do
            let objectFiles     =   x:xs
            byteModel           <-  decodeFile byteModelFile
            fileDists           <-  mapM fileToDist objectFiles
            let (total,counts)  =   foldr' combineDists (0,M.empty) fileDists
            --Actually calculate X^2
            let observedDist    =   M.map (%1) counts
            let expectedDist    =   M.map (total%1 *) $ frequencies byteModel
            let chi2            =   sum $ M.elems
                                $   M.unionWith (\o e -> ((o-e)^2)/e) observedDist expectedDist
            printf "%.2f\n" (fromRational chi2 :: Float)
        _                       ->
            putStrLn $ "Usage: "++progName++" bytemodel file-000.o file-001.o ..."

    where 
        -- Combining two distributions is the same as adding each individual
        -- count and adding the total counts. For now distributions are stored
        -- as tuples (total, Map of counts)
        combineDists :: Dist -> Dist -> Dist
        --combineDists        =   return . (\(a,b) -> (+a) *** M.unionWith (+) b )
        combineDists (a,m) (b,n) =   (a+b, M.unionWith (+) m n)
        
        -- Reads a file from disk and counts the number of bytes
        fileToDist :: FilePath -> IO Dist
        fileToDist filePath =   return
                            .   (sum . M.elems &&& id)
                            .   foldr' (uncurry $ M.insertWith (+)) M.empty
                            .   map (,1)
                            .   B.unpack
                            =<< B.readFile filePath

