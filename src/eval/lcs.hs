{-# LANGUAGE TupleSections #-}
import              Data.List
import              Data.List.LCS
import qualified    Data.ByteString     as B
import              System.Environment
import              Text.Printf

main :: IO ()
main = do
    args        <- getArgs
    progName    <- getProgName
    case args of
        (file1:file2:_)     ->  do
            file1Method     <-  return . B.unpack =<< B.readFile file1
            file2Method     <-  return . B.unpack =<< B.readFile file2
            let subseq      =   lcs file1Method file2Method
            putStrLn $ show $ fromIntegral (length $ subseq) / fromIntegral (length file1Method + length file2Method - length subseq)
            return ()
        _                   ->
            putStrLn $ "Usage: "++progName++" file1.bin file2.bin"
