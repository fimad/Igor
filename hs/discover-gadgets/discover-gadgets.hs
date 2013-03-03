import              Data.Binary
import              Igor.ByteModel
import              Igor.Gadget.Discovery
import              System.Directory
import              System.Environment
import              System.Mem
import              System.Random

main :: IO ()
main = do
    args        <- getArgs
    progName    <- getProgName
    case args of
        [file,"uniform"]        -> doDiscover file (IncreaseSizeBy 10000) $ uniform 16

        [file,"dist",distFile]  ->  decodeFile distFile 
                                >>= return . (fromDistribution 16)
                                >>= doDiscover file (IncreaseSizeBy 10000)

        [file,"scan",path]      ->  doDiscover file (UntilFailure) =<< fromFilePath 16 path

        _                       ->
            putStrLn $ concat $ [
                    "Usage: ", progName, " gadgetLibraryFile uniform\n"
                ,   "       ", progName, " gadgetLibraryFile dist distributionFile\n"
                ,   "       ", progName, " gadgetLibraryFile scan path/to/scan"
                ]

doDiscover :: FilePath -> StopCondition -> Source -> IO ()
doDiscover file stopCondition source = do
    let generator   = generate source
    fileExists      <- doesFileExist file
    existingLibrary <- if fileExists
                            then do
                                putStrLn "Reading existing library..."
                                loadLibrary file
                            else
                                return emptyLibrary
    putStrLn "Looking for gadgets..."
    newLibrary      <- discoverMore stopCondition generator existingLibrary
    putStrLn "Working..."
    saveLibrary file newLibrary
    putStrLn "Done!"
