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
        [file,"uniform"]        -> doDiscover file $ uniform 16

        [file,"dist",distFile]  ->  decodeFile distFile 
                                >>= return . (fromDistribution 16)
                                >>= doDiscover file

        _                       ->
            putStrLn $ concat $ [
                    "Usage: ", progName, " gadgetLibraryFile uniform\n"
                ,   "       ", progName, " gadgetLibraryFile dist distributionFile"
                ]

doDiscover :: FilePath -> Source -> IO ()
doDiscover file source = do
    let generator   = generate source
    fileExists      <- doesFileExist file
    existingLibrary <- if fileExists
                            then do
                                putStrLn "Reading existing library..."
                                load file
                            else
                                return emptyLibrary
    putStrLn "Looking for gadgets..."
    newLibrary      <- discoverMore 10000 generator existingLibrary
    putStrLn "Working..."
    save file newLibrary
    putStrLn "Done!"
