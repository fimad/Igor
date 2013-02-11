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
        [file] -> do
            fileExists      <- doesFileExist file
            existingLibrary <- if fileExists
                                    then do
                                        putStrLn "Reading existing library..."
                                        load file
                                    else
                                        return emptyLibrary
            putStrLn "Looking for gadgets..."
            gen             <- newStdGen
            let generator   = generate $ uniform 16
--            let newLibrary  = foldr1 merge $ map ($ generator) $ replicate 10 $ discover gen 10000
            let newLibrary  = discoverMore 10000 generator gen existingLibrary
            putStrLn "Working..."
            save file newLibrary
            putStrLn "Done!"
        _      ->
            putStrLn $ concat $ ["Usage: ", progName, " gadgetLibraryFile"]

