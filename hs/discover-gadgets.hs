import              Data.Binary
import              Data.Map
import              Igor.ByteModel
import              Igor.Gadget.Discovery
import              System.Directory
import              System.Environment
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
            let newLibrary  = discover gen 50000 $ generate $ uniform 16
            mergedLibrary   <- if existingLibrary /= emptyLibrary
                                    then do
                                        putStrLn "Adding newly discovered gadgets..."
                                        return $ existingLibrary `merge` newLibrary
                                    else
                                        return newLibrary
            putStrLn "Writing to file..."
            --encodeFile file mergedLibrary
            save file mergedLibrary
            putStrLn "Done!"
        _      ->
            putStrLn $ concat $ ["Usage: ", progName, " gadgetLibraryFile"]

