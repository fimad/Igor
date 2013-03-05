{-# LANGUAGE BangPatterns #-}
import              Data.Binary
import qualified    Data.Map                as  M
import              Igor.ByteModel
import              Igor.Gadget.Discovery
import              System.Directory
import              System.Environment
import              System.Mem
import              System.Random
import              System.Exit

-- | Increase size in increments of this to avoid crazy space leaks... :(
sizeIncrements  =   10000

main :: IO ()
main = do
    args        <- getArgs
    progName    <- getProgName
    case args of
        [file,"uniform"]        -> doDiscover file (IncreaseSizeBy sizeIncrements) $ uniform 16

        [file,"dist",distFile]  ->  decodeFile distFile 
                                >>= return . (fromDistribution 16)
                                >>= doDiscover file (IncreaseSizeBy sizeIncrements)

        [file,"scan",path]      ->  doDiscover file (UntilFailure) =<< fromFilePath 16 path

        _                       ->
            putStrLn $ concat $ [
                    "Usage: ", progName, " gadgetLibraryFile uniform\n"
                ,   "       ", progName, " gadgetLibraryFile dist distributionFile\n"
                ,   "       ", progName, " gadgetLibraryFile scan path/to/scan"
                ]

doDiscover :: FilePath -> StopCondition -> Source -> IO ()
doDiscover file (IncreaseSizeBy x) source   =   mapM_ (doDiscover' file source . IncreaseSizeBy)
                                            -- Construct a list whose sum is x
                                            -- but each element is no bigger
                                            -- than sizeIncrements
                                            $   ((x `mod` sizeIncrements) :)
                                            $   take (x `div` sizeIncrements)
                                            $   repeat sizeIncrements
doDiscover file (TotalSizeIs x) source      =   mapM_ (doDiscover' file source . TotalSizeIs)
                                            -- Construct lists of conditions
                                            -- where the total size increases in
                                            -- increments of sizeIncrements
                                            $   reverse
                                            $   takeWhile (<0)
                                            $   zipWith (-) [x..] [0,sizeIncrements..]
doDiscover file (UntilFailure) source       =   mapM_ (doDiscover' file source . IncreaseSizeBy) 
                                            $   repeat sizeIncrements

-- | Does the actual grunt work of discovery and in the case of `UntilFailure`
-- will call exitWith to terminate the program (so we don't go on and on
-- forever).
doDiscover' file source stopCondition = do
    let generator   = generate source
    fileExists      <- doesFileExist file
    existingLibrary <- if fileExists
                            then do
                                putStrLn "Reading existing library..."
                                loadLibrary file
                            else
                                return emptyLibrary
    putStrLn "Looking for gadgets..."
    !newLibrary     <-  discoverMore stopCondition generator existingLibrary
    putStrLn "Working..."
    !_              <-  saveLibrary file newLibrary
    putStrLn "Finishing iteration..."
    -- | If we can discover no more gadgets (i.e. UntilFailure) then we should
    -- stop looking.
    if M.size (gadgetMap existingLibrary) == M.size (gadgetMap newLibrary)
        then exitSuccess
        else return $! ()

