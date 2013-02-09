import              Data.Binary
import              Igor.CodeGen
import              Igor.Gadget.Discovery
import              Hdis86
import              System.Random

main :: IO ()
main = do
    library <- decodeFile "library" :: IO GadgetLibrary
    gen     <- newStdGen
    case generate library gen testProgram of
        Nothing         -> putStrLn "Could not generate :("
        Just metaList   -> do
            sequence_ $ map printMeta metaList
    where
        printMeta m = do
            putStr $ show $ mdLength m
            putStr " :\t"
            putStrLn $ mdAssembly m

testProgram :: PredicateProgram
testProgram = do
    [v1,v2] <- makeVariables 2
    jump 3
    jump 1
    noop
    jump (-1)
    jump (-3)
