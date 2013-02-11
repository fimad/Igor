import              Data.Binary
import              Igor.CodeGen
import              Igor.Gadget.Discovery
import              Hdis86
import              System.Random

main :: IO ()
main = do
    putStrLn "Loading library..."
    library <- load "library"
    putStrLn "Generating code..."
    gen     <- newStdGen
    case generate library gen testProgram of
        Nothing         -> putStrLn "Could not generate :("
        Just metaList   -> do
            sequence_ $ map printMeta metaList
    where
        printMeta m = do
            putStr $ show $ mdLength m
            putStr "\t:\t"
            putStrLn $ mdAssembly m

testProgram :: PredicateProgram
testProgram = do
    [v1,v2] <- makeVariables 2
--    jump 4
--    jump 2
    move v1 v2
--    jump (-2)
--    jump (-4)
