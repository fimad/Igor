import              Data.Binary
import              Igor.CodeGen
import              Igor.ByteModel (hdisConfig)
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
        Nothing     -> putStrLn "Could not generate :("
        Just result -> printBS result
    where
        printBS bs  = do
            let metaList    =  disassembleMetadata hdisConfig bs
            sequence_ $ map printMeta metaList
            
        printMeta m = do
            putStr $ show $ mdLength m
            putStr "\t:\t"
            putStrLn $ mdAssembly m

testProgram :: PredicateProgram
testProgram = do
    [v1,v2,v3,v4,v5,v6,v7] <- makeVariables 7
    jump 6
    move v1 v2
    move v2 v3
    move v3 v4
    move v5 v6
    move v6 v7
    jump (-6)
