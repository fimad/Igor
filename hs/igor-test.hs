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
    [v1,v2,v3]  <- makeVariables 3
    --start       <- makeLabel
    move v1 v2
    move v2 v3
    start       <- makeLabel
    jump start (v1 ->- v3)
