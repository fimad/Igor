import              Data.Binary
import qualified    Data.ByteString as B
import              Igor.CodeGen
import              Igor.CodeGen.GCC
import              Igor.ByteModel (hdisConfig)
import              Igor.Gadget.Discovery
import              Hdis86
import              System.Random
import              System.Environment

main :: IO ()
main = do
    args        <- getArgs
    progName    <- getProgName
    case args of
        [libraryFile, output]   -> do
            putStrLn "Loading library..."
            library <- loadLibrary libraryFile
            putStrLn "Generating code..."
            result  <- compile library output [("factorial", factorial)]
            case result of
                False   -> putStrLn "Could not generate :("
                _       -> putStrLn $ "Written to "++output
            --gen     <- newStdGen
            --case generate library gen testProgram of
            --    Nothing     -> putStrLn "Could not generate :("
            --    Just result -> printBS $ byteCode result

        _                       -> putStrLn $ concat $ [progName, ": libraryFile output"]

    where
        printBS bs  = do
            let metaList    =  disassembleMetadata hdisConfig bs
            sequence_ $ map printMeta metaList
            
        printMeta m = do
            putStr $ show $ mdLength m
            putStr "\t:\t"
            putStrLn $ mdAssembly m

factorial :: PredicateProgram
factorial = do
    [n]             <- makeInputs 1
    [tmp,total,one] <- makeLocals 3
    [begin]         <- makeLabels 1
    set     one     1
    move    tmp     n
    move    total   one
    label   begin
    mul     total   total   tmp
    sub     tmp     tmp     one
    jump    begin   (tmp ->- one)
    ret     total

