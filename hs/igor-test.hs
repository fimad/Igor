import              Data.Binary
import qualified    Data.ByteString as B
import qualified    Igor.Expr       as X
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
            --result  <- compile library output [("insertSort", insertSort)]
            result  <- compile library output [("factorial", factorial), ("insertSort", insertSort)]
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

factorial :: Program
factorial = do
    [n]                     <- makeInputs 1
    [tmp, total]            <- makeLocals 2
    [begin]                 <- makeLabels 1

    move    total   tmp

    move    total   (1 :: Integer)
    move    tmp     n

    label   begin
    mul     total   total   tmp
    sub     tmp     tmp     (1 :: Integer)
    jump    begin   ((1 :: Integer) -<- tmp)

    ret     total

insertSort :: Program
insertSort = do
    [array, length] <- makeInputs 2
    [currentIndex, tmpIndex, tmp] <- makeLocals 3
    [outer_loop, inner_loop] <- makeLabels 2

    move    currentIndex                (0 :: Integer)

    label   outer_loop
    move    tmpIndex                    currentIndex

    label   inner_loop
    move    tmp                         (array,tmpIndex,4,0)
--    move    (array,tmpIndex,4,0)        (array,currentIndex,4,0)       
--    move    (array,currentIndex,4,0)    tmp
    jump    inner_loop                  always

    add     currentIndex                currentIndex        (1 :: Integer)
    jump    outer_loop                  (currentIndex -<- length)
