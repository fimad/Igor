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
    [currentIndex, lastIndex, tmpIndex, newVal, sortedVal, tmp] <- makeLocals 6
    --[loop_begin, inner_loop_begin, inner_loop_end, end] <- makeLabels 4
    --move    tmp                 (4 :: Integer)
    move    currentIndex        array
    move    tmp                 length
    mul     tmp                 length          tmp
--    add     lastIndex           currentIndex    length

--    label   loop_begin
--    set     tmp                 4
--    add     currentIndex        currentIndex    tmp
--    jump    end                 (currentIndex -==- lastIndex)
--    move    tmpIndex            currentIndex
--    load    newVal              currentIndex
--
--    label   inner_loop_begin
--    set     tmp                 4
--    jump    inner_loop_end      (tmpIndex -==- array)
--    sub     tmpIndex            tmpIndex        tmp
--    load    sortedVal           tmpIndex
--    add     tmpIndex            tmpIndex        tmp
--    jump    inner_loop_end      (sortedVal -<- newVal)
--    add     tmpIndex            tmpIndex        tmp
--    store   tmpIndex            sortedVal
--    set     tmp                 8
--    sub     tmpIndex            tmpIndex        tmp
--    jump    inner_loop_begin    always
--
--    label   inner_loop_end
--    store   tmpIndex            newVal
--
--    jump    loop_begin          always
--    label   end
