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
            --result  <- compile library output [("xorEnc", xorEnc)]
            result  <- compile library output [("insertSort", insertSort)]
            --result  <- compile library output [("factorial", factorial)]
            --result  <- compile library output [("factorial", factorial), ("insertSort", insertSort)]
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

insertSort :: Program
insertSort = do
    [array, length] <- makeInputs 2
    [arrayEnd, currentIndex, tmpIndex, tmp, tmp2, arrayFront] <- makeLocals 6
    [outer_loop, inner_loop] <- makeLabels 2

--    move    tmp         (0 :: Integer)
--    add     tmp         tmp   (R,array,0)
--    add     tmp         tmp   (R,array,4)
--    move    (W,array,8) tmp

    move    currentIndex        array
    mul     arrayEnd            length              (4 :: Integer)
    add     arrayEnd            arrayEnd            array
    sub     arrayFront          array               (4 :: Integer)

    label   outer_loop
    move    tmpIndex            currentIndex

    label   inner_loop
--    move    tmp                 (R,tmpIndex,0)
--    move    (W,tmpIndex,0)      (R,tmpIndex,-4)
    move    (W,tmpIndex,-4)     tmp
    sub     tmpIndex            tmpIndex            (4 :: Integer)
--    jump    inner_loop          (tmpIndex -!=- arrayFront)

    add     currentIndex        currentIndex        (4 :: Integer)
    jump    outer_loop          (currentIndex -!=- arrayEnd)


    --
    --
    --
    --
    --
--    move    currentIndex                (0 :: Integer)
--
--    label   outer_loop
--    move    tmpIndex                    currentIndex
--
--    label   inner_loop
--    move    tmp                         (R,array,tmpIndex,4,0)
----    move    (W,array,tmpIndex,4,0)      (R,array,currentIndex,4,0)       
--    move    (W,array,currentIndex,4,0)  tmp
--    sub     tmpIndex                    tmpIndex            (1 :: Integer)
--    jump    inner_loop                  always
--
--    add     currentIndex                currentIndex        (1 :: Integer)
--    jump    outer_loop                  (currentIndex -<- length)
--
