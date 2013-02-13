import              Hdis86
import              Hdis86.Types
import qualified    Data.Map        as M
import qualified    Data.Set        as S
import              Igor
import              Igor.ByteModel
import              Igor.Gadget
import              Igor.Gadget.Discovery
import              System.Random

main :: IO ()
main = do
    gen             <- newStdGen
    let generator   = generate $ uniform 16
    let library  = discover 10000 generator gen
    sequence_ $ map prettyPrint $ M.toList $ gadgetMap library

    where
        prettyPrint (gadget, instances) = do
            putStrLn $ "Gadget: " ++ show gadget
            sequence_ $ map prettyPrint' $ S.toList instances

        prettyPrint' (meta, clobbered) = do
            printBS meta
            putStrLn $ "\tClobbered: " ++ show clobbered
            putStrLn $ "\t" ++ replicate 40 '-' ++ "\n"
            
        printBS bs  = do
            let metaList    =  disassembleMetadata hdisConfig bs
            sequence_ $ map printMeta metaList

        printMeta m = do
            putStr $ show $ mdLength m
            putStr "\t:\t"
            putStr $ show $ mdHex m
            putStr "\t:\t"
            putStrLn $ mdAssembly m
