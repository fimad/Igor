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
    let library     = discover gen 10000 generator
    sequence_ $ map prettyPrint $ M.toList library

    where
        prettyPrint (gadget, instances) = do
            putStrLn $ "Gadget: " ++ show gadget
            sequence_ $ map prettyPrint' $ S.toList instances

        prettyPrint' (meta, clobbered) = do
            putStr $ unlines . map (("\t"++) . mdAssembly) $ meta
            putStr $ unlines . map (("\t"++) . mdHex) $ meta
            putStrLn $ "\tClobbered: " ++ show clobbered
            putStrLn $ "\t" ++ replicate 40 '-' ++ "\n"
