import              Data.Binary
import              Igor.CodeGen
import              Igor.Gadget.Discovery
import              Hdis86

main :: IO ()
main = do
    library <- decodeFile "library" :: IO GadgetLibrary
    case generate library testProgram of
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
    [v1,v2,v3] <- makeVariables 3
    noop
    move v1 v2
    noop
    move v1 v3
    noop
    move v3 v1
    noop
    add v1 v2 v1
    noop
    add v2 v2 v3
    noop
    jump (-4)
