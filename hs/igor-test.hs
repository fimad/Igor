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
            sequence_ $ map (putStrLn . mdAssembly) metaList

testProgram :: PredicateProgram
testProgram = do
    [v1,v2,v3] <- makeVariables 3
    move v1 v2
    move v1 v3
    move v3 v1
    add v1 v2 v1
    add v2 v2 v3
