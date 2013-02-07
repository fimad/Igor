import              Control.Monad.State
import              Data.Binary
import qualified    Data.ByteString.Char8 as BS
import qualified    Data.Map        as M
import qualified    Data.Set        as S
import              Igor.CodeGen
import              Igor.Gadget.Discovery
import              Hdis86
import              Hdis86.Types

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
    move v2 v2
    move v3 v1
    add v1 v2 v1
