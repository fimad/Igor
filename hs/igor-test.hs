import Control.Monad
import Data.Word
import Data.List
import System.Random
import Hdis86
import Hdis86.Types
import Igor
import qualified Data.ByteString as B
import qualified Data.Map as M

randomDisas :: Int -> IO [(String,Instruction)]
randomDisas numBytes = do
  gen <- newStdGen
  -- grab a list of random words numBytes long
  let randomWords = take numBytes $ randoms gen
  -- attempt to disassemble them
  let result = disassembleMetadata (intel32 {cfgSyntax = SyntaxIntel}) $ B.pack randomWords 
  -- if successful return the instruction list, otherwise try again
  case result of
    []        -> randomDisas numBytes
    otherwise -> return $ map (\m -> (mdAssembly m, mdInst m)) result

-- | A gadget that captures registers and constants being moved into a location
moveGadget :: MatchTarget
moveGadget = ExpressionVariable "source"

main :: IO ()
main = do
  -- | Grab a list of random instruction lists
  instructionsList <- replicateM 1000 $ randomDisas 16
  -- | Show all of them
  sequence_ $ map showInstructions instructionsList
  where
  -- Pretty print the instructions
  showInstructions instructions = do
    let partialInstructions = tail $ inits $ map snd instructions
    let results = map evalFold partialInstructions
    let results' = map (>>= (\s -> return (s, match moveGadget s))) results
    putStrLn "-------------------------------"
    sequence_ $ map prettyPrintResults $ zip (map fst instructions) results'
    putStr "\n"

  prettyPrintResults (instruction, Nothing) = do
    putStrLn instruction
  prettyPrintResults (instruction, Just (state,matches)) = do
    putStrLn instruction
    prettyPrintState state
    sequence_ $ map prettyPrintMatch matches

  prettyPrintState state = do
    putStrLn "\tState:"
    sequence_ $ map (\(l,v)-> putStrLn $ "\t\t"++(show l)++" <- "++(show v)) (M.toList state)

  prettyPrintMatch (location,varMap,clobbered) = do
    putStrLn "\tMatching: "
    putStrLn $ "\t\tLocation: " ++ (show location)
    putStrLn $ "\t\tVariables: " 
    sequence_ $ map (\(var,val) -> putStrLn $ "\t\t\t"++(show var)++" <- "++(show val)) $ M.toList varMap
    putStr "\t\tClobbering: "
    putStrLn $ unwords $ intersperse "," (map show clobbered)
