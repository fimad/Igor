import Control.Monad
import Data.Word
import Data.List
import System.Random
import Hdis86
import Hdis86.Types
import Igor
import qualified Data.ByteString as B

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
    putStr "-------------------------------\n"
    putStr . unlines $ zipWith (\a b -> a ++ "\n=> " ++ b) (map fst instructions) (map show results)
    putStr "\n"

