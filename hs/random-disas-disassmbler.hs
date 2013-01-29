import Control.Monad
import Data.Word
import System.Random
import Text.ParserCombinators.Parsec.Error
import Text.Disassembler.X86Disassembler

randomDisas :: Int -> IO [Instruction]
randomDisas numBytes = do
  gen <- newStdGen
  -- grab a list of random words numBytes long
  let randomWords = take numBytes $ randoms gen
  -- attempt to disassemble them
  result <- disassembleList randomWords 
  -- if successful return the instruction list, otherwise try again
  case result of
    (Right instructions) -> return instructions
    (Left errors) -> do
      --putStr $ unlines $ (map messageString $ errorMessages errors)
      randomDisas numBytes

main :: IO ()
main = do
  -- | Grab a list of random instruction lists
  instructionsList <- replicateM 1000 $ randomDisas 16
  -- | Show all of them
  sequence_ $ map showInstructions instructionsList
  where
  -- Pretty print the instructions
  showInstructions instructions = do
    putStr "-------------------------------\n"
    putStr . unlines $ map showIntel instructions
    putStr "\n"

