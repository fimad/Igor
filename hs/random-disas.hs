import Control.Monad
import Data.Word
import System.Random
import Text.ParserCombinators.Parsec.Error
import Text.Disassembler.X86Disassembler

randomDisas :: Int -> IO [Instruction]
randomDisas numBytes = do
  gen <- newStdGen
  let randomWords = take numBytes $ randoms gen
  result <- disassembleList randomWords 
  case result of
    (Right instructions) -> return instructions
    (Left errors) -> do
      --putStr $ unlines $ (map messageString $ errorMessages errors)
      randomDisas numBytes

main :: IO ()
main = do
  instructionsList <- replicateM 1000 $ randomDisas 16
  sequence_ $ map showInstructions instructionsList
  where
  showInstructions instructions = do
    putStr "-------------------------------\n"
    putStr . unlines $ map showAtt instructions
    putStr "\n"

