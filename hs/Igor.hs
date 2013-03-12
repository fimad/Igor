-- | This module re-exports everything that is required to generate code using
-- an internal DSL. If you are interested in the implementation of anything, the
-- majority of the methods are lifted from `Igor.CodeGen` module.
--
-- The following is an example of how to write a simple function that takes two
-- arguments and returns the greater of the two.
--
--  > import Igor
--  > 
--  > main = defineMethod "max" $ do
--  >     [a, b] <- makeInputs 2
--  >     [aLessB] <- makeLabels 1
--  > 
--  >     jump    aLessB  (a -<- b)
--  >     ret     a
--  >     label   aLessB
--  >     ret     b
module Igor
( 
  defineMethod
-- * Variables, Labels and Input
, makeInputs
, makeLocals
, makeLabels
, MemoryAccessType (..)
, Statement
-- * Statements
, label
, move
, add
, sub
, xor
, mul
, noop
, ret
, jump
-- ** Jump Reasons
, (-<-)
, (-<=-)
, (->-)
, (->=-)
, (-==-)
, (-!=-)
, always
) where
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

-- | Defines a main method of a haskell program that will generate an gcc object
-- file that contains a method defined by the parameters passed to the function.
defineMethod :: String -> Program -> IO ()
defineMethod funcName program = do
    args        <- getArgs
    progName    <- getProgName
    case args of
        [libraryFile, output]   -> do
            putStrLn "Loading library..."
            library <- loadLibrary libraryFile
            putStrLn "Generating code..."
            result  <- compile library output [(funcName, program)]
            case result of
                False   -> putStrLn "Could not generate :("
                _       -> putStrLn $ "Written to "++output

        _                       -> putStrLn $ concat $ [progName, ": libraryFile output"]
