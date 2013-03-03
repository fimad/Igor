module Igor.DSL
( 
-- ^ This module re-exports much of the `Igor.CodeGen` and defines the method
-- 'defineMethod' which handles taking a library file and a destination .o file
-- from command line parameters and compiles a given method.
  defineMethod
-- * Variables, Labels and Input
, makeInputs
, makeLocals
, makeLabels
, MemoryAccessType (..)
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
