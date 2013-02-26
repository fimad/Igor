{-# LANGUAGE RecordWildCards #-}
module Igor.CodeGen.GCC
( 
-- * Methods
  compile
) where

import qualified    Data.ByteString as B
import              Data.Maybe
import              Igor.CodeGen
import              Igor.Gadget.Discovery
import              System.FilePath
import              System.Random

compile :: GadgetLibrary
        -> FilePath -- ^ Where to write the *.o file to
        -> [(String,PredicateProgram)]  -- ^ A list of functions to generate
        -> IO Bool -- ^ Was the file successfully generated?
compile library file methods = do
    maybeMethods        <- mapM compileMethod methods
    let compiledMethods = catMaybes maybeMethods
    -- Ensure that all of the methods could be generated
    if length compiledMethods == length maybeMethods
        then do
            let objContents = objTemplate file $ map objMethod compiledMethods
            writeFile file objContents
            return True
        else return False
    where
        compileMethod :: (String, PredicateProgram) -> IO (Maybe (String,B.ByteString,Integer))
        compileMethod (methodName,methodBody) = do
            gen                         <- newStdGen
            return $ do 
                GeneratedCode{..}       <- generate library gen methodBody
                return (methodName, byteCode, localVariableSize)

-- | Given a FilePath and a list of method definitions, fills in the AS
-- template.
objTemplate :: FilePath -> [String] -> String
objTemplate file methodStrings = "\t.file\t\"filename.igor\"\n\n" ++ unlines methodStrings

-- | Generate an AS method body for the given program with.
objMethod :: (String,B.ByteString,Integer) -> String
objMethod (method,body,localVarSize) =
    unlines [
            ".LC_"++method++":"
        ,   "\t.text"
        ,   "\t.globl\t"++method
        ,   "\t.type\t"++method++", @function"
        ,   method++":"
        ,   ".LFB_"++method++":"
        ,   "\tpush\t%ebp"
        ,   "\tpush\t%edi"
        ,   "\tpush\t%esi"
        ,   "\tpush\t%ebx"
        ,   "\tmov\t%esp, %ebp"
        ,   "\tsub\t$"++(show localVarSize)++", %esp"
        ]
    ++ bytesForBody
    ++ unlines [
            "\tadd\t$"++(show localVarSize)++", %esp"
        ,   "\tpop\t%ebx"
        ,   "\tpop\t%esi"
        ,   "\tpop\t%edi"
        ,   "\tpop\t%ebp"
        ,   "\tret"
        ,   ".LFE_"++method++":"
        ,   "\t.size\t"++method++", .-"++method
        ]
    where
        bytesForBody = unlines $ map (\b -> "\t.byte\t"++show b) $ B.unpack body
