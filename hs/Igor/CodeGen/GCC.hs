{-# LANGUAGE RecordWildCards #-}
module Igor.CodeGen.GCC
( 
-- * Methods
  createObjFile
) where

import qualified    Data.ByteString as B
import              Igor.CodeGen
import              Igor.Gadget.Discovery
import              System.FilePath
import              System.Random

createObjFile   :: FilePath -- ^ Where to write the *.o file to
                -> [(String,PredicateProgram)]  -- ^ A list of functions to generate
                -> GadgetLibrary
                -> IO Bool -- ^ Was the file successfully generated?
createObjFile file methods library = do
    gen     <- newStdGen
    return False
    --generate

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
        ,   "\t.glob\t"++method
        ,   "\t.type\t"++method++", @function"
        ,   method++":"
        ,   ".LFB_"++method++":"
        ,   "\tpushq\t%rbp"
        ,   "\tmovq\t%rsp, %rbp"
        ,   "\tsubq\t$"++(show localVarSize)++", %rsp"
        ]
    ++ bytesForBody
    ++ unlines [
            "\tleave"
        ,   "\tret"
        ,   ".LFE_"++method++":"
        ,   "\t.size\t"++method++", .-"++method
        ]
    where
        bytesForBody = unlines $ map (\b -> "\t.byte\t"++show b) $ B.unpack body
