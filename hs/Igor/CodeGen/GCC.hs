{-# LANGUAGE RecordWildCards #-}
module Igor.CodeGen.GCC
( 
-- * Methods
  createObjFile
) where

import Igor.CodeGen

createObjFile   :: FilePath -- ^ Where to write the *.o file to
                -> [(String,PredicateProgram)]  -- ^ A list of functions to generate
                -> IO Bool -- ^ Was the file successfully generated?
createObjFile _ _ = return False
