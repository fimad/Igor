module Igor.Gadget.Discovery
( 
-- * Types
  GadgetLibrary
-- * Methods
, discover
) where

import              Control.Applicative
import              Data.List
import qualified    Data.Map as     M
import qualified    Data.Set as     S
import              Data.Maybe
import              Igor.ByteModel
import              Igor.Eval
import              Igor.Gadget
import              Hdis86.Types

type GadgetLibrary = M.Map Gadget (S.Set ([Metadata], ClobberList))

-- | Given a target size and an instruction 'Metadata' 'Generator', builds a
-- library of gadgets of that is at least as large as the target size.
discover :: Int -> Generator -> IO GadgetLibrary
discover targetSize generator = discover' M.empty
    where
        discover' :: GadgetLibrary -> IO GadgetLibrary
        discover' library = if M.size library >= targetSize
            then do
                return library
            else do
                stream <- generator >>= return . inits
                let newLibrary = foldr insertIntoLibrary library $ concatMap process stream
                discover' newLibrary

        -- Insert something produced by process into a gadget library
        insertIntoLibrary :: (Gadget,([Metadata],ClobberList)) -> GadgetLibrary -> GadgetLibrary
        insertIntoLibrary (gadget, value) library =
            M.insertWith (\a b -> S.union a b) gadget (S.singleton value) library

        -- Turn an instruction stream into a list of key value pairs for the
        -- gadget library
        process :: [Metadata] -> [(Gadget,([Metadata],ClobberList))]
        process stream = do
            (gadget, clobber) <- concat $ maybeToList $ eval stream >>= return . match
            return (gadget, (stream,clobber))

