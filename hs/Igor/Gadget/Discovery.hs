{-# LANGUAGE TemplateHaskell #-}
module Igor.Gadget.Discovery
( 
-- * Types
  GadgetLibrary
-- * Methods
, emptyLibrary
, merge
, discover
) where

import              Data.Binary
import              Data.DeriveTH
import              Data.List
import qualified    Data.Map        as M
import qualified    Data.Set        as S
import              Data.Maybe
import              Igor.ByteModel
import              Igor.Eval
import qualified    Igor.Gadget     as G
import              Hdis86.Types

type GadgetLibrary  = M.Map G.Gadget (S.Set ([Metadata], G.ClobberList))

$( derive makeBinary ''XMMRegister)
$( derive makeBinary ''X87Register)
$( derive makeBinary ''MMXRegister)
$( derive makeBinary ''DebugRegister)
$( derive makeBinary ''ControlRegister)
$( derive makeBinary ''Segment)
$( derive makeBinary ''Half)
$( derive makeBinary ''GPR)
$( derive makeBinary ''WordSize)
$( derive makeBinary ''Prefix)
$( derive makeBinary ''Immediate )
$( derive makeBinary ''Pointer )
$( derive makeBinary ''Register )
$( derive makeBinary ''Memory )
$( derive makeBinary ''Opcode )
$( derive makeBinary ''Operand )
$( derive makeBinary ''Instruction )
$( derive makeBinary ''Metadata )

emptyLibrary :: GadgetLibrary
emptyLibrary = M.empty

merge :: GadgetLibrary -> GadgetLibrary -> GadgetLibrary
merge a b = M.unionWith S.union a b

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
        insertIntoLibrary :: (G.Gadget,([Metadata],G.ClobberList)) -> GadgetLibrary -> GadgetLibrary
        insertIntoLibrary (gadget, value) library =
            M.insertWith (\a b -> S.union a b) gadget (S.singleton value) library

        -- Turn an instruction stream into a list of key value pairs for the
        -- gadget library
        process :: [Metadata] -> [(G.Gadget,([Metadata],G.ClobberList))]
        process stream = do
            (gadget, clobber) <- concat $ maybeToList $ eval stream >>= return . G.match
            return (gadget, (stream,clobber))

