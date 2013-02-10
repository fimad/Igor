{-# LANGUAGE TemplateHaskell #-}
module Igor.Gadget.Discovery
( 
-- * Types
  GadgetLibrary
-- * Methods
, emptyLibrary
, libraryLookup
, merge
, discover
, save
, load
) where

import              Codec.Compression.GZip
import              Control.Monad
import qualified    Data.ByteString         as B
import qualified    Data.ByteString.Lazy    as LB
import              Data.Binary
import              Data.DeriveTH
import              Data.List
import              Data.Foldable (foldr')
import qualified    Data.Map        as M
import qualified    Data.Set        as S
import              Data.Maybe
import              Data.Random
import              Data.Random.RVar
import              Igor.ByteModel
import              Igor.Eval
import qualified    Igor.Gadget     as G
import              Hdis86.Types
import              System.Random

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

save :: String -> GadgetLibrary -> IO ()
save file library = LB.writeFile file . compress . encode $ library

load :: String -> IO GadgetLibrary
load file = return . decode . decompress . LB.pack . B.unpack =<< B.readFile file

emptyLibrary :: GadgetLibrary
emptyLibrary = M.empty

-- | Wraps the look up function, and handles certain cases that may not be in
-- the library but can map to noops, like moving a register ontop of itself.
libraryLookup :: G.Gadget -> GadgetLibrary -> Maybe (S.Set ([Metadata], G.ClobberList))
libraryLookup g@(G.LoadReg a b) library
    | a == b            = return $ S.singleton ([],[])
    | otherwise         = M.lookup g library
libraryLookup g library = M.lookup g library

merge :: GadgetLibrary -> GadgetLibrary -> GadgetLibrary
merge a b = M.unionWith S.union a b

-- | Given a target size and an instruction 'Metadata' 'Generator', builds a
-- library of gadgets of that is at least as large as the target size.
discover :: StdGen -> Int -> Generator -> GadgetLibrary
discover gen targetSize generator = fst $ sampleState (discover' M.empty) gen
    where
        discover' :: GadgetLibrary -> RVar GadgetLibrary
        discover' library =
            if M.size library >= targetSize
                then do
                    return library
                else do
                    stream          <- generator >>= return . inits
                    let newLibrary  = foldr' insertIntoLibrary library $ concatMap process stream
                    newLibrary `seq` discover' newLibrary
                    --discover' newLibrary

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

