{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Igor.Gadget.Discovery
( 
-- * Types
  GadgetLibrary(..)
-- * Methods
, emptyLibrary
, libraryLookup
, libraryMerge
, libraryInsert
, discover
, discoverMore
, save
, load
) where

import              Codec.Compression.Zlib
import              Control.DeepSeq
import              Control.Monad
import qualified    Data.ByteString         as B
import              Data.ByteString.Lazy.Internal (ByteString)
import qualified    Data.ByteString.Lazy    as LB
import              Data.Binary
import              Data.DeriveTH
import              Data.List
import              Data.Foldable (foldr')
import qualified    Data.Map                as M
import qualified    Data.HashMap            as HM
import qualified    Data.Set                as S
import              Data.Maybe
import              Data.Random
import              Data.Random.RVar
import              Data.Tuple
import              Igor.Derive
import              Igor.ByteModel
import              Igor.Eval
import qualified    Igor.Gadget             as G
import              Hdis86.Pure
import              Hdis86.Types
import              System.Mem
import              System.Random

-- | A collection of sequences of bytecodes that correspond to specific gadgets.
newtype GadgetLibrary = GadgetLibrary {
        gadgetMap   :: M.Map G.Gadget (S.Set (B.ByteString, G.ClobberList))
    }
    deriving (Eq,Ord,Show,Read)
$( derive makeNFData ''GadgetLibrary )

instance Binary GadgetLibrary where
    put library@GadgetLibrary{..} = do
        let metadataSet     =   S.toList
                            $   S.unions 
                            -- $   S.toList 
                            -- $   S.map S.fromList 
                            -- $   S.unions 
                            $   map (S.map fst) 
                            $   M.elems gadgetMap
        let metadataToInt   = metadataSet `deepseq` M.fromList $ zip metadataSet [1::Int ..] 
        let intToBytes      = metadataToInt `deepseq` M.fromList $ map swap $ M.assocs metadataToInt
        let libraryWithInts = intToBytes `deepseq` M.map (S.map (\(m,c) -> (metadataToInt M.! m,c))) gadgetMap
        put $!! intToBytes
        put $!! libraryWithInts

    get = do
        !intToBytes          <- get :: Get (M.Map Int B.ByteString)
        !libraryWithInts     <- intToBytes `deepseq` get
        let library         = libraryWithInts `deepseq` M.map (S.map (\(m,c) -> (intToBytes M.! m,c))) libraryWithInts
        return $ GadgetLibrary $!! library :: Get GadgetLibrary

--------------------------------------------------------------------------------
-- GadgetLibrary operations
--------------------------------------------------------------------------------

save :: String -> GadgetLibrary -> IO ()
save file library = B.writeFile file . B.concat . LB.toChunks . encode $!! library

load :: String -> IO GadgetLibrary
load file = do
    library <- return . decode . LB.fromChunks . return =<< B.readFile file
    library `deepseq` performGC
    return $!! library

emptyLibrary :: GadgetLibrary
emptyLibrary = GadgetLibrary {
        gadgetMap   = M.empty
    }

-- | Wraps the look up function, and handles certain cases that may not be in
-- the library but can map to noops, like moving a register ontop of itself.
libraryLookup :: G.Gadget -> GadgetLibrary -> Maybe (S.Set (B.ByteString, G.ClobberList))
libraryLookup g@(G.LoadReg a b) library@GadgetLibrary{..}
    | a == b            = return $ S.singleton (B.empty,[])
    | otherwise         = M.lookup g gadgetMap
libraryLookup g library@GadgetLibrary{..}   = M.lookup g gadgetMap

libraryMerge :: GadgetLibrary -> GadgetLibrary -> GadgetLibrary
libraryMerge a b = GadgetLibrary $ M.unionWith S.union (gadgetMap a) (gadgetMap b)

libraryInsert :: G.Gadget -> (B.ByteString, G.ClobberList) -> GadgetLibrary -> GadgetLibrary
libraryInsert !gadget !(value,clobber) !library = GadgetLibrary 
                                                $! M.insertWith S.union gadget (S.singleton (B.copy value,clobber))
                                                $ gadgetMap library

-- | Given a target size and an instruction 'Metadata' 'Generator', builds a
-- library of gadgets of that is at least as large as the target size.
discover :: Int -> Generator -> StdGen -> GadgetLibrary
discover targetSize generator gen = discoverMore targetSize generator gen emptyLibrary

discoverMore :: Int -> Generator -> StdGen -> GadgetLibrary -> GadgetLibrary
discoverMore targetIncrease !generator gen library =  fst $ sampleState (discover' library) gen
    where
        targetSize = M.size (gadgetMap library) + targetIncrease

        discover' :: GadgetLibrary -> RVar GadgetLibrary
        discover' library =
            if M.size (gadgetMap library) >= targetSize
                then do
                    return library
                else do
                    !stream          <- generator >>= return . inits
                    --stream          <- generator >>= return . subsequences
                    let newLibrary  = foldr' (uncurry libraryInsert) library $!! concatMap process stream
                    newLibrary `seq` discover' newLibrary

        -- Turn an instruction stream into a list of key value pairs for the
        -- gadget library
        process :: [Metadata] -> [(G.Gadget,(B.ByteString,G.ClobberList))]
        process stream = do
            (gadget, clobber) <- concat $ maybeToList $ eval stream >>= return . G.match
            return (gadget, (B.concat $ map mdBytes stream,clobber))

