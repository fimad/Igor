--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Igor.Gadget.Discovery
( 
-- * Types
  GadgetLibrary(..)
, StopCondition (..)
-- * Methods
, emptyLibrary
, libraryLookup
, libraryMerge
, libraryInsert
, discover
, discoverMore
, saveLibrary
, loadLibrary
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
import qualified    Data.IntMap             as IM
import qualified    Data.HashMap            as HM
import qualified    Data.Set                as S
import              Data.Maybe
import              Data.Random
import              Data.Random.RVar
import              Data.Tuple
import              Igor.ByteModel
import              Igor.Eval
import qualified    Igor.Gadget             as G
import              Hdis86.Pure
import              Hdis86.Types
import              System.Mem
import              System.Random
import              System.IO

data StopCondition  =   IncreaseSizeBy  Int
                    |   TotalSizeIs     Int
                    |   UntilFailure

-- | A collection of sequences of bytecodes that correspond to specific gadgets.
newtype GadgetLibrary = GadgetLibrary {
        gadgetMap   :: M.Map G.Gadget (S.Set (B.ByteString, G.ClobberList))
    }
    deriving (Eq,Ord,Show,Read)
-- $( derive makeNFData ''GadgetLibrary )

instance Binary GadgetLibrary where
    put library@GadgetLibrary{..} = do
        let metadataSet     =   S.toList
                            $   S.unions 
                            $   map (S.map fst) 
                            $   M.elems gadgetMap
        let metadataToInt   = M.fromList $ zip metadataSet [1::Int ..] 
        let intToBytes      = IM.fromList $ map swap $ M.assocs metadataToInt
        let libraryWithInts = M.map (S.map (\(m,c) -> (metadataToInt M.! m,c))) gadgetMap
        put $! intToBytes
        put $! libraryWithInts

    get = do
        intToBytes          <- get :: Get (IM.IntMap B.ByteString)
        libraryWithInts     <- get
        let library         =  M.map (S.map (\(m,c) -> (intToBytes IM.! m,c))) libraryWithInts
        return $ GadgetLibrary $ library :: Get GadgetLibrary

--------------------------------------------------------------------------------
-- GadgetLibrary operations
--------------------------------------------------------------------------------

saveLibrary :: String -> GadgetLibrary -> IO ()
saveLibrary file library = withFile file WriteMode $ \handle -> LB.hPut handle $ encode $ library

loadLibrary :: String -> IO GadgetLibrary
loadLibrary file = do
    library <- return . decode =<< LB.readFile file
    return $ library

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
libraryLookup g@(G.Jump 0 _ 0) library@GadgetLibrary{..}    = return $ S.singleton (B.empty,[])
libraryLookup g library@GadgetLibrary{..}                   = M.lookup g gadgetMap

libraryMerge :: GadgetLibrary -> GadgetLibrary -> GadgetLibrary
libraryMerge a b = GadgetLibrary $ M.unionWith S.union (gadgetMap a) (gadgetMap b)

libraryInsert :: G.Gadget -> (B.ByteString, G.ClobberList) -> GadgetLibrary -> GadgetLibrary
libraryInsert !gadget !(value,clobber) !library =   GadgetLibrary 
                                                $!  M.insertWith union' gadget (S.singleton (B.copy value,clobber))
                                                $   gadgetMap library
    where
        union' a b = let r = S.union a b in r `seq` r


-- | Given a target size and an instruction 'Metadata' 'Generator', builds a
-- library of gadgets of that is at least as large as the target size.
discover :: StopCondition -> Generator -> IO GadgetLibrary
discover stop generator = discoverMore stop generator emptyLibrary

-- | Handles the stop condition
discoverMore :: StopCondition -> Generator -> GadgetLibrary -> IO GadgetLibrary
discoverMore stop@UntilFailure                  !generator !library = discoverMore' stop generator library
discoverMore stop@(TotalSizeIs targetSize)      !generator !library
    | M.size (gadgetMap library) >= targetSize                      = return library
    | otherwise                                                     = discoverMore' stop generator library
discoverMore (IncreaseSizeBy targetIncrease)    !generator !library = discoverMore' stop generator library
    where
        originalSize    = M.size (gadgetMap library) 
        stop            = TotalSizeIs $ originalSize + targetIncrease
                
-- | Performs one iteration of the discovery
discoverMore' stop generator library = do
    --stream          <- generator >>= return . subsequences
    !stream         <- generator
    case stream of
        Nothing     -> return library
        Just stream -> do
            let streams     = inits stream
            let newLibrary  = foldr' (uncurry libraryInsert) library $! concatMap process streams
            newLibrary `seq` discoverMore stop generator newLibrary

    where
        -- Turn an instruction stream into a list of key value pairs for the
        -- gadget library
        process :: [Metadata] -> [(G.Gadget,(B.ByteString,G.ClobberList))]
        process stream = do
            let bytes           =   B.concat $ map mdBytes stream
            !(gadget, clobber)  <-  concat $ maybeToList $ eval stream >>= return . G.match (fromIntegral $ B.length bytes)
            return $! (gadget, (bytes ,clobber))

