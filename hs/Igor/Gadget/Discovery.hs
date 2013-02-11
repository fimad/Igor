{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
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
import              Igor.Binary
import              Igor.ByteModel
import              Igor.Eval
import qualified    Igor.Gadget             as G
import              Hdis86.Types
import              System.Mem
import              System.Random

-- | A collection of sequences of bytecodes that correspond to specific gadgets.
data GadgetLibrary = GadgetLibrary {
        gadgetMap   :: M.Map G.Gadget (S.Set ([Int], G.ClobberList))
        -- | The reason for the indirection here is that, while discovering
        -- gadgets, if multiple gadgets use the same meta data they share the
        -- instance in memory. The binary library does not preserve this quality
        -- during serialization and it leads to an explosion in the amount of
        -- memory required to keep the library in memory.
    ,   intToMeta   :: M.Map Int Metadata
    ,   metaToInt   :: M.Map Metadata Int
    ,   maxIndex    :: Int
    }
    deriving (Eq,Ord,Show,Read)
$( derive makeBinary ''GadgetLibrary )

save :: String -> GadgetLibrary -> IO ()
--save file library = B.writeFile file . B.concat . LB.toChunks . compress . encode $ library
save file library = LB.writeFile file . compress . encode $! library
--save file library = B.writeFile file . B.concat . LB.toChunks . encode $! library

load :: String -> IO GadgetLibrary
load file = do
--    library <- return . decode . decompress . LB.fromChunks . return  =<< B.readFile file
    byteString <- return . decompress . LB.fromChunks . return  =<< B.readFile file
    let library = LB.length byteString `seq` decode byteString
    return $! library

emptyLibrary :: GadgetLibrary
emptyLibrary = GadgetLibrary {
        gadgetMap   = M.empty
    ,   metaToInt   = M.empty
    ,   intToMeta   = M.empty
    ,   maxIndex    = 0
    }

-- | Wraps the look up function, and handles certain cases that may not be in
-- the library but can map to noops, like moving a register ontop of itself.
libraryLookup :: G.Gadget -> GadgetLibrary -> Maybe (S.Set ([Metadata], G.ClobberList))
libraryLookup g@(G.LoadReg a b) library@GadgetLibrary{..}
    | a == b            = return $ S.singleton ([],[])
    | otherwise         = return . S.map (\(m,c) -> (map (intToMeta M.!) m, c)) =<< M.lookup g gadgetMap
libraryLookup g library@GadgetLibrary{..}   =
    return . S.map (\(m,c) -> (map (intToMeta M.!) m, c)) =<< M.lookup g gadgetMap

libraryMerge :: GadgetLibrary -> GadgetLibrary -> GadgetLibrary
libraryMerge a b 
    | M.size (gadgetMap a) > M.size (gadgetMap b)   = a `libraryMerge'` b
    | otherwise                                     = b `libraryMerge'` a
    where
        libraryMerge' big small@GadgetLibrary{..} = foldr' (uncurry libraryInsert) big allSmallValues
            where
                allSmallGadgets = (M.keys gadgetMap)
                allSmallValues  = 
                        S.toList 
                    $   S.unions 
                    $   zipWith (\g v -> S.map (\v -> (g,v)) v) allSmallGadgets
                    $   map (fromJust . flip libraryLookup small) allSmallGadgets

libraryInsert :: G.Gadget -> ([Metadata], G.ClobberList) -> GadgetLibrary -> GadgetLibrary
libraryInsert gadget (metaList,clobberList) library@GadgetLibrary{..} =
    let
        (newMetaToInt,newIntToMeta,newMaxIndex,newMetaList)
            = foldr' intForMeta (metaToInt,intToMeta,maxIndex,[]) metaList
    in
        library {
                gadgetMap   = M.insertWith S.union gadget (S.singleton (newMetaList,clobberList)) gadgetMap
            ,   metaToInt   = newMetaToInt
            ,   intToMeta   = newIntToMeta
            ,   maxIndex    = newMaxIndex
            }
    where
        intForMeta metadata (metaToInt,intToMeta,maxIndex,metaList)
            | M.member metadata metaToInt   =
                    (   metaToInt
                    ,   intToMeta
                    ,   maxIndex
                    ,   (metaToInt M.! metadata):metaList
                    )
            | otherwise                     =  
                   (    M.insert metadata (maxIndex+1) metaToInt
                   ,    M.insert (maxIndex+1) metadata intToMeta
                   ,    maxIndex+1
                   ,    (maxIndex+1):metaList
                   )

-- | Given a target size and an instruction 'Metadata' 'Generator', builds a
-- library of gadgets of that is at least as large as the target size.
discover :: Int -> Generator -> StdGen -> GadgetLibrary
discover targetSize generator gen = discoverMore targetSize generator gen emptyLibrary

discoverMore :: Int -> Generator -> StdGen -> GadgetLibrary -> GadgetLibrary
discoverMore targetIncrease generator gen library =  fst $ sampleState (discover' library) gen
    where
        targetSize = M.size (gadgetMap library) + targetIncrease

        discover' :: GadgetLibrary -> RVar GadgetLibrary
        discover' library =
            if M.size (gadgetMap library) >= targetSize
                then do
                    return library
                else do
                    --stream          <- generator >>= return . inits
                    stream          <- generator >>= return . subsequences
                    let newLibrary  = foldr' (uncurry libraryInsert) library $ concatMap process stream
                    newLibrary `seq` discover' newLibrary
                    --discover' newLibrary

        -- Turn an instruction stream into a list of key value pairs for the
        -- gadget library
        process :: [Metadata] -> [(G.Gadget,([Metadata],G.ClobberList))]
        process stream = do
            (gadget, clobber) <- concat $ maybeToList $ eval stream >>= return . G.match
            return (gadget, (stream,clobber))

