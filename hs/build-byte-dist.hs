import              Control.Monad.IO.Class
import              Control.Monad.Trans.Class
import              Data.Enumerator
import qualified    Data.Foldable           as F
import qualified    Data.Map                as M
import              Data.String
import              Filesystem.Enumerator
import qualified    Filesystem.Path         as P
import              Igor.ByteModel

builder :: Step P.FilePath IO ByteDistribution
builder = Continue $ go $ SampledDistribution M.empty
    where
        go :: ByteDistribution -> Stream P.FilePath -> Iteratee P.FilePath IO ByteDistribution
        go dist (Chunks paths)  = do
            dist' <- liftIO $ F.foldrM process dist paths
            continue $ go dist
        go dist EOF             = yield dist EOF
        
        process :: P.FilePath -> ByteDistribution -> IO ByteDistribution
        process path dist = do
            putStrLn $ show path
            return dist

main :: IO ()
main = do
    let enum = traverse True (fromString "./")
    runIteratee $ enum builder
    return ()
