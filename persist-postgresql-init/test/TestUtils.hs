module TestUtils
    where

import           BasicSchema                 (migrateAll)
import           Cache                       (RedisInfo, localRedisInfo)
import           CacheServer                 (runServer)
import           Control.Concurrent          (ThreadId, forkIO, threadDelay)
import           Control.Monad.Logger        (runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Database                    (PGInfo, localConnString)
import           Database.Persist.Postgresql
    ( runMigrationSilent
    , withPostgresqlConn
    )
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Servant.Client              (ClientEnv (..), parseBaseUrl)

setupTests :: IO (PGInfo, RedisInfo, ClientEnv, ThreadId)
setupTests = do
  mgr <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8000"
  let clientEnv = ClientEnv mgr baseUrl Nothing
  runStdoutLoggingT $ withPostgresqlConn localConnString $ \dbConn ->
    runReaderT (runMigrationSilent migrateAll) dbConn
  tid <- forkIO runServer
  threadDelay 1000000
  return (localConnString, localRedisInfo, clientEnv, tid)
