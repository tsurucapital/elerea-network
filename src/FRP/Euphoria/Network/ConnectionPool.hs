---------------------------------------------------------------------------------
-- | Simple abstraction layer above UDP
module FRP.Euphoria.Network.ConnectionPool
    ( Peer
    , mkPeer

    , ConnectionPool
    , mkConnectionPool
    , connectionPoolRecv
    , connectionPoolSend
    , connectionPoolPeers
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad             (filterM, forever)
import           Data.ByteString           (ByteString)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Ord                  (comparing)
import           Data.Time                 (UTCTime, addUTCTime, getCurrentTime)
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB


--------------------------------------------------------------------------------
data Peer = Peer S.SockAddr
    deriving (Eq, Show)


--------------------------------------------------------------------------------
instance Ord Peer where
    compare = comparing show


--------------------------------------------------------------------------------
mkPeer :: String -> Int -> IO Peer
mkPeer host port = do
    addrinfos <- S.getAddrInfo Nothing (Just host) (Just $ show port)
    return $ Peer $ S.addrAddress $ head addrinfos


--------------------------------------------------------------------------------
data ConnectionPool = ConnectionPool
    { poolSocket            :: S.Socket
    , poolPeers             :: MVar (Map Peer UTCTime)
    , poolTimeout           :: Int
    , poolConnectHandler    :: Peer -> IO ()
    , poolDisconnectHandler :: Peer -> IO ()
    }


--------------------------------------------------------------------------------
mkConnectionPool :: String -> Int
                 -> (Peer -> IO ())
                 -> (Peer -> IO ())
                 -> IO ConnectionPool
mkConnectionPool host port connect disconnect = do
    socket <- S.socket S.AF_INET S.Datagram S.defaultProtocol
    _      <- S.setSocketOption socket S.ReuseAddr 1
    host'  <- S.inet_addr host
    S.bindSocket socket $ S.SockAddrInet (fromIntegral port) host'

    peers <- newMVar M.empty

    let pool = ConnectionPool socket peers 10 connect disconnect

    -- TODO: Some way to stop this thread?
    _ <- forkIO $ connectionCheckDisconnects pool

    return pool


--------------------------------------------------------------------------------
connectionPoolRecv :: ConnectionPool -> IO (Peer, ByteString)
connectionPoolRecv pool = do
    (bs, addr) <- SB.recvFrom (poolSocket pool) 1024
    time       <- getCurrentTime
    let peer = Peer addr

    modifyMVar_ (poolPeers pool) $ \peers -> do
        case M.lookup peer peers of
            Nothing -> poolConnectHandler pool peer
            _       -> return ()

        return $ M.insert peer time peers

    return (peer, bs)


--------------------------------------------------------------------------------
connectionPoolSend :: ConnectionPool -> Peer -> ByteString -> IO ()
connectionPoolSend pool (Peer addr) bs = do
    _ <- SB.sendTo (poolSocket pool) bs addr
    return ()


--------------------------------------------------------------------------------
connectionCheckDisconnects :: ConnectionPool -> IO ()
connectionCheckDisconnects pool = forever $ do
    threadDelay $ 1000 * 1000
    now <- getCurrentTime

    modifyMVar_ (poolPeers pool) $
        fmap M.fromAscList . filterM (check now) . M.toAscList
  where
    check now (peer, time)
        | fromIntegral (poolTimeout pool) `addUTCTime` time > now = return True
        | otherwise                                               = do
            poolDisconnectHandler pool peer
            return False


--------------------------------------------------------------------------------
-- | List of connected peers
connectionPoolPeers :: ConnectionPool -> IO [Peer]
connectionPoolPeers = fmap M.keys . readMVar . poolPeers
