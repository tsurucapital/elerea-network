---------------------------------------------------------------------------------
-- | Simple abstraction layer above UDP
module FRP.Euphoria.Network.UdpPool
    ( Peer
    , mkPeer

    , UdpPool
    , mkUdpPool
    , udpPoolRecv
    , udpPoolSend
    , udpPoolPeers
    , udpPoolSetHandlers
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
data UdpPool = UdpPool
    { poolSocket            :: S.Socket
    , poolPeers             :: MVar (Map Peer UTCTime)
    , poolTimeout           :: Int
    , poolConnectHandler    :: Peer -> IO ()
    , poolDisconnectHandler :: Peer -> IO ()
    }


--------------------------------------------------------------------------------
mkUdpPool :: String -> Int -> IO UdpPool
mkUdpPool host port = do
    socket <- S.socket S.AF_INET S.Datagram S.defaultProtocol
    _      <- S.setSocketOption socket S.ReuseAddr 1
    host'  <- S.inet_addr host
    S.bindSocket socket $ S.SockAddrInet (fromIntegral port) host'

    peers <- newMVar M.empty

    let pool = UdpPool socket peers 10 nullHandler nullHandler

    -- TODO: Some way to stop this thread?
    _ <- forkIO $ udpCheckDisconnects pool

    return pool
  where
    nullHandler _ = return ()


--------------------------------------------------------------------------------
udpPoolRecv :: UdpPool -> IO (Peer, ByteString)
udpPoolRecv pool = do
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
udpPoolSend :: UdpPool -> Peer -> ByteString -> IO ()
udpPoolSend pool (Peer addr) bs = do
    _ <- SB.sendTo (poolSocket pool) bs addr
    return ()


--------------------------------------------------------------------------------
udpCheckDisconnects :: UdpPool -> IO ()
udpCheckDisconnects pool = forever $ do
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
udpPoolPeers :: UdpPool -> IO [Peer]
udpPoolPeers = fmap M.keys . readMVar . poolPeers


--------------------------------------------------------------------------------
-- | Set new connect/disconnect handlers
udpPoolSetHandlers :: (Peer -> IO ()) -> (Peer -> IO ()) -> UdpPool -> UdpPool
udpPoolSetHandlers connect disconnect pool = pool
    { poolConnectHandler    = connect
    , poolDisconnectHandler = disconnect
    }
