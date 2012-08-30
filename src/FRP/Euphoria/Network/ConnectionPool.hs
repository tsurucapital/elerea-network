---------------------------------------------------------------------------------
-- | Simple abstraction layer above UDP
module FRP.Euphoria.Network.ConnectionPool
    ( Peer
    , mkPeer

    , ConnectionPool
    , mkConnectionPool
    , connectionPoolRecv
    , connectionPoolSend
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString           (ByteString)
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB


--------------------------------------------------------------------------------
data Peer = Peer S.SockAddr
    deriving (Show)


--------------------------------------------------------------------------------
mkPeer :: String -> Int -> IO Peer
mkPeer host port = do
    addrinfos <- S.getAddrInfo Nothing (Just host) (Just $ show port)
    return $ Peer $ S.addrAddress $ head addrinfos


--------------------------------------------------------------------------------
data ConnectionPool = ConnectionPool S.Socket


--------------------------------------------------------------------------------
mkConnectionPool :: String -> Int -> IO ConnectionPool
mkConnectionPool host port = do
    socket <- S.socket S.AF_INET S.Datagram S.defaultProtocol
    _      <- S.setSocketOption socket S.ReuseAddr 1
    host'  <- S.inet_addr host
    S.bindSocket socket $ S.SockAddrInet (fromIntegral port) host'
    return $ ConnectionPool socket


--------------------------------------------------------------------------------
connectionPoolRecv :: ConnectionPool -> IO (Peer, ByteString)
connectionPoolRecv (ConnectionPool socket) = do
    (bs, addr) <- SB.recvFrom socket 1024
    return (Peer addr, bs)


--------------------------------------------------------------------------------
connectionPoolSend :: ConnectionPool -> Peer -> ByteString -> IO ()
connectionPoolSend (ConnectionPool socket) (Peer addr) bs = do
    _ <- SB.sendTo socket bs addr
    return ()
