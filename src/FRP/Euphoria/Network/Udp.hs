--------------------------------------------------------------------------------
module FRP.Euphoria.Network.Udp
    ( mkUdpReceiver
    , mkUdpSender
    ) where


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network
import           FRP.Euphoria.Network.ConnectionPool


--------------------------------------------------------------------------------
mkUdpReceiver :: String -> Int -> IO Receiver
mkUdpReceiver host port = do
    pool <- mkConnectionPool host port
    mkReceiver $ do
        (_, bs) <- connectionPoolRecv pool
        return $ Just bs


--------------------------------------------------------------------------------
mkUdpSender :: String -> Int -> String -> Int -> IO Sender
mkUdpSender shost sport host port = do
    pool <- mkConnectionPool shost sport
    peer <- mkPeer host port
    mkSender $ \bs -> connectionPoolSend pool peer bs
