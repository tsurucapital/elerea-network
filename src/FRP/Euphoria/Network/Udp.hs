--------------------------------------------------------------------------------
module FRP.Euphoria.Network.Udp
    ( mkUdpReceiver
    , mkUdpSender
    ) where


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network
import           FRP.Euphoria.Network.ConnectionPool


--------------------------------------------------------------------------------
connectHandler, disconnectHandler :: Peer -> IO ()
connectHandler    peer = putStrLn $ "Connected: "    ++ show peer
disconnectHandler peer = putStrLn $ "Disconnected: " ++ show peer


--------------------------------------------------------------------------------
mkUdpReceiver :: String -> Int -> IO Receiver
mkUdpReceiver host port = do
    pool <- mkConnectionPool host port connectHandler disconnectHandler
    mkReceiver $ do
        (_, bs) <- connectionPoolRecv pool
        return $ Just bs


--------------------------------------------------------------------------------
mkUdpSender :: String -> Int -> String -> Int -> IO Sender
mkUdpSender shost sport host port = do
    pool <- mkConnectionPool shost sport connectHandler disconnectHandler
    peer <- mkPeer host port
    mkSender $ \bs -> connectionPoolSend pool peer bs
