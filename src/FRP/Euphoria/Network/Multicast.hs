--------------------------------------------------------------------------------
module FRP.Euphoria.Network.Multicast
    ( mkMulticastReceiver
    , mkMulticastSender
    ) where


--------------------------------------------------------------------------------
import           Network.Multicast         (multicastReceiver, multicastSender)
import           Network.Socket.ByteString (recvFrom, sendTo)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network


--------------------------------------------------------------------------------
mkMulticastReceiver :: String -> Int -> IO Receiver
mkMulticastReceiver host port = do
    sock <- multicastReceiver host (fromIntegral port)
    mkReceiver $ do
        (bs, _) <- recvFrom sock 1024
        return $ Just bs


--------------------------------------------------------------------------------
mkMulticastSender :: String -> Int -> IO Sender
mkMulticastSender host port = do
    (sock, addr) <- multicastSender host (fromIntegral port)
    mkSender $ \bs -> do
        putStrLn "Sending..."
        _ <- sendTo sock bs addr
        return ()
