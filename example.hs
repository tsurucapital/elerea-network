--------------------------------------------------------------------------------
{-# LANGUAGE DoRec #-}


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>), (<*>), (<*))
import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (forever)
import           System.Environment             (getArgs)
import           System.Random


--------------------------------------------------------------------------------
import           FRP.Elerea.Simple
import           FRP.Euphoria.Network
import           FRP.Euphoria.Network.Udp


--------------------------------------------------------------------------------
server :: String -> Int -> IO ()
server host port = do
    sender <- mkUdpSender "0.0.0.0" 123456 host port
    gen    <- newStdGen

    sampler <- start $ do
        r <- randomSignal gen
        s <- sendSignal sender r
        return $ r <* s

    forever $ do
        x <- sampler
        putStrLn $ "Server generated sample: " ++ show x
        threadDelay $ 1000 * 1000


--------------------------------------------------------------------------------
-- | The server generates random numbers between 0 and 10.
randomSignal :: RandomGen g => g -> SignalGen (Signal Int)
randomSignal gen = do
    signal <- stateful (gen, 0) $ \(g, _) ->
        let (x, g') = randomR (0, 10) g
        in (g', x)

    return $ fmap snd signal


--------------------------------------------------------------------------------
client :: String -> Int -> IO ()
client host port = do
    receiver <- mkUdpReceiver host port
    sgen     <- receiveSignal receiver 0

    sampler <- start $ average =<< sgen

    forever $ do
        x <- sampler
        putStrLn $ "Client generated sample: " ++ show x
        threadDelay $ 1000 * 1000


--------------------------------------------------------------------------------
-- | The client computes a weighted average
average :: Signal Int -> SignalGen (Signal Double)
average signal = do
    rec
        let averages = update <$> averages' <*> signal
        averages' <- delay 0 averages

    return averages
  where
    update w x = w * 0.9 + fromIntegral x * 0.1


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server" : _) -> server "127.0.0.1" 12345
        ("client" : _) -> client "127.0.0.1" 12345
        _              -> error "Specify either 'server' or 'client'"
