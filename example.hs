--------------------------------------------------------------------------------
{-# LANGUAGE DoRec #-}


--------------------------------------------------------------------------------
import           Control.Applicative            (pure, (<$>), (<*>), (<*))
import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (forever)
import           System.Environment             (getArgs)
import           System.Random


--------------------------------------------------------------------------------
import           FRP.Elerea.Simple
import           FRP.Euphoria.Network
import           FRP.Euphoria.Network.UdpPool


--------------------------------------------------------------------------------
server :: String -> Int -> IO ()
server host port = do
    sender <- mkNetwork "0.0.0.0" 123456
    gen    <- newStdGen
    peer   <- mkPeer host port

    sampler <- start $ do
        r <- randomSignal gen
        s <- networkBroadcast sender [peer] 0 (+) r
        return s

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
    network <- mkNetwork host port
    sampler <- start $ networkReceive network 0 (+) :: IO (IO [(Peer, Int)])
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
        ("client" : _) -> client "0.0.0.0" 12345
        _              -> error "Specify either 'server' or 'client'"
