--------------------------------------------------------------------------------
import           Control.Applicative            ((*>))
import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Chan
import           Control.Monad                  (forever, replicateM)
import           System.Environment             (getArgs)
import           System.Random


--------------------------------------------------------------------------------
import           FRP.Elerea.Simple
import           FRP.Euphoria.Event
import           FRP.Euphoria.Network
import           FRP.Euphoria.Network.Multicast


--------------------------------------------------------------------------------
server :: String -> Int -> IO ()
server host port = do
    sender <- mkMulticastSender host port
    gen    <- newStdGen

    sampler <- start $ do
        r <- randomEvent gen
        s <- senderEvent sender r
        return $ eventToSignal r *> s

    forever $ do
        x <- sampler
        putStrLn $ "Server generated sample: " ++ show x
        threadDelay $ 1000 * 1000
  where
    randomE gen = fmap return $ stateful gen


--------------------------------------------------------------------------------
-- | The server generates random numbers between 0 and 10.
randomEvent :: RandomGen g => g -> SignalGen (Event Int)
randomEvent gen = do
    signal <- stateful (gen, []) $ \(g, _) ->
        let (x, g') = randomR (0, 10) g
        in (g', [x])

    return $ signalToEvent $ fmap snd signal


--------------------------------------------------------------------------------
client :: String -> Int -> IO ()
client host port = do
    receiver <- mkMulticastReceiver host port
    sgen     <- receiverExternalEvent receiver

    sampler <- start $ do
        sumE' <- sumE =<< sgen
        return $ eventToSignal sumE'

    forever $ do
        x <- sampler
        putStrLn $ "Client generated sample: " ++ show x
        threadDelay $ 3 * 1000 * 1000


--------------------------------------------------------------------------------
-- | The client adds all received numbers
sumE :: Event Int -> SignalGen (Event Int)
sumE e = scanAccumE 0 $ fmap (\a s -> let s' = a + s in (s', s')) e


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server" : _) -> server "239.0.0.1" 12345
        ("client" : _) -> client "239.0.0.1" 12345
