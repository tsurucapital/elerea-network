--------------------------------------------------------------------------------
{-# LANGUAGE DoRec #-}


--------------------------------------------------------------------------------
import           Control.Applicative            (pure, (<$>), (<*>), (<*))
import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (forever, join)
import           System.Environment             (getArgs)
import           System.Random


--------------------------------------------------------------------------------
import           FRP.Elerea.Simple
import qualified FRP.Euphoria.Network as Network


--------------------------------------------------------------------------------
server :: IO ()
server = do
    gen <- newStdGen

    sampler <- start $ do
        deltas <- randomSignal gen

        rec
            let rsum = (+) <$> rsum' <*> deltas :: Signal Int
            rsum' <- delay 0 rsum

        ssignals <- join $ execute $ Network.server "0.0.0.0" 123456
            (fmap const rsum) (fmap const deltas)

        return $ (,) <$> rsum <*> ssignals

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
client :: IO ()
client = do
    sampler <- start $
        join $ execute $ Network.client "127.0.0.1" 123456 (0 :: Int) (+)
    forever $ do
        x <- sampler
        putStrLn $ "Client generated sample: " ++ show x
        threadDelay $ 1000 * 1000


{-
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
-}


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server" : _) -> server
        ("client" : _) -> client
        _              -> error "Specify either 'server' or 'client'"
