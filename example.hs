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
import qualified FRP.Elerea.Network as Network


--------------------------------------------------------------------------------
update :: Int -> [Int] -> [Int]
update d xs
    | length xs > 5 = drop 1 xs ++ [d]
    | otherwise     = xs ++ [d]


--------------------------------------------------------------------------------
server :: IO ()
server = do
    gen <- newStdGen

    sampler <- start $ do
        deltas <- stateful 0 (+ 1)

        rec
            let rsum = update <$> deltas <*> rsum'
            rsum' <- delay [] rsum

        ssignals <- join $ execute $ Network.server "0.0.0.0" 123456
            () (\() () -> ()) (fmap const rsum) (fmap const $ fmap Just deltas)

        return $ (,) <$> rsum <*> ssignals

    forever $ do
        x <- sampler
        putStrLn $ "Server generated sample: " ++ show x
        threadDelay $ 1000 * 1000


--------------------------------------------------------------------------------
client :: IO ()
client = do
    sampler <- start $
        join $ execute $ Network.client "127.0.0.1" 123456 ([] :: [Int]) update
            (pure ()) (pure (Nothing :: Maybe ()))
    forever $ do
        x <- sampler
        putStrLn $ "Client generated sample: " ++ show x
        threadDelay $ 1000 * 1000


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server" : _) -> server
        ("client" : _) -> client
        _              -> error "Specify either 'server' or 'client'"
