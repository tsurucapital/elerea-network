--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module FRP.Euphoria.Network
    ( module FRP.Euphoria.Network.Types

    , Receiver
    , mkReceiver
    , receiverExternalEvent

    , Sender
    , mkSender
    , senderEvent
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (pure, (<$>), (<*>))
import           Control.Concurrent            (forkIO)
import           Data.ByteString               (ByteString)
import           Data.IORef
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Monoid                   (mappend)
import           Data.Serialize                (Serialize)
import qualified Data.Serialize                as Serialize
import           Data.Typeable                 (Typeable)
import           FRP.Elerea.Simple
import           FRP.Euphoria.Event
import           System.Mem.Weak               (addFinalizer)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Dispatch
import           FRP.Euphoria.Network.Types


--------------------------------------------------------------------------------
data Decoder = forall a. Decoder (Serialize.Get a)


--------------------------------------------------------------------------------
data Receiver = Receiver
    { receiverReader   :: IO (Maybe ByteString)
    , receiverDispatch :: Dispatch
    , receiverDecoders :: IORef (Map BeamType Decoder)
      -- TODO          : sequence numbers, ...
      -- TODO: More efficient type like hashmap or...
      -- TODO: unregister callbacks using finalizers
    }


--------------------------------------------------------------------------------
mkReceiver :: IO (Maybe ByteString)
           -> IO Receiver
mkReceiver reader = do
    rcv <- Receiver <$> pure reader <*> mkDispatch <*> newIORef M.empty
    _   <- forkIO $ receiverLoop rcv
    return rcv


--------------------------------------------------------------------------------
receiverLoop :: Receiver -> IO ()
receiverLoop rcv = do
    putStrLn "Reading..."
    mbs <- receiverReader rcv
    putStrLn $ "Read: " ++ show mbs

    case mbs of
        Nothing -> putStrLn "Reader EOF"
        Just bs -> do
            case Serialize.runGetState Serialize.get bs 0 of
                Left err        -> error $ "Can't parse beamtype: " ++ err
                Right (bt, bs') -> do
                    gets <- readIORef $ receiverDecoders rcv
                    case M.lookup bt gets of
                        Nothing            -> putStrLn "No Get for beamtype"
                        Just (Decoder get) -> case Serialize.runGet get bs' of
                            Left err -> error $ "Can't parse data: " ++ err
                            Right x  -> dispatchCall (receiverDispatch rcv) bt x

            receiverLoop rcv


--------------------------------------------------------------------------------
receiverExternalEvent :: forall a. (Serialize a, Typeable a)
                      => Receiver -> IO (SignalGen (Event a))
receiverExternalEvent rcv = do
    putStrLn $ "Registering get for " ++ show bt
    modifyIORef (receiverDecoders rcv) $ M.insert bt (Decoder get)
    (sgen, callback) <- externalEvent
    finalizer        <- dispatchAdd (receiverDispatch rcv) callback
    return $ do
        ev <- sgen
        execute $ addFinalizer ev finalizer
        return ev
  where
    bt  = beamType (undefined :: a)
    get = Serialize.get :: Serialize.Get a


--------------------------------------------------------------------------------
data Sender = Sender
    { senderWriter :: ByteString -> IO ()
    }


--------------------------------------------------------------------------------
mkSender :: (ByteString -> IO ()) -> IO Sender
mkSender writer = return $ Sender writer


--------------------------------------------------------------------------------
-- TODO: Modify to work on [a]?
senderSend :: (Serialize a, Typeable a) => Sender -> a -> IO ()
senderSend s x = do
    let bs = Serialize.encode (beamType x) `mappend` Serialize.encode x
    putStrLn $ "Writing to chan, type: " ++ show (beamType x)
    senderWriter s bs


--------------------------------------------------------------------------------
senderEvent :: (Serialize a, Typeable a)
            => Sender -> Event a -> SignalGen (Signal ())
senderEvent s e = effectful1 (mapM_ $ senderSend s) (eventToSignal e )
