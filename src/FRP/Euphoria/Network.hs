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
import           Control.Concurrent.Chan
import           Control.Monad                 (forever)
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


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Dispatch
import           FRP.Euphoria.Network.Types


--------------------------------------------------------------------------------
data Decoder = forall a. Decoder (Serialize.Get a)


--------------------------------------------------------------------------------
data Receiver = Receiver
    { receiverDispatch :: Dispatch
    , receiverChan     :: Chan ByteString
    , receiverDecoders :: IORef (Map BeamType Decoder)
      -- TODO          : sequence numbers, ...
      -- TODO: More efficient type like hashmap or...
      -- TODO: unregister callbacks using finalizers
    }


--------------------------------------------------------------------------------
mkReceiver :: Chan ByteString -> IO Receiver
mkReceiver chan = do
    rcv <- Receiver <$> mkDispatch <*> pure chan <*> newIORef M.empty
    _   <- forkIO $ receiverLoop rcv
    return rcv


--------------------------------------------------------------------------------
receiverLoop :: Receiver -> IO ()
receiverLoop rcv = forever $ do
    putStrLn "Reading from chan..."
    bs <- readChan $ receiverChan rcv

    putStrLn $ "Read: " ++ show bs
    case Serialize.runGetState Serialize.get bs 0 of
        Left err        -> error $ "Can't parse beamtype: " ++ err
        Right (bt, bs') -> do
            gets <- readIORef $ receiverDecoders rcv
            case M.lookup bt gets of
                Nothing            -> putStrLn "No Get for beamtype"
                Just (Decoder get) -> case Serialize.runGet get bs' of
                    Left err -> error $ "Can't parse data: " ++ err
                    Right x  -> dispatchCall (receiverDispatch rcv) bt x


--------------------------------------------------------------------------------
receiverExternalEvent :: forall a. (Serialize a, Typeable a)
                      => Receiver -> IO (SignalGen (Event a))
receiverExternalEvent rcv = do
    putStrLn $ "Registering get for " ++ show bt
    modifyIORef (receiverDecoders rcv) $ M.insert bt (Decoder get)
    (sgen, callback) <- externalEvent
    dispatchAdd (receiverDispatch rcv) callback
    return sgen
  where
    bt  = beamType (undefined :: a)
    get = Serialize.get :: Serialize.Get a


--------------------------------------------------------------------------------
data Sender = Sender
    { senderChan :: Chan ByteString
    }


--------------------------------------------------------------------------------
mkSender :: Chan ByteString -> IO Sender
mkSender chan = return $ Sender chan


--------------------------------------------------------------------------------
-- TODO: Modify to work on [a]?
senderSend :: (Serialize a, Typeable a) => Sender -> a -> IO ()
senderSend s x = do
    let bs = Serialize.encode (beamType x) `mappend` Serialize.encode x
    putStrLn $ "Writing to chan, type: " ++ show (beamType x)
    writeChan (senderChan s) bs


--------------------------------------------------------------------------------
senderEvent :: (Serialize a, Typeable a)
            => Sender -> Event a -> SignalGen ()
senderEvent s e = do
    _ <- effectful1 (mapM_ $ senderSend s) (eventToSignal e )
    return ()
