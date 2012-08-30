--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module FRP.Euphoria.Network
    ( module FRP.Euphoria.Network.Types

    , Receiver
    , mkReceiver
    , receiveSignal

    , Sender
    , mkSender
    , sendSignal
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative        (pure, (<$>), (<*>))
import           Control.Concurrent         (forkIO)
import           Data.ByteString            (ByteString)
import           Data.IORef
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Monoid                (mappend)
import           Data.Serialize             (Serialize)
import qualified Data.Serialize             as Serialize
import           Data.Typeable              (Typeable)
import           FRP.Elerea.Simple
import           Unsafe.Coerce              (unsafeCoerce)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Types


--------------------------------------------------------------------------------
data Anything = forall a. Anything a


--------------------------------------------------------------------------------
data Decoder = forall a. Decoder (Serialize.Get a)


--------------------------------------------------------------------------------
data Receiver = Receiver
    { receiverReader   :: IO (Maybe ByteString)
    , receiverDecoders :: IORef (Map BeamType Decoder)
    , receiverSignals  :: IORef (Map BeamType Anything)
      -- TODO          : sequence numbers, ...
      -- TODO: More efficient type like hashmap or...
      -- TODO: unregister callbacks using finalizers
    }


--------------------------------------------------------------------------------
mkReceiver :: IO (Maybe ByteString)
           -> IO Receiver
mkReceiver reader = do
    rcv <- Receiver <$> pure reader
                    <*> newIORef M.empty
                    <*> newIORef M.empty

    _ <- forkIO $ receiverLoop rcv
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
                            Right x  -> modifyIORef (receiverSignals rcv) $
                                M.insert bt (Anything x)

            receiverLoop rcv


--------------------------------------------------------------------------------
receiveSignal :: forall a. (Serialize a, Typeable a)
              => Receiver -> a -> IO (SignalGen (Signal a))
receiveSignal rcv initial = do
    putStrLn $ "Registering get for " ++ show bt
    modifyIORef (receiverDecoders rcv) $ M.insert bt (Decoder get)
    modifyIORef (receiverSignals rcv)  $ M.insert bt (Anything initial)
    return $ effectful $ do
        map' <- readIORef (receiverSignals rcv)
        case M.lookup bt map' of
            Just (Anything x) -> return (unsafeCoerce x)
            Nothing           -> error "Receiver internal error"
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
sendSignal :: (Serialize a, Typeable a)
           => Sender -> Signal a -> SignalGen (Signal ())
sendSignal s signal = effectful1 (senderSend s) signal
