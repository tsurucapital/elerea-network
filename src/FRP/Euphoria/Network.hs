--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module FRP.Euphoria.Network
    ( Receiver
    , mkReceiver
    , receiveSignal

    , Sender
    , mkSender
    , sendSignal
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative              (pure, (<$>), (<*>))
import           Control.Concurrent               (forkIO)
import           Data.ByteString                  (ByteString)
import           Data.IORef
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromMaybe)
import           Data.Serialize                   (Serialize)
import qualified Data.Serialize                   as Serialize
import           Data.Typeable                    (Typeable)
import           FRP.Elerea.Simple
import           Unsafe.Coerce                    (unsafeCoerce)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Incremental
import           FRP.Euphoria.Network.Packet
import           FRP.Euphoria.Network.Tag


--------------------------------------------------------------------------------
data Anything = forall a. Anything a


--------------------------------------------------------------------------------
data Decoder = forall a d. Decoder (Serialize.Get a) (Serialize.Get d)


--------------------------------------------------------------------------------
data Receiver = Receiver
    { receiverReader   :: IO (Maybe ByteString)
    , receiverDecoders :: IORef (Map Tag Decoder)
    , receiverSignals  :: IORef (Map Tag Anything)
    , receiverPackets  :: IORef (Map Tag [Packet])
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
            case Serialize.decode bs of
                Left err     -> error $ "Can't parse packet: " ++ err
                Right packet ->
                    atomicModifyIORef_ (receiverPackets rcv) $
                        M.insertWith (++) (packetChannel packet) [packet]

            receiverLoop rcv

  where
    atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())


--------------------------------------------------------------------------------
receiveSignal :: forall a d. (Serialize a, Serialize d, Typeable a, Typeable d)
              => Receiver -> a -> (d -> a -> a) -> IO (SignalGen (Signal a))
receiveSignal rcv initial update = do
    putStrLn $ "Registering get for " ++ show tag
    modifyIORef (receiverDecoders rcv) $ M.insert tag (Decoder getA getD)
    modifyIORef (receiverSignals rcv)  $ M.insert tag (Anything initial)


    return $ effectful $ do
        packets <- atomicModifyIORef (receiverPackets rcv) $ \m ->
            let packets = fromMaybe [] $ M.lookup tag m
            in (M.delete tag m, packets)

        state <- atomicModifyIORef (receiverSignals rcv) $ \m ->
            let s  = case m M.! tag of Anything x -> (unsafeCoerce x)
                s' = foldr (incremental update) s packets
                m' = M.insert tag (Anything s') m
            in (m', s')

        return state
  where
    tag  = typeTag (undefined :: a)  -- Todo: make this a tuple of (a, d)
    getA = Serialize.get :: Serialize.Get a
    getD = Serialize.get :: Serialize.Get d


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
    let packet = Packet FullState (typeTag x) 0 (Serialize.encode x)
    putStrLn $ "Writing to chan, type: " ++ show (typeTag x)
    senderWriter s (Serialize.encode packet)


--------------------------------------------------------------------------------
sendSignal :: (Serialize a, Typeable a)
           => Sender -> Signal a -> SignalGen (Signal ())
sendSignal s signal = effectful1 (senderSend s) signal
