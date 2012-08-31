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
data Receiver = Receiver
    { receiverReader  :: IO (Maybe ByteString)
    , receiverSignals :: IORef (Map Tag Anything)
    , receiverPackets :: IORef (Map Tag [Packet])
      -- TODO          : sequence numbers, ...
      -- TODO: More efficient type like hashmap or...
      -- TODO: unregister callbacks using finalizers
    }


--------------------------------------------------------------------------------
mkReceiver :: IO (Maybe ByteString)
           -> IO Receiver
mkReceiver reader = do
    rcv <- Receiver <$> pure reader <*> newIORef M.empty <*> newIORef M.empty
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
            case Serialize.decode bs of
                Left err     -> error $ "Can't parse packet: " ++ err
                Right packet ->
                    atomicModifyIORef_ (receiverPackets rcv) $
                        M.insertWith (++) (packetChannel packet) [packet]

            receiverLoop rcv


--------------------------------------------------------------------------------
receiveSignal :: forall a d. (Serialize a, Serialize d, Typeable a, Typeable d)
              => Receiver
              -> a
              -> (d -> a -> a)
              -> IO (SignalGen (Signal a))
receiveSignal rcv initial update = do
    putStrLn $ "Registering get for " ++ show tag
    atomicModifyIORef_ (receiverSignals rcv)  $ M.insert tag (Anything initial)

    return $ effectful $ do
        packets <- atomicModifyIORef (receiverPackets rcv) $ \m ->
            let packets = fromMaybe [] $ M.lookup tag m
            in (M.delete tag m, packets)

        state <- atomicModifyIORef (receiverSignals rcv) $ \m ->
            let s  = case m M.! tag of Anything x -> unsafeCoerce x
                s' = foldr (incremental update) s packets
                m' = M.insert tag (Anything s') m
            in (m', s')

        return state
  where
    tag = typeTag (undefined :: a)  -- Todo: make this a tuple of (a, d)


--------------------------------------------------------------------------------
data Sender = Sender
    { senderWriter  :: ByteString -> IO ()
    , senderSignals :: IORef (Map Tag Anything)
    }


--------------------------------------------------------------------------------
mkSender :: (ByteString -> IO ()) -> IO Sender
mkSender writer = Sender <$> pure writer <*> newIORef M.empty


--------------------------------------------------------------------------------
-- TODO: Modify to work on [a]?
senderSend :: Sender -> Packet -> IO ()
senderSend s packet = do
    putStrLn $ "Writing to chan: " ++ show (packetChannel packet)
    senderWriter s (Serialize.encode packet)


--------------------------------------------------------------------------------
sendSignal :: (Serialize a, Serialize d, Typeable a, Typeable d)
           => Sender
           -> a
           -> (d -> a -> a)
           -> Signal d
           -> SignalGen (Signal a)
sendSignal sender initial update delta = do
    execute $ atomicModifyIORef_ (senderSignals sender) $
        M.insert tag (Anything initial)

    effectful1 update' delta
  where
    tag       = typeTag initial
    update' d = do
        state <- atomicModifyIORef (senderSignals sender) $ \m ->
            let s  = case m M.! tag of Anything x -> unsafeCoerce x
                s' = update d s
                m' = M.insert tag (Anything s') m
            in (m', s')

        -- (A) Send full state
        -- senderSend sender $ Packet FullState tag 0 $ Serialize.encode state

        -- (B) Send incremental update
        senderSend sender $ Packet Delta tag 0 $ Serialize.encode d

        return state
        

--------------------------------------------------------------------------------
-- | Utility
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())
