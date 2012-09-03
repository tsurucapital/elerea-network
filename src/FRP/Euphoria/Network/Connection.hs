--------------------------------------------------------------------------------
-- | Simple connection management between two peers which can transfer signals
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module FRP.Euphoria.Network.Connection
    ( Connection
    , mkConnection
    , connectionQueue
    , connectionFlush
    , connectionSend
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative              (pure, (<$>), (<*>))
import           Data.ByteString                  (ByteString)
import           Data.IORef
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromMaybe)
import           Data.Serialize                   (Serialize)
import qualified Data.Serialize                   as Serialize
import           Data.Typeable                    (Typeable)
import           Unsafe.Coerce                    (unsafeCoerce)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Incremental
import           FRP.Euphoria.Network.Packet
import           FRP.Euphoria.Network.Tag
import           FRP.Euphoria.Network.Types


--------------------------------------------------------------------------------
data Anything = forall a. Anything a


--------------------------------------------------------------------------------
data Connection = Connection
    { connectionWriter      :: ByteString -> IO ()

    , connectionSignalsIn   :: IORef (Map Tag Anything)
    , connectionPacketQueue :: IORef (Map Tag [Packet])
      -- TODO          : sequence numbers, ...
      -- TODO: More efficient type like hashmap or...
      -- TODO: unregister callbacks using finalizers
    }


--------------------------------------------------------------------------------
mkConnection :: (ByteString -> IO ())
             -> IO Connection
mkConnection writer = Connection
    <$> pure writer
    <*> newIORef M.empty
    <*> newIORef M.empty


--------------------------------------------------------------------------------
connectionQueue :: Connection -> Packet -> IO ()
connectionQueue conn packet =
    atomicModifyIORef_ (connectionPacketQueue conn) $
        M.insertWith (++) (packetChannel packet) [packet]


--------------------------------------------------------------------------------
-- | Flushed receival state for a tag?
connectionFlush
    :: forall a d. (Serialize a, Serialize d, Typeable a, Typeable d)
    => Connection
    -> a
    -> (d -> a -> a)
    -> IO a
connectionFlush conn initial update = do
    packets <- atomicModifyIORef (connectionPacketQueue conn) $ \m ->
        let packets = fromMaybe [] $ M.lookup tag m
        in (M.delete tag m, packets)

    state <- atomicModifyIORef (connectionSignalsIn conn) $ \m ->
        let s  = case M.lookup tag m of
                    Nothing           -> initial
                    Just (Anything x) -> unsafeCoerce x
            s' = foldr (incremental update) s packets
            m' = M.insert tag (Anything s') m
        in (m', s')

    return state
  where
    tag = typeTag (undefined :: a)  -- Todo: make this a tuple of (a, d)


--------------------------------------------------------------------------------
{-
connectionReceive
    :: forall a d. (Serialize a, Serialize d, Typeable a, Typeable d)
    => Connection
    -> a
    -> (d -> a -> a)
    -> SignalGen (Signal a)
connectionReceive conn initial update = do
    execute $ putStrLn $ "Registering get for " ++ show tag
    execute $ atomicModifyIORef_ (connectionSignalsIn conn) $
        M.insert tag (Anything initial)

    effectful $ do
        packets <- atomicModifyIORef (connectionPacketQueue conn) $ \m ->
            let packets = fromMaybe [] $ M.lookup tag m
            in (M.delete tag m, packets)

        state <- atomicModifyIORef (connectionSignalsIn conn) $ \m ->
            let s  = case m M.! tag of Anything x -> unsafeCoerce x
                s' = foldr (incremental update) s packets
                m' = M.insert tag (Anything s') m
            in (m', s')

        return state
  where
    tag = typeTag (undefined :: a)  -- Todo: make this a tuple of (a, d)
-}


--------------------------------------------------------------------------------
-- TODO: Modify to work on [a]?
sendPacket :: Connection -> Packet -> IO ()
sendPacket conn packet = do
    putStrLn $ "Writing to chan: " ++ show (packetChannel packet)
    connectionWriter conn (Serialize.encode packet)


--------------------------------------------------------------------------------
connectionSend :: forall a d. (Serialize a, Serialize d, Typeable a, Typeable d)
               => Connection
               -> Update a d
               -> IO a
connectionSend sender update =
    -- Note how we currently always send delta's when possible. This needs to
    -- change once we're receiving acks!
    case update of
        -- (A) Send full state
        Absolute s -> do
            sendPacket sender $ Packet AbsolutePacket tag 0 $ Serialize.encode s
            return s
        -- (B) Send incremental update
        Delta d s -> do
            sendPacket sender $ Packet DeltaPacket tag 0 $ Serialize.encode d
            return s
  where
    tag = typeTag (undefined :: a)


--------------------------------------------------------------------------------
-- | Utility
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())
