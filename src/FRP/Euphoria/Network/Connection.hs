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
    , connectionReceive
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
import           FRP.Elerea.Simple
import           Unsafe.Coerce                    (unsafeCoerce)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Incremental
import           FRP.Euphoria.Network.Packet
import           FRP.Euphoria.Network.Tag


--------------------------------------------------------------------------------
data Anything = forall a. Anything a


--------------------------------------------------------------------------------
data Connection = Connection
    { connectionWriter      :: ByteString -> IO ()

    , connectionSignalsIn   :: IORef (Map Tag Anything)
    , connectionPacketQueue :: IORef (Map Tag [Packet])

    , connectionSignalsOut  :: IORef (Map Tag Anything)
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


--------------------------------------------------------------------------------
-- TODO: Modify to work on [a]?
sendPacket :: Connection -> Packet -> IO ()
sendPacket conn packet = do
    putStrLn $ "Writing to chan: " ++ show (packetChannel packet)
    connectionWriter conn (Serialize.encode packet)


--------------------------------------------------------------------------------
-- TODO: Currently, if we're broadcasting, the full state is kept on the server
-- for each connection, which is very bad. We should probably move this up to
-- 'Network'?
connectionSend :: (Serialize a, Serialize d, Typeable a, Typeable d)
           => Connection
           -> a
           -> (d -> a -> a)
           -> d
           -> IO a
connectionSend sender initial update delta = do
    state <- atomicModifyIORef (connectionSignalsOut sender) $ \m ->
        let s  = case M.lookup tag m of
                    Nothing           -> initial
                    Just (Anything x) -> unsafeCoerce x
            s' = update delta s
            m' = M.insert tag (Anything s') m
        in (m', s')

    -- (A) Send full state
    -- sendPacket sender $ Packet FullState tag 0 $ Serialize.encode state

    -- (B) Send incremental update
    sendPacket sender $ Packet Delta tag 0 $ Serialize.encode delta

    return state
  where
    tag = typeTag initial


--------------------------------------------------------------------------------
-- | Utility
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())
