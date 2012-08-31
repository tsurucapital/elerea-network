--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module FRP.Euphoria.Network
    ( Network
    , mkNetwork
    , networkPeers
    , networkReceive
    , networkSend
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                    (forM)
import           Data.IORef
import Control.Concurrent (forkIO)
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Serialize                   (Serialize)
import qualified Data.Serialize                   as Serialize
import           Data.Typeable                    (Typeable)
import           FRP.Elerea.Simple
import           Unsafe.Coerce                    (unsafeCoerce)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Connection
import           FRP.Euphoria.Network.Incremental
import           FRP.Euphoria.Network.Packet
import           FRP.Euphoria.Network.Tag
import           FRP.Euphoria.Network.UdpPool


--------------------------------------------------------------------------------
data Network = Network
    { networkUdpPool     :: UdpPool
    , networkConnections :: IORef (Map Peer Connection)
    }


--------------------------------------------------------------------------------
mkNetwork :: String -> Int -> IO Network
mkNetwork host port = do
    pool        <- mkUdpPool host port
    connections <- newIORef M.empty

    let pool'   = udpPoolSetHandlers (connect network) (disconnect network) pool
        network = Network pool' connections

    _ <- forkIO $ receiveLoop network
    return network


--------------------------------------------------------------------------------
receiveLoop :: Network -> IO ()
receiveLoop nw = do
    putStrLn "Reading..."
    (peer, bs) <- udpPoolRecv $ networkUdpPool nw
    putStrLn $ "[" ++ show peer ++ "]: " ++ show bs

    case Serialize.decode bs of
        Left err     -> error $ "Can't parse packet: " ++ err
        Right packet -> do
            conns <- readIORef (networkConnections nw)
            case M.lookup peer conns of
                Nothing   -> error "Unknown peer!"  -- Should not happen
                Just conn -> connectionQueue conn packet

    receiveLoop nw


--------------------------------------------------------------------------------
networkPeers :: Network -> SignalGen (Signal [Peer])
networkPeers = effectful . udpPoolPeers . networkUdpPool


--------------------------------------------------------------------------------
networkReceive
    :: forall a d. (Serialize a, Serialize d, Typeable a, Typeable d)
    => Network
    -> a
    -> (d -> a -> a)
    -> SignalGen (Signal [(Peer, a)])
networkReceive nw initial update = effectful flushAll
  where
    flushAll = do
        conns <- readIORef (networkConnections nw)
        forM (M.toList conns) $ \(peer, conn) -> do
            x <- connectionFlush conn initial update
            return (peer, x)


--------------------------------------------------------------------------------
networkSend :: (Serialize a, Serialize d, Typeable a, Typeable d)
           => Network
           -> a
           -> (d -> a -> a)
           -> Signal d
           -> Signal Peer
           -> SignalGen (Signal a)
networkSend nw initial update deltaS peerS =
    effectful2 send deltaS peerS
  where
    send delta peer = do
        conns <- readIORef (networkConnections nw)
        case M.lookup peer conns of
            Nothing   -> connect nw peer >> send delta peer
            Just conn -> connectionSend conn initial update delta


--------------------------------------------------------------------------------
connect :: Network -> Peer -> IO ()
connect nw peer = do
    putStrLn $ "Connected: " ++ show peer
    conn <- mkConnection $ \bs -> udpPoolSend (networkUdpPool nw) peer bs
    atomicModifyIORef (networkConnections nw) $ \m ->
        (M.insert peer conn m, ())


--------------------------------------------------------------------------------
disconnect :: Network -> Peer -> IO ()
disconnect nw peer = do
    putStrLn $ "Disconnected: " ++ show peer
    atomicModifyIORef (networkConnections nw) $ \m -> (M.delete peer m, ())
