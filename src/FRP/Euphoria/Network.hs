--------------------------------------------------------------------------------
{-# LANGUAGE DoRec                      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module FRP.Euphoria.Network
    ( Network
    , mkNetwork
    , networkPeers
    , networkReceive
    , networkSend
    , networkBroadcast
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative             ((<$>), (<*>), (*>))
import           Control.Concurrent              (forkIO)
import           Control.Monad                   (forM, void)
import           Data.IORef
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Serialize                  (Serialize)
import qualified Data.Serialize                  as Serialize
import           Data.Typeable                   (Typeable)
import           FRP.Elerea.Simple


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Connection
import           FRP.Euphoria.Network.Types
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

    let pool' = udpPoolSetHandlers (connect_ nw) (disconnect nw) pool
        nw    = Network pool' connections

    _ <- forkIO $ receiveLoop nw
    return nw


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
    :: forall s d. (Serialize s, Serialize d, Typeable s, Typeable d)
    => Network
    -> s
    -> (d -> s -> s)
    -> SignalGen (Signal [(Peer, s)])
networkReceive nw initial update = effectful flushAll
  where
    flushAll = do
        conns <- readIORef (networkConnections nw)
        forM (M.toList conns) $ \(peer, conn) -> do
            x <- connectionFlush conn initial update
            return (peer, x)


--------------------------------------------------------------------------------
networkSend :: (Serialize s, Serialize d, Typeable s, Typeable d)
            => Network
            -> Signal [(Peer, Update s d)]
            -> SignalGen (Signal [(Peer, s)])
networkSend nw peerS = effectful1 send peerS
  where
    send peers = do
        conns <- readIORef (networkConnections nw)
        forM peers $ \(peer, update) -> do
            conn  <- maybe (connect nw peer) return $ M.lookup peer conns
            state <- connectionSend conn update
            return (peer, state)


--------------------------------------------------------------------------------
networkBroadcast
    :: forall s d. (Serialize s, Serialize d, Typeable s, Typeable d)
    => Network
    -> [Peer]
    -> s
    -> (d -> s -> s)
    -> Signal d
    -> SignalGen (Signal s)
networkBroadcast nw peers initial update deltaS = do
    rec
        updates <- delay (Absolute initial) updates'
        let updates' = update' <$> deltaS <*> updates

    out <- networkSend nw $ zip peers . repeat <$> updates 
    return $ out *> fmap updateState updates
  where
    update' :: d -> Update s d -> Update s d
    update' d = Delta d . update d . updateState


--------------------------------------------------------------------------------
connect :: Network -> Peer -> IO Connection
connect nw peer = do
    putStrLn $ "Connected: " ++ show peer
    conn <- mkConnection $ \bs -> udpPoolSend (networkUdpPool nw) peer bs
    atomicModifyIORef (networkConnections nw) $ \m ->
        (M.insert peer conn m, ())
    return conn


--------------------------------------------------------------------------------
connect_ :: Network -> Peer -> IO ()
connect_ nw = void . connect nw


--------------------------------------------------------------------------------
disconnect :: Network -> Peer -> IO ()
disconnect nw peer = do
    putStrLn $ "Disconnected: " ++ show peer
    atomicModifyIORef (networkConnections nw) $ \m -> (M.delete peer m, ())
