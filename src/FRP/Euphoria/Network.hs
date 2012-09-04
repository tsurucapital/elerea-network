--------------------------------------------------------------------------------
{-# LANGUAGE DoRec #-}
module FRP.Euphoria.Network
    ( client

    , ServerSignals (..)
    , server
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative         ((<*), (<$>), (<*>))
import           Control.Concurrent          (forkIO)
import           Control.Monad               (forM, forM_, void)
import           Data.IORef
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Serialize              (Serialize)
import           Data.Serialize              (Serialize)
import qualified Data.Serialize              as Serialize
import qualified Data.Serialize              as Serialize
import           Data.Typeable               (Typeable)
import           FRP.Elerea.Simple


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Packet
import           FRP.Euphoria.Network.Socket


--------------------------------------------------------------------------------
client :: (Serialize is, Serialize id)
       => String
       -> Int
       -> is
       -> (id -> is -> is)
       -> IO (SignalGen (Signal is))
client host port initialIn updateIn = do
    (packetsInGen, packetIn) <- externalMulti
    c <- runClient host port packetIn

    return $ do
        packetsIn <- packetsInGen
        statesIn  <- foldSignal
            (flip $ foldr $ updateWithPacket updateIn) initialIn packetsIn

        return statesIn


--------------------------------------------------------------------------------
data ServerSignals = ServerSignals
    { connectingClients    :: [Client]
    , connectedClients     :: [Client]
    , disconnectingClients :: [Client]
    }


--------------------------------------------------------------------------------
server :: (Serialize os, Serialize od)
       => String
       -> Int
       -> Signal os               -- Maybe this should be: Signal (Client -> os)
       -> Signal [(Client, od)]   -- Maybe this should be: Signal (Client -> od)
       -> IO (SignalGen (Signal ServerSignals))
server host port initialOut deltasOut = do
    (connectsGen, putConnect) <- externalMulti
    (disconnectsGen, putDisconnect) <- externalMulti
    s <- runServer host port putConnect putDisconnect onPacket
    return $ do
        out         <- effectful1 sendDeltasOut deltasOut
        clients     <- effectful $ serverGetClients s
        connects    <- connectsGen
        connects'   <- effectful2 sendInitialOut initialOut connects
        disconnects <- disconnectsGen
        return $ ServerSignals <$>
            connects' <*> (clients <* out) <*> disconnects
  where
    sendInitialOut io cs = do
        putStrLn $ show cs ++ " connected"
        forM_ cs $ \c ->
            clientSend c $ Packet AbsolutePacket (Serialize.encode io)
        return cs

    onPacket _ _ = return ()

    sendDeltasOut ds = forM_ ds $ \(c, d) ->
        clientSend c $ Packet DeltaPacket (Serialize.encode d)


--------------------------------------------------------------------------------
foldSignal :: (a -> b -> b) -> b -> Signal a -> SignalGen (Signal b)
foldSignal f initial x = do
    rec
        let acc = f <$> x <*> acc'
        acc' <- delay initial acc

    return acc


--------------------------------------------------------------------------------
updateWithPacket
    :: (Serialize s, Serialize d)
    => (d -> s -> s)  -- ^ Delta updater
    -> Packet         -- ^ Packet with updating payload
    -> s              -- ^ Initial state
    -> s              -- ^ Resulting state
updateWithPacket update packet initial = case packetType packet of
    AckPacket      -> initial  -- But this should not happen
    AbsolutePacket -> case Serialize.decode (packetData packet) of
        Left  err      -> error err  -- TODO return the initial state?
        Right newstate -> newstate
    DeltaPacket    -> case Serialize.decode (packetData packet) of
        Left err    -> error err  -- TODO probably return initial state
        Right delta -> update delta initial
