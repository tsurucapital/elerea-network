--------------------------------------------------------------------------------
{-# LANGUAGE DoRec #-}
module FRP.Euphoria.Network
    ( client
    , server
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative         ((*>), (<$>), (<*>))
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
server :: (Serialize os, Serialize od)
       => String
       -> Int
       -> os
       -> Signal [(Client, od)]
       -> IO (SignalGen (Signal [Client]))
server host port initialOut deltasOut = do
    s <- runServer host port onConnect onDisconnect onPacket
    return $ do
        out     <- effectful1 sendDeltasOut deltasOut
        clients <- effectful $ serverGetClients s
        return $ out *> clients
  where
    onConnect c = do
        putStrLn $ show c ++ " disconnected"
        clientSend c $ Packet AbsolutePacket (Serialize.encode initialOut)

    onDisconnect c = putStrLn $ show c ++ " disconnected"

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
