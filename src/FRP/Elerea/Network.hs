--------------------------------------------------------------------------------
{-# LANGUAGE DoRec #-}
module FRP.Elerea.Network
    ( client

    , ServerSignals (..)
    , server
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative         ((<*), (<$>), (<*>))
import           Control.Monad               (forM_)
import           Data.List                   ((\\))
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              (Serialize)
import qualified Data.Serialize              as Serialize
import           FRP.Elerea.Simple


--------------------------------------------------------------------------------
import           FRP.Elerea.Network.Packet
import           FRP.Elerea.Network.Socket


--------------------------------------------------------------------------------
sendAbsolute :: Serialize s => Client -> s -> IO ()
sendAbsolute c s = clientSend c $ Packet AbsolutePacket $ Serialize.encode s


--------------------------------------------------------------------------------
sendDelta :: Serialize d => Client -> Maybe d -> IO ()
sendDelta _ Nothing  = return ()
sendDelta c (Just d) = clientSend c $ Packet DeltaPacket $ Serialize.encode d


--------------------------------------------------------------------------------
client :: (Serialize is, Serialize id, Serialize os, Serialize od)
       => String
       -> Int
       -> is
       -> (id -> is -> is)
       -> Signal os
       -> Signal (Maybe od)
       -> IO (SignalGen (Signal is))
client host port initialIn updateIn initialOut deltaOut = do
    (packetsInGen, packetIn) <- externalMulti
    c <- runClient host port packetIn

    return $ do
        packetsIn <- packetsInGen
        statesIn  <- foldSignal
            (flip $ foldr $ updateWithPacket updateIn) initialIn packetsIn

        -- This function first sends the initial out, end then deltas...
        send <- stateful (sendAbsolute c . fst) (const $ sendDelta c . snd)
        out  <- effectful1 id $ send <*> ((,) <$> initialOut <*> deltaOut)

        return $ statesIn <* out


--------------------------------------------------------------------------------
data ServerSignals is = ServerSignals
    { connectingClients    :: [Client]
    , connectedClients     :: Map Client is
    , disconnectingClients :: [Client]
    } deriving (Show)


--------------------------------------------------------------------------------
server :: (Serialize is, Serialize id, Serialize os, Serialize od)
       => String
       -> Int
       -> is
       -> (id -> is -> is)
       -> Signal (Client -> os)
       -> Signal (Client -> Maybe od)
       -> IO (SignalGen (Signal (ServerSignals is)))
server host port initialIn updateIn initialOut deltaOut = do
    (connectsGen, putConnect) <- externalMulti
    (disconnectsGen, putDisconnect) <- externalMulti
    (packetsInGen, putPacketIn) <- externalMulti
    s <- runServer host port putConnect putDisconnect (curry putPacketIn)
    return $ do
        -- List of connecting clients at this point: send the initial out
        connects  <- connectsGen
        connects' <- effectful2 sendInitialOut initialOut connects

        -- List of already connected clients: send the delta
        clist <- effectful $ serverGetClients s
        let clist' = (\\) <$> clist <*> connects
        out <- effectful2 sendDeltaOut deltaOut clist'

        -- The 'in' states for clients
        packetsIn <- packetsInGen
        rec
            let clients = updateClients <$> clients' <*> clist' <*> packetsIn
            clients' <- delay M.empty clients

        -- List of disconnecting clients
        disconnects <- disconnectsGen

        return $ ServerSignals <$>
            connects' <*> (clients <* out) <*> disconnects
  where
    sendInitialOut io cs = do
        putStrLn $ show cs ++ " connected"
        forM_ cs $ \c -> sendAbsolute c (io c)
        return cs

    sendDeltaOut d cs = forM_ cs $ \c -> sendDelta c (d c)

    -- Update all the client states using a fold over the arrived packets
    updateClients previousMap clients packets =
        let initialMap = M.fromList
                [ (c, fromMaybe initialIn (M.lookup c previousMap))
                | c <- clients
                ]
        in foldr (\(c, p) -> M.update (Just . updateWithPacket updateIn p) c)
                initialMap packets

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
