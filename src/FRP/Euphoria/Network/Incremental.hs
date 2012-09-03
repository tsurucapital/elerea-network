--------------------------------------------------------------------------------
-- | Some code to support incremental updates
module FRP.Euphoria.Network.Incremental
    ( incremental
    ) where


--------------------------------------------------------------------------------
import           Data.Serialize              (Serialize)
import qualified Data.Serialize              as Serialize


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Packet


--------------------------------------------------------------------------------
-- | TODO: Packets should be ordered and filtered before being passed to this
-- function
incremental :: (Serialize a, Serialize d)
            => (d -> a -> a)  -- ^ Delta updater
            -> Packet         -- ^ Packet with updating payload
            -> a              -- ^ Initial state
            -> a              -- ^ Resulting state
incremental update packet initial = case packetType packet of
    AckPacket      -> initial  -- But this should not happen
    AbsolutePacket -> case Serialize.decode (packetData packet) of
        Left  err      -> error err  -- TODO return the initial state?
        Right newstate -> newstate
    DeltaPacket    -> case Serialize.decode (packetData packet) of
        Left err    -> error err  -- TODO probably return initial state
        Right delta -> update delta initial
