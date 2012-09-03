--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module FRP.Euphoria.Network.Packet
    ( PacketType (..)
    , Packet (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Monad            (guard)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    ()
import           Data.Serialize           (Serialize (..))
import qualified Data.Serialize           as Serialize
import           Data.Word                (Word32)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Tag


--------------------------------------------------------------------------------
data PacketType
    = AbsolutePacket
    | DeltaPacket
    | AckPacket
    deriving (Show)


--------------------------------------------------------------------------------
instance Serialize PacketType where
    get = do
        bytes <- Serialize.getBytes 4
        case bytes of
            "ABSL" -> return AbsolutePacket
            "DELT" -> return DeltaPacket
            "ACK " -> return AckPacket
            _      -> fail $ "Unknown PacketType: " ++ show bytes


    put AbsolutePacket = Serialize.putByteString "ABSL"
    put DeltaPacket    = Serialize.putByteString "DELT"
    put AckPacket      = Serialize.putByteString "ACK "


--------------------------------------------------------------------------------
data Packet = Packet
    { packetType    :: PacketType
    , packetChannel :: Tag
    , packetSeqNo   :: Word32
    , packetData    :: ByteString
    } deriving (Show)


--------------------------------------------------------------------------------
instance Serialize Packet where
    get = do
        check   <- Serialize.getBytes 8
        guard (check == "EUPHORIA")
        type'   <- Serialize.get
        channel <- Serialize.get
        seqNo   <- Serialize.getWord32be
        len     <- Serialize.remaining
        data'   <- Serialize.getBytes (fromIntegral len)
        return $ Packet type' channel seqNo data'

    put (Packet type' channel seqNo data') = do
        Serialize.putByteString "EUPHORIA"
        Serialize.put           type'
        Serialize.put           channel
        Serialize.putWord32be   seqNo
        Serialize.putByteString data'
