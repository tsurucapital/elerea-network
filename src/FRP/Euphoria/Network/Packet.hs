--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module FRP.Euphoria.Network.Packet
    (
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         (guard)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import           Data.ByteString.Char8
import           Data.Serialize        (Serialize (..))
import qualified Data.Serialize        as Serialize
import           Data.Word             (Word32)


--------------------------------------------------------------------------------
data PacketType
    = Data
    | Ack
    deriving (Show)


--------------------------------------------------------------------------------
instance Serialize PacketType where
    get = do
        bytes <- Serialize.getBytes 4
        case bytes of
            "DATA" -> return Data
            "ACK " -> return Ack
            _      -> fail $ "Unknown PacketType: " ++ show bytes


    put Data = Serialize.putByteString "DATA"
    put Ack  = Serialize.putByteString "ACK "


--------------------------------------------------------------------------------
data Packet = Packet
    { packetType  :: PacketType
    , packetSeqNo :: Word32
    , packetData  :: ByteString
    } deriving (Show)


--------------------------------------------------------------------------------
instance Serialize Packet where
    get = do
        check <- Serialize.getBytes 8
        guard (check == "EUPHORIA")
        type' <- Serialize.get
        seqNo <- Serialize.getWord32be
        len   <- Serialize.getWord32be
        data' <- Serialize.getBytes (fromIntegral len)

        return $ Packet type' seqNo data'

    put (Packet type' seqNo data') = do
        Serialize.putByteString "EUPHORIA"
        Serialize.put           type'
        Serialize.putWord32be   seqNo
        Serialize.putWord32be   (fromIntegral $ B.length data')
        Serialize.putByteString data'
