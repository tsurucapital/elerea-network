--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module FRP.Euphoria.Network.Packet
    ( PacketType (..)
    , Packet (..)
    , parsePacket
    , buildPacket
    ) where


--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import           Control.Applicative      ((<$>))
import qualified Data.Attoparsec          as A
import           Data.Bits                (shiftL, (.|.))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.Monoid              (mappend)


--------------------------------------------------------------------------------
data PacketType
    = AbsolutePacket
    | DeltaPacket
    | AckPacket
    deriving (Show)


--------------------------------------------------------------------------------
parsePacketType :: A.Parser PacketType
parsePacketType = do
    bytes <- A.take 4
    case bytes of
        "ABSL" -> return AbsolutePacket
        "DELT" -> return DeltaPacket
        "ACK " -> return AckPacket
        _      -> fail $ "Unknown PacketType: " ++ show bytes


--------------------------------------------------------------------------------
buildPacketType :: PacketType -> Builder
buildPacketType AbsolutePacket = Builder.fromByteString "ABSL"
buildPacketType DeltaPacket    = Builder.fromByteString "DELT"
buildPacketType AckPacket      = Builder.fromByteString "ACK "


--------------------------------------------------------------------------------
data Packet = Packet
    { packetType :: PacketType
    , packetData :: ByteString
    } deriving (Show)


--------------------------------------------------------------------------------
-- | TODO: Some nice trick to encode the length compactly
parseLength :: A.Parser Int
parseLength = do
    b1 <- fromIntegral <$> A.anyWord8
    b2 <- fromIntegral <$> A.anyWord8
    b3 <- fromIntegral <$> A.anyWord8
    b4 <- fromIntegral <$> A.anyWord8
    return $ (b1 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 8) .|. b4


--------------------------------------------------------------------------------
buildLength :: Int -> Builder
buildLength = Builder.fromWord32be . fromIntegral


--------------------------------------------------------------------------------
parsePacket :: A.Parser Packet
parsePacket = do
    type' <- parsePacketType
    len   <- parseLength
    data' <- A.take len
    return $ Packet type' data'


--------------------------------------------------------------------------------
buildPacket :: Packet -> Builder
buildPacket (Packet type' data') =
    buildPacketType type'        `mappend`
    buildLength (B.length data') `mappend`
    Builder.fromByteString data'
