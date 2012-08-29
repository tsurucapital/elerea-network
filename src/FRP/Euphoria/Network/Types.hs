--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Euphoria.Network.Types
    ( BeamType
    , beamType
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Digest.Pure.SHA       (bytestringDigest, sha1)
import           Data.Serialize             (Serialize)
import           Data.Typeable              (Typeable, showsTypeRep, typeOf)


--------------------------------------------------------------------------------
newtype BeamType = BeamType ByteString
    deriving (Eq, Ord, Serialize, Show, Typeable)


--------------------------------------------------------------------------------
beamType :: Typeable a => a -> BeamType
beamType x = BeamType $ sha1' $ showsTypeRep (typeOf x) ""
  where
    sha1' = B.concat . BL.toChunks . bytestringDigest . sha1 . BLC.pack
