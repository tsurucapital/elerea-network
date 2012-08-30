--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Euphoria.Network.Types
    ( BeamType
    , beamType
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative        ((<$>), (<*>))
import           Data.Bits                  (shiftR)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Digest.Pure.SHA       (integerDigest, sha1)
import           Data.Serialize             (Serialize (..))
import qualified Data.Serialize             as Serialize
import           Data.Typeable              (Typeable, showsTypeRep, typeOf)
import           Data.Word                  (Word64)


--------------------------------------------------------------------------------
data BeamType = BeamType {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
    deriving (Eq, Ord, Show, Typeable)


--------------------------------------------------------------------------------
instance Serialize BeamType where
    get                  =
        BeamType <$> Serialize.getWord64be <*> Serialize.getWord64be
    put (BeamType w1 w2) =
        Serialize.putWord64be w1 >> Serialize.putWord64be w2


--------------------------------------------------------------------------------
-- | Determine a strict 128-bit hash
beamType :: Typeable a => a -> BeamType
beamType x = BeamType w1 w2
  where
    digest  = sha1 $ BLC.pack $ showsTypeRep (typeOf x) ""
    integer = integerDigest digest
    w1      = fromInteger $ integer `shiftR` 64
    w2      = fromInteger integer
