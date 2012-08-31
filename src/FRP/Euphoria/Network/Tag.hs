--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Euphoria.Network.Tag
    ( Tag
    , typeTag
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
data Tag = Tag {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
    deriving (Eq, Ord, Show, Typeable)


--------------------------------------------------------------------------------
instance Serialize Tag where
    get             = Tag <$> Serialize.getWord64be <*> Serialize.getWord64be
    put (Tag w1 w2) = Serialize.putWord64be w1 >> Serialize.putWord64be w2


--------------------------------------------------------------------------------
-- | Determine a strict 128-bit hash
typeTag :: Typeable a => a -> Tag
typeTag x = Tag w1 w2
  where
    digest  = sha1 $ BLC.pack $ showsTypeRep (typeOf x) ""
    integer = integerDigest digest
    w1      = fromInteger $ integer `shiftR` 64
    w2      = fromInteger integer
