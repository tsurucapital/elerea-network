--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FRP.Euphoria.Network.Dispatch
    ( Dispatch
    , mkDispatch
    , dispatchAdd
    , dispatchCall
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative        ((<$>))
import           Control.Concurrent.MVar
import           Control.Monad              (forM_)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Typeable              (Typeable)
import           Unsafe.Coerce              (unsafeCoerce)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Types


--------------------------------------------------------------------------------
data Callback = forall a. Callback (a -> IO ())


--------------------------------------------------------------------------------
newtype Dispatch = Dispatch (MVar (Map BeamType [Callback]))


--------------------------------------------------------------------------------
mkDispatch :: IO Dispatch
mkDispatch = Dispatch <$> newMVar M.empty


--------------------------------------------------------------------------------
dispatchAdd :: forall a. Typeable a => Dispatch -> (a -> IO ()) -> IO ()
dispatchAdd (Dispatch d) cb = modifyMVar_ d $
    return .  M.insertWith (++) bt [Callback cb]
  where
    bt = beamType (undefined :: a)


--------------------------------------------------------------------------------
dispatchCall :: Dispatch -> BeamType -> a -> IO ()
dispatchCall (Dispatch d) bt x = do
    cbs <- fromMaybe [] . M.lookup bt <$> readMVar d
    forM_ cbs $ \(Callback f) -> f (unsafeCoerce x)
