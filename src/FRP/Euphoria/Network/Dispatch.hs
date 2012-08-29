--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module FRP.Euphoria.Network.Dispatch
    ( Callback (..)
    , Dispatch (..)

    , mkDispatch
    , dispatchAdd
    , dispatchCall
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative        ((<$>), (<*>))
import           Control.Concurrent.MVar
import           Control.Monad              (forM_)
import           Data.Int                   (Int64)
import           Data.IORef
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Typeable              (Typeable)
import           Unsafe.Coerce              (unsafeCoerce)


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Types


--------------------------------------------------------------------------------
data Callback = forall a. Callback
    { callbackId  :: Int64
    , callbackFun :: a -> IO ()
    }


--------------------------------------------------------------------------------
data Dispatch = Dispatch
    { dispatchNextId :: IORef Int64
    , dispatchMap    :: MVar (Map BeamType [Callback])
    }


--------------------------------------------------------------------------------
mkDispatch :: IO Dispatch
mkDispatch = Dispatch <$> newIORef 0 <*> newMVar M.empty


--------------------------------------------------------------------------------
dispatchAdd :: forall a. Typeable a
            => Dispatch -> (a -> IO ()) -> IO (IO ())
dispatchAdd dispatch cb = do
    id' <- atomicModifyIORef (dispatchNextId dispatch) $ \x -> (x + 1, x)

    modifyMVar_ (dispatchMap dispatch) $
        return . M.insertWith (++) bt [Callback id' cb]

    return $ dispatchRemove dispatch bt id'
  where
    bt = beamType (undefined :: a)


--------------------------------------------------------------------------------
dispatchRemove :: Dispatch -> BeamType -> Int64 -> IO ()
dispatchRemove dispatch bt id' = do
    putStrLn $ "Removing callback " ++ show id'
    modifyMVar_ (dispatchMap dispatch) $
        return . M.update remove bt
  where
    remove cs = case filter ((/= id') . callbackId) cs of
        []  -> Nothing
        cs' -> Just cs'


--------------------------------------------------------------------------------
dispatchCall :: Dispatch -> BeamType -> a -> IO ()
dispatchCall (Dispatch _ d) bt x = do
    cbs <- fromMaybe [] . M.lookup bt <$> readMVar d
    forM_ cbs $ \(Callback _ f) -> f (unsafeCoerce x)
