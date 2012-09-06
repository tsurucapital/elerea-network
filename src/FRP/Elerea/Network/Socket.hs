--------------------------------------------------------------------------------
-- | Low-level socket stuff
{-# LANGUAGE DeriveDataTypeable #-}
module FRP.Elerea.Network.Socket
    ( Client
    , clientDisconnect
    , clientSend

    , Server
    , serverGetClients

    , runServer
    , runClient
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder       as Builder
import           Control.Applicative            ((<$>), (<*>))
import           Control.Concurrent             (forkIO, myThreadId)
import           Control.Concurrent.MVar
import           Control.Exception              (Exception, SomeException,
                                                 handle, throwTo)
import           Control.Monad                  (forever, void)
import qualified Data.Attoparsec                as A
import qualified Data.ByteString                as B
import           Data.IORef
import           Data.Ord                       (comparing)
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Typeable                  (Typeable)
import           Data.Word                      (Word64)
import qualified Network.Socket                 as S
import qualified Network.Socket.ByteString      as SB
import qualified Network.Socket.ByteString.Lazy as SBL


--------------------------------------------------------------------------------
import           FRP.Elerea.Network.Packet


--------------------------------------------------------------------------------
type ReadState = A.Result Packet


--------------------------------------------------------------------------------
emptyReadState :: ReadState
emptyReadState = A.parse parsePacket B.empty


--------------------------------------------------------------------------------
readPacket :: S.Socket -> ReadState -> IO (Maybe (Packet, ReadState))
readPacket socket readState = do
    chunk <- SB.recv socket 1024
    if B.null chunk
        then return Nothing  -- EOF
        else case A.feed readState chunk of
                A.Fail _ _ _ -> return Nothing
                A.Done _ x   -> return $ Just (x, emptyReadState)
                partial      -> readPacket socket partial


--------------------------------------------------------------------------------
data Client = Client
    { clientId     :: Word64
    , clientSocket :: S.Socket
    }


--------------------------------------------------------------------------------
instance Eq Client where
    c1 == c2 = clientId c1 == clientId c2


--------------------------------------------------------------------------------
instance Ord Client where
    compare = comparing clientId


--------------------------------------------------------------------------------
instance Show Client where
    show (Client id' _) = "(Client " ++ show id' ++ " _)"


--------------------------------------------------------------------------------
clientDisconnect :: Client -> IO ()
clientDisconnect client = S.sClose (clientSocket client)


--------------------------------------------------------------------------------
clientSend :: Client -> Packet -> IO ()
clientSend client packet = handle ignoreExceptions $
    SBL.sendAll (clientSocket client) $
        Builder.toLazyByteString $ buildPacket packet


--------------------------------------------------------------------------------
data Server = Server
    { serverNextClientId :: IORef Word64
    , serverClients      :: MVar (Set Client)
    }


--------------------------------------------------------------------------------
mkServer :: IO Server
mkServer = Server <$> newIORef 0 <*> newMVar S.empty


--------------------------------------------------------------------------------
serverGetClients :: Server -> IO [Client]
serverGetClients = fmap S.toList . readMVar . serverClients


--------------------------------------------------------------------------------
serverAddClient :: Server -> S.Socket -> IO Client
serverAddClient server socket = do
    id' <- atomicModifyIORef (serverNextClientId server) $ \i -> (i + 1, i)
    let client = Client id' socket
    modifyMVar_ (serverClients server) (return . S.insert client)
    return client


--------------------------------------------------------------------------------
serverRemoveClient :: Server -> Client -> IO ()
serverRemoveClient server client =
    modifyMVar_ (serverClients server) $ return . S.delete client


--------------------------------------------------------------------------------
runServer :: String                       -- ^ Host
          -> Int                          -- ^ Port
          -> (Client -> IO ())            -- ^ Connect handler
          -> (Client -> IO ())            -- ^ Disconnect handler
          -> (Client -> Packet -> IO ())  -- ^ Packet handler
          -> IO Server
runServer host port onConnect onDisconnect onPacket = do
    server <- mkServer
    socket <- S.socket S.AF_INET S.Stream S.defaultProtocol
    _      <- S.setSocketOption socket S.ReuseAddr 1
    host'  <- S.inet_addr host
    S.bindSocket socket $ S.SockAddrInet (fromIntegral port) host'
    S.listen socket 5

    void $ forkIO $ forever $ do
        (conn, _) <- S.accept socket
        _         <- forkIO $ do
            client <- serverAddClient server conn
            onConnect client
            handle ignoreExceptions $ readLoop client onPacket
            serverRemoveClient server client
            onDisconnect client
        return ()

    return server


--------------------------------------------------------------------------------
data ElereaConnectionClosedException = ElereaConnectionClosedException
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Exception ElereaConnectionClosedException


--------------------------------------------------------------------------------
-- | Start a client in the background. This thread will receive an exception if
-- there is an unexpected disconnect.
runClient :: String             -- ^ Host
          -> Int                -- ^ Port
          -> (Packet -> IO ())  -- ^ Packet handler
          -> IO Client
runClient host port onPacket = do
    socket <- S.socket S.AF_INET S.Stream S.defaultProtocol
    host'  <- S.inet_addr host
    S.connect socket $ S.SockAddrInet (fromIntegral port) host'

    myThread <- myThreadId
    let client  = Client 0 socket
        rethrow = throwTo myThread

    _ <- forkIO $ handle rethrow $ do
        readLoop client (\_ packet -> onPacket packet)
        rethrow ElereaConnectionClosedException

    return client


--------------------------------------------------------------------------------
readLoop :: Client -> (Client -> Packet -> IO ()) -> IO ()
readLoop client onPacket = go emptyReadState
  where
    go state = do
        result <- readPacket (clientSocket client) state
        case result of
            Nothing               -> S.sClose (clientSocket client)
            Just (packet, state') -> onPacket client packet >> go state'


--------------------------------------------------------------------------------
-- | Ignore All Exceptions (TM)
ignoreExceptions :: SomeException -> IO ()
ignoreExceptions _ = return ()
