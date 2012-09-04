--------------------------------------------------------------------------------
-- | Low-level socket stuff
module FRP.Euphoria.Network.Socket
    ( Client
    , clientDisconnect
    , clientSend

    , runServer
    , runClient
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder       as Builder
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.MVar
import           Control.Exception              (SomeException, handle)
import           Control.Monad                  (forever)
import qualified Data.Attoparsec                as A
import qualified Data.ByteString                as B
import           Data.Ord                       (comparing)
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Word                      (Word64)
import qualified Network.Socket                 as S
import qualified Network.Socket.ByteString      as SB
import qualified Network.Socket.ByteString.Lazy as SBL


--------------------------------------------------------------------------------
import           FRP.Euphoria.Network.Packet


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
data ServerState = ServerState
    { serverNextClientId :: !Word64
    , serverClients      :: Set Client
    }


--------------------------------------------------------------------------------
clientDisconnect :: Client -> IO ()
clientDisconnect client = S.sClose (clientSocket client)


--------------------------------------------------------------------------------
clientSend :: Client -> Packet -> IO ()
clientSend client packet = SBL.sendAll (clientSocket client) $
    Builder.toLazyByteString $ buildPacket packet


--------------------------------------------------------------------------------
emptyServerState :: ServerState
emptyServerState = ServerState 0 S.empty


--------------------------------------------------------------------------------
serverAddClient :: S.Socket -> ServerState -> (ServerState, Client)
serverAddClient socket server =
    let id' = serverNextClientId server
    in (server {serverNextClientId = id' + 1}, Client id' socket)


--------------------------------------------------------------------------------
serverRemoveClient :: Client -> ServerState -> ServerState
serverRemoveClient client s =
    s {serverClients = S.delete client (serverClients s)}


--------------------------------------------------------------------------------
-- TODO: Fork not block
runServer :: String                       -- ^ Host
          -> Int                          -- ^ Port
          -> (Client -> IO ())            -- ^ Connect handler
          -> (Client -> IO ())            -- ^ Disconnect handler
          -> (Client -> Packet -> IO ())  -- ^ Packet handler
          -> IO ()
runServer host port onConnect onDisconnect onPacket = do
    state  <- newMVar emptyServerState
    socket <- S.socket S.AF_INET S.Stream S.defaultProtocol
    _      <- S.setSocketOption socket S.ReuseAddr 1
    host'  <- S.inet_addr host
    S.bindSocket socket $ S.SockAddrInet (fromIntegral port) host'
    S.listen socket 5
    forever $ do
        (conn, _) <- S.accept socket
        _         <- forkIO $ do
            client <- modifyMVar state $ return . serverAddClient conn
            onConnect client
            handle handler $ readLoop client onPacket
            modifyMVar_ state $ return . serverRemoveClient client
            onDisconnect client
        return ()
  where
    -- Ignore All Exceptions (TM)
    handler :: SomeException -> IO ()
    handler _ = return ()


--------------------------------------------------------------------------------
runClient :: String             -- ^ Host
          -> Int                -- ^ Port
          -> (Packet -> IO ())  -- ^ Packet handler
          -> IO Client
runClient host port onPacket = do
    socket <- S.socket S.AF_INET S.Stream S.defaultProtocol
    host'  <- S.inet_addr host
    S.connect socket $ S.SockAddrInet (fromIntegral port) host'

    let client = Client 0 socket
    _ <- forkIO $ readLoop client (\_ packet -> onPacket packet)
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
