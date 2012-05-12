websockets example
==================

This is the Haskell implementation of the example for the WebSockets library. We
implement a simple multi-user chat program. A live demo of the example is
available [here](http://jaspervdj.be/websockets-example). In order to understand
this example, keep the [reference](http://jaspervdj.be/websockets/reference)
nearby to check out the functions we use.

> {-# LANGUAGE OverloadedStrings #-}
> import Data.Char (isPunctuation, isSpace)
> import Data.Monoid (mappend)
> import Data.Text (Text)
> import Control.Concurrent (forkIO, MVar, newMVar, readMVar)
> import qualified Control.Concurrent.MVar.Lifted as Lifted
> import Control.Exception
> import qualified Control.Exception.Lifted as Lifted
> import Control.Monad (forM_)
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans (lift)
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as T
> import qualified Data.Text.IO as T
> import qualified Data.Conduit as C
> import qualified Data.Conduit.Network as Net

> import qualified Network.WebSockets.Conduit as WS
> import qualified Network.WebSockets.Monad as WS
> import Network.Wai.Handler.Warp
> import Network.Wai.Application.Static

We represent a client by his username and a 'WS.Sender'. We can use this sender
to asynchronously send 'Text' to the client later. Note that using `WS.Hybi00`
here does not imply that our server is only compatible with the `hybi-00`
version of the protocol, for more details on this, see the
[Network.WebSockets](http://jaspervdj.be/websockets/reference/Network-WebSockets.html) 
reference.

> type Client = (Text, WS.Sender)

The state kept on the server is simply a list of connected clients. We've added
an alias and some utility functions, so it will be easier to extend this state
later on.

> type ServerState = [Client]

Create a new, initial state

> newServerState :: ServerState
> newServerState = []

Get the number of active clients

> numClients :: ServerState -> Int
> numClients = length

Check if a user already exists (based on username)

> clientExists :: Client -> ServerState -> Bool
> clientExists client = any ((== fst client) . fst)

Add a client (first, you should verify the client is not already connected using
'clientExists')

> addClient :: Client -> ServerState -> ServerState
> addClient client clients = client : clients

Remove a client

> removeClient :: Client -> ServerState -> ServerState
> removeClient client = filter ((/= fst client) . fst)

Send a message to all clients, and log it on stdout.

> broadcast :: Text -> ServerState -> WS.Server ()
> broadcast message clients = do
>     liftIO $ T.putStrLn message
>     forM_ clients $ \(_, sender) -> lift $ sender $ T.encodeUtf8 message

The main function first creates a new state for the server, then spawns the
actual server. For this purpose, we use the simple server provided by
'WS.runServer'.

> main :: IO ()
> main = do
>     state <- newMVar newServerState
>     let app = WS.runServerT (application state)
>     _ <- forkIO $ C.runResourceT $
>         Net.runTCPServer
>             (Net.ServerSettings 9161 Net.HostAny)
>             app
>     let interceptDefault = WS.intercept (undefined::WS.Hybi00) (WS.WebSocketsOptions (return ()))
>     runSettings defaultSettings
>         { settingsPort = 9160
>         , settingsIntercept = interceptDefault (const app)
>         } $ staticApp defaultFileServerSettings
>               { ssIndices = ["client.html"]
>               }

When a client connects, we accept the connection, regardless of the path.

> application :: MVar ServerState -> WS.Server ()
> application state = do

If we want to be able to send data to this client later, from another thread, we
obtain a sink. We will add this to the server state later.

>     sender <- WS.getSender

When a client is succesfully connected, we read the first message. This should
be in the format of "Hi, I am Jasper", where Jasper is the requested username.

>     msg <- WS.recvUtf8
>     clients <- liftIO $ readMVar state
>     case msg of

Check that the first message has the right format

>         _   | not (prefix `T.isPrefixOf` msg) ->
>                 WS.sendUtf8 "Wrong announcement"

Check the validity of the username

>             | any ($ fst client)
>                 [T.null, T.any isPunctuation, T.any isSpace] ->
>                     WS.sendUtf8 ("Name cannot " `mappend`
>                         "contain punctuation or whitespace, and " `mappend`
>                         "cannot be empty")

Check that the given username is not already taken

>             | clientExists client clients ->
>                 WS.sendUtf8 "User already exists"

All is right!

>             | otherwise -> do

We send a "Welcome!", according to our own little protocol. We add the client to
the list and broadcast the fact that he has joined. Then, we give control to the
'talk' function.

>                Lifted.modifyMVar_ state $ \s -> do
>                    let s' = addClient client s
>                    WS.sendUtf8 $
>                        "Welcome! Users: " `mappend`
>                        T.intercalate ", " (map fst s)
>                    broadcast (fst client `mappend` " joined") s'
>                    return s'
>                talk state client
>           where
>             prefix = "Hi! I am "
>             client = (T.drop (T.length prefix) msg, sender)

The talk function continues to read messages from a single client until he
disconnects. All messages are broadcasted to the other clients.

> talk :: MVar ServerState -> Client -> WS.Server ()
> talk state client@(user, _) = do
>     msg <- WS.recvUtf8
>     Lifted.readMVar state >>= broadcast
>         (user `mappend` ": " `mappend` msg)
>     talk state client
>     `Lifted.catch` disconnect
>   where
>     disconnect e = do
>         liftIO $ print (e::SomeException)
>         Lifted.modifyMVar_ state $ \s -> do
>             let s' = removeClient client s
>             broadcast (user `mappend` " disconnected") s'
>             return s'
