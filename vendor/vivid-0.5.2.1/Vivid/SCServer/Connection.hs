{-# LANGUAGE
     BangPatterns
   , LambdaCase
   , OverloadedStrings
   , ViewPatterns

   , NoIncoherentInstances
   , NoMonomorphismRestriction
   , NoUndecidableInstances
   #-}

module Vivid.SCServer.Connection (
     createSCServerConnection'

   , defaultConnectConfig
   , defaultMessageFunction
   , ignoreMessagesFunction

   , SCConnectConfig(..)

   , closeSCServerConnection'

   , ConnProtocol(..)

   , getMailboxForSyncId'

   , getSCServerSocket'

   , waitForSync_io'
   , waitForSync_io_noGC'

   , sendMaybePad

   -- Exported at least for 'Vivid.GlobalState':
   , startMailbox'
   , connectToSCServer'

   -- for tests:
   , splitMessagesFromChunk
   ) where

import Vivid.SC.Server.Commands as SCCmd

import Vivid.OSC
import Vivid.OSC.Bundles (initTreeCommand)
import Vivid.SCServer.State

import Network.Socket (
     SocketType(Datagram , Stream), defaultProtocol, socket
   , AddrInfo(..), getAddrInfo
   -- , AddrInfoFlag(..),
   , defaultHints
   , Socket, HostName, ServiceName, connect, close -- , listen, bind
   -- , bindSocket, accept
   , accept, listen
                                                            
    -- We put this everywhere we do socket actions for Windows compatibility:
   , withSocketsDo
   )

import Network.Socket.ByteString (sendAll, recv)

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.Map as Map
import Data.Monoid
import Data.Serialize (encode, decode)

-- | __You usually don't need to call this function__
-- 
--   Use this if to connect on a non-default port or to a server not at localhost
-- 
--   Otherwise the connection is created when it's needed.
--   You can also use this to explicitly create the connection, so the
--   computation is done upfront
-- 
--   The 'HostName' is the ip address or "localhost". The 'ServiceName' is the port
createSCServerConnection' :: SCServerState -> SCConnectConfig -> IO (Either String Socket)
createSCServerConnection' serverState connConfig = do
   let !_ = serverState
   shouldMakeSock serverState >>= \case
      True -> makeSock serverState connConfig >>= \case
         Just s -> pure $ Right s
         Nothing -> pure $ Left "Unable to create socket"
      False ->
         pure $ Left "Too late -- connection already established. Disconnect first."

-- | Explicitly close Vivid's connection to a SC server.
-- 
--   Day-to-day, you can usually just let your program run without using this.
-- 
--   For example though, if you're running code that uses Vivid in ghci, and
--   you ":r", you'll want to disconnect first -- there are processes running
--   which can step on the toes of your new instance
--   (TODO: this isn't fully true - I ":r" all the time - what do I mean here?)
-- 
--   Also if you want to change the params of your connection (e.g. to connect
--   to a different server), you'll want to disconnect from the other
--   connection first
closeSCServerConnection' :: SCServerState -> IO ()
closeSCServerConnection' serverState = do
   let !_ = serverState
   ish <- atomically $ do
      writeTVar (_scServerState_socketConnectStarted serverState) False
{-
      (,) <$> swapTVar (_scServerState_socket serverState) Nothing
          <*> swapTVar (_scServerState_listener serverState) Nothing
-}
      (,) <$> tryTakeTMVar (_scServerState_socket serverState)
          <*> tryTakeTMVar (_scServerState_listener serverState)

   case ish of
      (Just sock, Just listener) -> do
         killThread listener
         withSocketsDo $ close sock
      (Nothing, Nothing) -> pure ()
      _ -> error "well that's weird"


data SCConnectConfig
   = SCConnectConfig {
    _scConnectConfig_hostName :: HostName
  , _scConnectConfig_port :: ServiceName
  , _scConnectConfig_clientId :: Int32
     -- ^ To prevent NodeId clashes when multiple clients are connected to
     --   the same server, each client should have a separate clientId, which
     --   keeps the nodeId separate. Sclang's default clientId is 0, and ours
     --   is 1, so you can run both at the same time without config.
     --   This number must be between 0 and 31
  , _scConnectConfig_connProtocol :: ConnProtocol
  , _scConnectConfig_password :: Maybe ByteString -- TODO: I don't like that e.g. a password could be provided with UDP, which doesn't use them
  , _scConnectConfig_serverMessageFunction :: OSC -> IO ()
  -- max # of synthdefs -- and clear em out
  }
-- deriving (Show, Read, Eq)


-- | The default _scConnectConfig_clientId is 1, and sclang's is 0, so you should
--   be able to run vivid side-by-side with the SC IDE out of the box.
defaultConnectConfig :: SCConnectConfig
defaultConnectConfig = SCConnectConfig {
     _scConnectConfig_hostName = "127.0.0.1"
   , _scConnectConfig_port = "57110"
   , _scConnectConfig_clientId = 1
   , _scConnectConfig_connProtocol = ConnProtocol_UDP -- ConnProtocol_TCP -- UDP -- TODO: not for release!
   , _scConnectConfig_password = Nothing
   , _scConnectConfig_serverMessageFunction = defaultMessageFunction
   }

-- Internal -- this is what gets called after we check a socket doesn't
-- already exist:
connectToSCServer' :: SCServerState -> SCConnectConfig -> IO (Socket, ThreadId)
connectToSCServer' serverState scConnectConfig = withSocketsDo $ do
   let !_ = serverState
   let hostName = _scConnectConfig_hostName scConnectConfig
       port = _scConnectConfig_port scConnectConfig
       connType = case _scConnectConfig_connProtocol scConnectConfig of
          ConnProtocol_UDP -> Datagram
          ConnProtocol_TCP -> Stream
   (serverAddr:_) <- getAddrInfo (Just (defaultHints { addrSocketType = connType })) {- (Just (defaultHints {addrFlags = [AI_PASSIVE]})) -} (Just hostName) (Just port)
   s <- socket (addrFamily serverAddr) connType (addrProtocol serverAddr) -- defaultProtocol
   setServerClientId serverState (_scConnectConfig_clientId scConnectConfig)
   connect s (addrAddress serverAddr)

   atomically $ do
      writeTVar (_scServerState_serverMessageFunction serverState) $
         _scConnectConfig_serverMessageFunction scConnectConfig
      writeTVar (_scServerState_connProtocol serverState) $
         _scConnectConfig_connProtocol scConnectConfig


   case _scConnectConfig_password scConnectConfig of
      Nothing -> pure ()
      -- This must be the very first message sent to the server:
      Just password -> sendAll {- MaybePad  (_scConnectConfig_connProtocol scConnectConfig) -} s $ password

   listener <- forkIO $ startMailbox' serverState s

   let firstSyncID = toEnum $ numberOfSyncIdsToDrop - 2
   _ <- sendMaybePad (_scConnectConfig_connProtocol scConnectConfig) s $
      encodeOSCBundle $ OSCBundle (Timestamp 0) [
        Right $ SCCmd.dumpOSC DumpOSC_Parsed
      , Right $ initTreeCommand
      , Right $ SCCmd.sync (SyncId firstSyncID)
      ]

   waitForSync_io' serverState (SyncId firstSyncID)

   pure (s, listener)

-- ALWAYS use this instead of 'send':
sendMaybePad :: ConnProtocol -> Socket -> ByteString -> IO ()
sendMaybePad connProtocol socket msg_noPad =
   withSocketsDo $ sendAll socket msg -- TODO: send vs sendAll? Makes a difference?
 where
   msg :: ByteString
   msg = case connProtocol of
      ConnProtocol_UDP -> msg_noPad
      ConnProtocol_TCP -> (encode (toEnum (BS.length msg_noPad) :: Int32))<>msg_noPad

waitForSync_io' :: SCServerState -> SyncId -> IO ()
waitForSync_io' serverState syncId = do
   _ <- readMVar =<< getMailboxForSyncId' serverState syncId
   -- We garbage-collect these so the Map stays small -- but it means you can only wait
   -- for a sync from one place:
   atomically $ modifyTVar' (_scServerState_syncIdMailboxes serverState) $
      Map.delete syncId

waitForSync_io_noGC' :: SCServerState -> SyncId -> IO ()
waitForSync_io_noGC' serverState syncId = do
   _ <- readMVar =<< getMailboxForSyncId' serverState syncId
   pure ()

-- TODO: what's "mailbox" here? Is it like an Erlang mailbox, to receive and
-- dispatch all messages?
startMailbox' :: SCServerState -> Socket -> IO ()
startMailbox' serverState s = do
   let !_ = serverState
   connProtocol <- readTVarIO $ _scServerState_connProtocol serverState :: IO ConnProtocol -- note we only read this once at the beginning. If this can change throughout program run, update this code
   forever $ recv {- From -} s 65536 >>= \(msg_maybePad {- , _ -}) -> do
      case connProtocol of
         ConnProtocol_UDP ->
            handleMsg msg_maybePad
         ConnProtocol_TCP -> do
            case splitMessagesFromChunk msg_maybePad of -- From the 'network' docs: "For TCP sockets, a zero length return value means the peer has closed its half side of the connection."
               Right [] -> errorWithoutStackTrace "Connection to the SC server lost. Probably the server has quit or the network is down. You will need to reconnect."
               Left err -> putStrLn err
               Right msgs ->
                  mapM_ handleMsg msgs
 where
   handleMsg :: ByteString -> IO ()
   handleMsg msg = do
      case decodeOSC msg of
         Right (OSC "/synced" [OSC_I theSyncId]) -> do
            syncBox <- getMailboxForSyncId' serverState (SyncId theSyncId)
            tryPutMVar syncBox () >>= \case
               True -> pure ()
               False ->
                  putStrLn $
                     "That's weird!: we got the same syncId twice: "
                     ++ show theSyncId
         Right x -> do
            otherMessageFunction <- readTVarIO $
               _scServerState_serverMessageFunction serverState
            otherMessageFunction x
         Left e -> do
            putStrLn $ "ERROR DECODING OSC: " ++ show (msg, e)

-- There can be more than one message that we recieve as one binary blob:
splitMessagesFromChunk :: ByteString -> Either String [ByteString]
splitMessagesFromChunk b =
   case BS.splitAt 4 b of
      ("", "") -> Right []
      (sizeBS, rest) ->
         case decode sizeBS :: Either String Int32 of
            Left e -> Left $ "ERROR DECODING OSC PAD: "++show b
            Right (fromEnum -> size) ->
               let (thisChunk, nextBS) = BS.splitAt size rest
               in case BS.length thisChunk == size of
                     False -> Left $ "INCORRECT SIZE: "++show b
                     True ->
                        case splitMessagesFromChunk nextBS of
                           Left e -> Left e
                           Right allRest -> Right (thisChunk : allRest)



-- | Print all messages other than \"/done\"s
defaultMessageFunction :: OSC -> IO ()
defaultMessageFunction = \case
   -- Some examples you might want to handle individually:
   {-
   OSC "/fail" [OSC_S "/blah", OSC_S "Command not found"] -> pure ()
   OSC "/fail" [OSC_S "/s_new", OSC_S "wrong argument type"] -> pure ()
   OSC "/fail" [OSC_S "/b_allocRead", OSC_S "File 'blah.ogg' could not be opened: Error : flac decoder lost sync.\n",OSC_I 2]
   -}
   OSC "/done" [OSC_S _] -> pure ()
   OSC "/done" [OSC_S _, OSC_I _] -> pure ()
   x -> putStrLn $ "Msg from server: " <> show x

-- | If you don't want to hear what the server has to say
ignoreMessagesFunction :: OSC -> IO ()
ignoreMessagesFunction _ = pure ()

-- This is a nice example of when STM can be really helpful -
-- It's impossible! (right?) to have 2 threads create mailboxes and have em overwrite each
-- other -- so we can make a guarantee about recieving a sync that you register for
getMailboxForSyncId' :: SCServerState -> SyncId -> IO (MVar ())
getMailboxForSyncId' serverState syncId = do
   mvarThatIMightWannaUse <- newEmptyMVar -- TODO: make this a TMVar so it doesn't have to sit outside like this?
   m <- atomically $ do
      allMailboxes <- readTVar (_scServerState_syncIdMailboxes serverState)
      case Map.lookup syncId allMailboxes of
         Just syncBox -> pure syncBox
         Nothing -> do
            writeTVar (_scServerState_syncIdMailboxes serverState)
              (Map.insert syncId mvarThatIMightWannaUse allMailboxes)
            pure mvarThatIMightWannaUse
   pure m

getSCServerSocket' :: SCServerState -> IO Socket
getSCServerSocket' scServerState' = do
   let !_ = scServerState'
   shouldMakeSock scServerState' >>= \case
      True -> do
         makeSock scServerState' defaultConnectConfig >>= \case
            Just x -> pure x
            Nothing -> error "Unexpected failure creating socket"
      False -> atomically . readTMVar $ _scServerState_socket scServerState'

shouldMakeSock :: SCServerState -> IO Bool
shouldMakeSock serverState = atomically $ do
   let theVar = _scServerState_socketConnectStarted serverState
   alreadyBeingMade <- readTVar theVar
   case alreadyBeingMade of
      True -> pure False
      False -> do
         writeTVar theVar True
         pure True

makeSock :: SCServerState -> SCConnectConfig -> IO (Maybe Socket)
makeSock serverState connConfig = do
   (sock, listener) <- connectToSCServer' serverState connConfig
   atomically $ (do
      -- writeTVar (_scServerState_socket serverState) $ Just sock
      -- writeTVar (_scServerState_listener serverState) $ Just listener
      a <- tryPutTMVar (_scServerState_socket serverState) sock
      b <- tryPutTMVar (_scServerState_listener serverState) listener
      check $ a && b
      pure $ Just sock)
         `orElse` (pure Nothing)

