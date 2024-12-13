{-# LANGUAGE
     BangPatterns
   , InstanceSigs
   #-}

module Vivid.GlobalState (
     withGlobalSCServerState
   , setClientId
   , setMaxBufferIds
   , waitForSync_io
   , waitForSync_io_noGC
   , startMailbox
   , getMailboxForSyncId
   , connectToSCServer
   , createSCServerConnection
   , closeSCServerConnection
   , getSCServerSocket
   , doScheduledIn
   , doScheduledAt
   , doScheduledNow
   , quitSCServer
   , defineSDFromFile

   -- Not sure we need to export these:
   , getNextAvailables
   , getNextAvailable
   ) where

import Control.Monad (void)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Int
import Network.Socket (Socket)

-- We use this only for "the unsafePerformIO hack"
-- (https://wiki.haskell.org/Top_level_mutable_state) so that functions can
-- refer to the state without being passed the state explicitly. This should
-- still be safe:
import System.IO.Unsafe (unsafePerformIO)

import Vivid.Actions (quitSCServerWith)
import Vivid.Actions.Class (VividAction(..))
import Vivid.Actions.IO (defineSDFromFileWith)
import Vivid.Actions.Scheduled
import Vivid.OSC (Timestamp)
import Vivid.SCServer.Connection
import Vivid.SCServer.State
import Vivid.SynthDef.Types (SynthDef)

{-# NOINLINE globalSCServerState #-}
globalSCServerState :: SCServerState
-- See the above note about this use of unsafePerformIO:
globalSCServerState = unsafePerformIO makeEmptySCServerState

withGlobalSCServerState :: (SCServerState -> x) -> x
withGlobalSCServerState action =
   -- This line is meant to prevent 'nested STM' errors.
   -- Would like a more systematic look at the causes.:
   let !_ = globalSCServerState

   in action globalSCServerState


setClientId :: Int32 -> IO ()
setClientId = withGlobalSCServerState setServerClientId

setMaxBufferIds :: Int32 -> IO ()
setMaxBufferIds = withGlobalSCServerState setServerMaxBufferIds

getNextAvailable :: (SCServerState -> TVar [a]) -> IO a
getNextAvailable = withGlobalSCServerState getNextAvailable'

getNextAvailables :: Int -> (SCServerState -> TVar [a]) -> IO [a]
getNextAvailables = withGlobalSCServerState getNextAvailables'


closeSCServerConnection :: IO ()
closeSCServerConnection = withGlobalSCServerState closeSCServerConnection'

connectToSCServer :: SCConnectConfig -> IO (Socket, ThreadId)
connectToSCServer = withGlobalSCServerState connectToSCServer'

waitForSync_io :: SyncId -> IO ()
waitForSync_io = withGlobalSCServerState waitForSync_io'

waitForSync_io_noGC :: SyncId -> IO ()
waitForSync_io_noGC = withGlobalSCServerState waitForSync_io_noGC'

startMailbox :: Socket -> IO ()
startMailbox = withGlobalSCServerState startMailbox'

getMailboxForSyncId :: SyncId -> IO (MVar ())
getMailboxForSyncId = withGlobalSCServerState getMailboxForSyncId'

createSCServerConnection :: SCConnectConfig -> IO (Either String Socket)
createSCServerConnection = withGlobalSCServerState createSCServerConnection'

getSCServerSocket :: IO Socket
getSCServerSocket = withGlobalSCServerState getSCServerSocket'

defineSDFromFile :: SynthDef a -> IO ()
defineSDFromFile = withGlobalSCServerState defineSDFromFileWith

doScheduledAt :: Timestamp -> Scheduled a -> IO a
doScheduledAt =
   withGlobalSCServerState doScheduledAtWith

doScheduledIn :: Double -> Scheduled a -> IO a
doScheduledIn =
   withGlobalSCServerState doScheduledInWith

doScheduledNow :: Scheduled a -> IO a
doScheduledNow =
   withGlobalSCServerState doScheduledNowWith

quitSCServer :: IO ()
quitSCServer = withGlobalSCServerState quitSCServerWith

instance VividAction IO where
   callBS :: ByteString -> IO ()
   callBS bs = globalIO $ callBS bs

   sync :: IO ()
   sync = globalIO sync

   waitForSync :: SyncId -> IO ()
   waitForSync syncId = globalIO $ waitForSync syncId

   wait :: Real n => n -> IO ()
   wait n = globalIO $ wait n

   getTime :: IO Timestamp
   getTime = globalIO getTime

   newBufferId :: IO BufferId
   newBufferId = globalIO newBufferId

   newNodeId :: IO NodeId
   newNodeId = globalIO newNodeId

   newSyncId :: IO SyncId
   newSyncId = globalIO newSyncId

   fork :: IO () -> IO ()
   fork = void . forkIO

   defineSD :: SynthDef a -> IO ()
   defineSD synthDef = globalIO $ defineSD synthDef

globalIO :: (ReaderT SCServerState IO x) -> IO x
globalIO action =
   withGlobalSCServerState (runReaderT action)

