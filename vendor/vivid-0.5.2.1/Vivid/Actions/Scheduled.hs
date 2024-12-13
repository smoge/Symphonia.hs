-- | This is for timing of actions that's more precise than IO
-- 
--   It tells the server when to perform the actions, so musical timing won't
--   be affected by e.g. network latency or the time it took to compute a value
-- 
--   If you're running vivid on a different computer than the SC synth, make
--   sure the clocks agree

-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE
     FlexibleInstances
   , InstanceSigs
   , KindSignatures
   , TypeSynonymInstances
   #-}
-- {-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.Actions.Scheduled (
     Scheduled

   , doScheduledInWith

   , doScheduledAtWith

   , doScheduledNowWith
   ) where

import Vivid.Actions.Class
import Vivid.Actions.IO () -- Just until we remove MonadIO
import Vivid.OSC
import Vivid.SCServer
import Vivid.SynthDef (SynthDef)

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, mapReaderT)
import Control.Monad.State (evalStateT, put, get, modify, StateT)
import Data.ByteString (ByteString)
import Prelude

type Scheduled = ReaderT SCServerState (StateT Timestamp IO)


instance VividAction Scheduled where
   callOSC :: OSC -> Scheduled ()
   callOSC message = do
      now <- getTime
      mapReaderT liftIO . callBS . encodeOSCBundle $ OSCBundle now [Right message]

   callBS :: ByteString -> Scheduled ()
   callBS message = do
      now <- getTime
      mapReaderT liftIO . callBS . encodeOSCBundle $ OSCBundle now [Left message]

   sync :: Scheduled ()
   sync = return () -- always right?

   waitForSync :: SyncId -> Scheduled ()
   waitForSync _ = return () -- always right?

   wait :: Real n => n -> Scheduled ()
   wait t = modify (`addSecs` realToFrac t)

   getTime :: Scheduled Timestamp
   getTime = get

   newBufferId :: Scheduled BufferId
   newBufferId = mapReaderT liftIO (newBufferId :: ReaderT SCServerState IO BufferId)

   newNodeId :: Scheduled NodeId
   newNodeId = mapReaderT liftIO (newNodeId :: ReaderT SCServerState IO NodeId)

   newSyncId :: Scheduled SyncId
   newSyncId = mapReaderT liftIO (newSyncId :: ReaderT SCServerState IO SyncId)

   fork :: Scheduled () -> Scheduled ()
   fork action = do
      timeOfFork <- get
      action
      put timeOfFork

   defineSD :: SynthDef a -> Scheduled ()
   defineSD = mapReaderT (liftIO . void . forkIO) . defineSD

-- | Schedule an action to happen at the given time
doScheduledAtWith :: SCServerState -> Timestamp -> Scheduled a -> IO a
doScheduledAtWith serverState startTime action =
   let stateAction = runReaderT action serverState
   in evalStateT stateAction startTime

-- | Schedule an action to happen n seconds from now
doScheduledInWith :: SCServerState -> Double -> Scheduled x -> IO x
doScheduledInWith serverState numSecs action = do
   now <- runReaderT (getTime :: ReaderT SCServerState IO Timestamp) serverState -- Not that the server state matters here
   doScheduledAtWith serverState (addSecs now numSecs) action

-- | Schedule an action to happen right now. Because of server latency this
--   could arrive late, so you might want to do something like
--   @doScheduledIn 0.01@ instead:
doScheduledNowWith :: SCServerState -> Scheduled x -> IO x
doScheduledNowWith serverState action = do
   now <- runReaderT (getTime :: ReaderT SCServerState IO Timestamp) serverState -- Not that the server state matters here
   doScheduledAtWith serverState now action
