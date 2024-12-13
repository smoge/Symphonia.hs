-- | Non-realtime synthesis. Create a sound file from the same instructions
--   you use to perform live!
-- 
--   **Note** we don't currently support redefining Synthdefs midway -- e.g.
--   you can't explicitly define a SynthDef "foo" (with 'defineSD'), then make a
--   synth from it, then explicitly define it again with a new definition, and
--   then make a new synth with the new definition


{-# LANGUAGE
     BangPatterns
   , FlexibleInstances
   , InstanceSigs
   , LambdaCase
   , TypeSynonymInstances

   , NoIncoherentInstances
   , NoMonomorphismRestriction
   , NoUndecidableInstances
   #-}

module Vivid.Actions.NRT (
     NRT -- (..) -- ^ May not be exported in the future
   , writeNRT
   , writeNRTScore
   , encodeNRTScore
   , runNRT

   , writeNRTWith
   , NRTArgs(..)
   , defaultNRTArgs
   ) where

import qualified Vivid.SC.Server.Commands as SCCmd

import Vivid.Actions.Class
import Vivid.OSC
import Vivid.OSC.Bundles (encodeOSCBundles)
import Vivid.SCServer
-- import Vivid.SCServer.State
import Vivid.SynthDef (sdToLiteral)
import Vivid.SynthDef.Types

import Control.Applicative
-- import Control.Arrow (first, second)
import Control.Exception
import Control.Monad
import Control.Monad.State (get, modify, execStateT, StateT, state)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (writeFile, hPut)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import qualified Data.Set as Set
import System.Directory (canonicalizePath, getTemporaryDirectory) -- , removeFile)
import System.Exit
import System.FilePath (takeExtension)
import System.IO (openBinaryTempFile, hClose)
import System.Process (system)
import Prelude

data NRTState
   = NRTState {
     nrtState_now :: Timestamp
     -- We keep track of the maximum timestamp so that the generated audio file
     --   doesn't cut off before a final 'wait' finishes:
   , nrtState_maxTime :: Maximum Timestamp
   , nrtState_messages :: Map Timestamp [Either ByteString OSC]
   , nrtState_bufferIds :: [BufferId]
   , nrtState_nodeIds :: [NodeId]
   , nrtState_syncIds :: [SyncId]
   }

type NRT = StateT NRTState IO

callMessage :: Either ByteString OSC -> NRT ()
callMessage msg = do
   now <- getTime
   -- Writing it this way so that we can be crystal-clear that it's going on
   --   the end of the list (future: use dlist for efficiency):
   modify $ \ns -> ns {
        nrtState_messages =
           case Map.lookup now $ nrtState_messages ns of
              Nothing ->   Map.insert now [msg]           $ nrtState_messages ns
              Just msgs -> Map.insert now (msgs ++ [msg]) $ nrtState_messages ns
      }

instance VividAction NRT where
   callOSC :: OSC -> NRT ()
   callOSC = callMessage . Right

   callBS :: ByteString -> NRT ()
   callBS = callMessage . Left

   sync :: NRT ()
   sync = return ()

   waitForSync :: SyncId -> NRT ()
   waitForSync _ = return ()

   wait :: Real n => n -> NRT ()
   wait t = modify $ \ns ->
      let newT = nrtState_now ns `addSecs` realToFrac t
      in ns {
            nrtState_now = newT
          , nrtState_maxTime = Maximum newT `max` (nrtState_maxTime ns)
          }

   getTime :: NRT Timestamp
   getTime = nrtState_now <$> get

   newBufferId :: NRT BufferId
   newBufferId = state $ \ns ->
      let (x:xs) = nrtState_bufferIds ns
      in (x, ns { nrtState_bufferIds = xs})

   newNodeId :: NRT NodeId
   newNodeId = state $ \ns ->
      let (x:xs) = nrtState_nodeIds ns
      in (x, ns { nrtState_nodeIds = xs})

   newSyncId :: NRT SyncId
   newSyncId =  state $ \ns ->
      let (x:xs) = nrtState_syncIds ns
      in (x, ns { nrtState_syncIds = xs})

   fork :: NRT () -> NRT ()
   fork action = do
      NRTState{nrtState_now=timeOfFork, nrtState_maxTime = oldMaxTime} <- get
      action
      modify $ \ns -> ns {
           nrtState_now = timeOfFork
           -- this 'max' probably isn't necessary:
         , nrtState_maxTime = (nrtState_maxTime ns) `max` oldMaxTime :: Maximum Timestamp
         }

   defineSD :: SynthDef a -> NRT ()
   defineSD synthDef = do
      modify $ \ns -> ns {
           nrtState_messages =
              let cmd = [
                     Right $ SCCmd.d_recv [sdToLiteral synthDef] Nothing
                   ]
              in Map.insertWith mappendIfNeeded (Timestamp 0) cmd (nrtState_messages ns)
         }
    where
      mappendIfNeeded :: (Ord a) {- , Monoid m)-} => [a] -> [a] -> [a]
      mappendIfNeeded maybeSubset maybeSuperset =
         if Set.fromList maybeSubset `Set.isSubsetOf` Set.fromList maybeSuperset
            then maybeSuperset
            else maybeSubset <> maybeSuperset

-- This way we can be positive that it's going to the end of the list:
_addAtTime :: Timestamp -> Either ByteString OSC -> Map Timestamp [Either ByteString OSC] -> Map Timestamp [Either ByteString OSC]
_addAtTime t msg m =
   -- Separating lookup and insert is just being extra careful that the message
   --   goes on the end:
   --   (Why do you want that? - Well, one example is if you read a buffer in
   --   at time 0 and then immediately play it. You want to read it *before*
   --   you play it :) )
   Map.insert t v m
 where
   v :: [Either ByteString OSC]
   v = case Map.lookup t m of
      Nothing -> [msg]
      Just l -> l ++ [msg] -- Could use a dlist here to snoc


runNRT :: NRT a -> IO [OSCBundle]
runNRT action = do
   result <- execStateT action startingNRTState

   let Maximum maxTSeen = nrtState_maxTime result
       protoBundles_woLast :: Map Timestamp [Either ByteString OSC]
       protoBundles_woLast = nrtState_messages result
       protoBundles = Map.insertWith (<>) maxTSeen [] protoBundles_woLast

   return [ OSCBundle t as | (t, as) <- Map.toAscList protoBundles ]

startingNRTState :: NRTState
startingNRTState = NRTState {
     nrtState_now = Timestamp 0
   , nrtState_maxTime = Maximum (Timestamp 0)
   , nrtState_messages = Map.empty
   , nrtState_bufferIds = map BufferId [0..]
   , nrtState_nodeIds = map NodeId [2..] -- TODO: make sure this is good
   , nrtState_syncIds = map SyncId [0..]
   }

encodeNRTScore :: NRT x -> IO ByteString
encodeNRTScore action =
   encodeOSCBundles <$> runNRT action

-- | Generate a file of actions that SC can use to do NRT with.
-- 
--   __If you just want the .aiff file, you probably want 'writeNRT' instead.__
-- 
--   Usage: this will create a file at "/tmp/NRTout.aiff" with your @sound :: NRT a@:
-- 
--   > writeNRT "/tmp/foo.osc" test
--   > scsynth -N /tmp/foo.osc _ /tmp/NRTout.aiff 44100 AIFF int16
writeNRTScore :: FilePath -> NRT a -> IO ()
writeNRTScore path action =
   BS.writeFile path =<< encodeNRTScore action


-- | Generate an audio file from an NRT action -- this can write songs far faster
--   than it would take to play them.
-- 
--   This uses 'defaultNRTArgs' for its sample rate, number of channels, etc.
--   If you want to use args other than the default, use 'writeNRTWith'.
-- 
--   The file type is detected from its extension.
--   The extensions supported at the moment are .aif, .aiff, and .wav
-- 
--   (macOS users will need to make sure 'scsynth' is in their $PATH)
-- 
--   (And I apologize, but I really don't know what Windows users will need to do)
-- 
--   Currently doesn't work with single quotes in the filename
writeNRT :: FilePath -> NRT a -> IO ()
writeNRT = writeNRTWith defaultNRTArgs

writeNRTWith ::  NRTArgs -> FilePath -> NRT a -> IO ()
writeNRTWith nrtArgs fPath nrtActions = do
   when ('\'' `elem` fPath) $ error "Didnt have time to implement filepaths with single quotes"
   contents <- encodeOSCBundles <$> runNRT nrtActions

   --  ${SHELL}
   -- Does this work on Windows?:
   system "/bin/sh -c 'which scsynth > /dev/null'" >>= \case
      ExitSuccess -> return ()
      ExitFailure _ -> error "No 'scsynth' found! Be sure to put it in your $PATH"
   let
       !fileType =
          case Map.lookup (map toLower $ takeExtension fPath) extensionMap of
             Just x -> x
             Nothing -> error $
                "The only file extensions we currently understand are: "
                ++ show (Map.keys extensionMap)
       extensionMap = Map.fromList [
            (".aif", "AIFF")
          , (".aiff", "AIFF")
          , (".wav", "WAV")
          , (".flac", "FLAC")
          , (".raw", "raw")
            -- todo: these formats seem not to work.
            -- Try it on more-recent versions of SC:
          -- , (".ogg", "VORBIS")
          ]

   tempDir <- canonicalizePath =<< getTemporaryDirectory
   tempFile <- bracket
      (openBinaryTempFile tempDir "vivid_nrt_.osc")
      (\(_, tempFileHandle) ->
         hClose tempFileHandle)
      (\(tempFile, tempFileHandle) -> do
         BS.hPut tempFileHandle $ contents
         pure tempFile)

   ExitSuccess <- system $ mconcat [
        --  ${SHELL}
        "/bin/sh -c "
      , " \"" -- Note these beginning and ending quotes
      , " scsynth"
      , " -o ", show $ _nrtArgs_numChans nrtArgs
      , " -N "
      , tempFile
      , " _ '", fPath, "' "
      , show $ _nrtArgs_sampleRate nrtArgs," ", fileType, " int16 "
      , " \""
      ]

   -- TODO: I'm a little skittish about turning this on:
   -- removeFile tempFile

   pure ()


data NRTArgs
   = NRTArgs {
    _nrtArgs_sampleRate :: Int
   ,_nrtArgs_numChans :: Int
   }
 deriving (Show, Read, Eq, Ord)

defaultNRTArgs :: NRTArgs
defaultNRTArgs = NRTArgs {
    _nrtArgs_sampleRate = 48000
   ,_nrtArgs_numChans = 2
   }

-- Given an explicit type and tag so we don't accidentally  get the wrong element out of the tuple anywhere:
newtype Maximum a = Maximum a

instance (Eq a, Ord a) => Ord (Maximum a) where
   compare (Maximum a) (Maximum b) = compare a b
   Maximum a <= Maximum b = a <= b
   Maximum a < Maximum b = a < b
   Maximum a > Maximum b = a > b
   Maximum a >= Maximum b = a >= b
   max (Maximum a) (Maximum b) = Maximum $ max a b
   min (Maximum a) (Maximum b) = Maximum $ min a b

instance Eq a => Eq (Maximum a) where
   Maximum a == Maximum b = a == b
