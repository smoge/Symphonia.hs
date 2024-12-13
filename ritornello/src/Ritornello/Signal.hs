{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Ritornello.Signal where

-- Inspired by several arrow functional reactive packages.
-- including Yampa, reactimate, bearriver, etc
-- trying out ideas specific for audio

import Control.Arrow
import Control.Category
import Control.DeepSeq (NFData) -- deepseq
import Control.Exception (bracket)
import Control.Monad (join, (>=>))
import Data.IORef
import qualified Data.Vector.Unboxed as V -- vector
import qualified Data.Vector.Unboxed.Mutable as VM
import Prelude hiding (id, repeat, (.))

newtype Signal a b = Signal (Finalizer -> IO (a -> IO b))

unSignal :: Signal a b -> Finalizer -> IO (a -> IO b)
unSignal (Signal signalFunction) = signalFunction
{-# INLINE unSignal #-}

newtype Finalizer = Finalizer (IORef (IO ()))

instance Functor (Signal a) where
  --fmap f (Signal m) = Signal $ \fin -> fmap (fmap f .) (m fin)
  fmap f (Signal m) = Signal $ \fin -> (fmap . fmap . fmap) f (m fin)
  {-# INLINE fmap #-}

instance Applicative (Signal a) where
  -- pure a = Signal $ \_ -> pure $ \_ -> pure a
  pure a = Signal $ const $ pure $ const $ pure a

  (Signal signal_one) <*> (Signal signal_two) = Signal $ \fin -> do
    f1 <- signal_one fin
    f2 <- signal_two fin
    pure $ \a -> f1 a <*> f2 a
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Category Signal where
  id = Signal $ \_ -> pure $ \a -> pure a
  (Signal signal_one) . (Signal signal_two) = Signal $ \fin -> do
    f1 <- signal_one fin
    f2 <- signal_two fin
    pure $ f2 >=> f1
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow Signal where
  arr f = Signal $ \_ -> pure (pure . f)
  first (Signal signal) = Signal $ \fin -> do
    f <- signal fin
    pure $ \(a, b) -> (,b) <$> f a
  second (Signal signal) = Signal $ \fin -> do
    f <- signal fin
    pure $ \(a, b) -> (a,) <$> f b
  (Signal signal1) *** (Signal signal2) = Signal $ \fin -> do
    f1 <- signal1 fin
    f2 <- signal2 fin
    pure $ \(a, b) -> (,) <$> f1 a <*> f2 b
  (Signal signal1) &&& (Signal signal2) = Signal $ \fin -> do
    f1 <- signal1 fin
    f2 <- signal2 fin
    pure $ \a -> (,) <$> f1 a <*> f2 a
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}

instance ArrowChoice Signal where
  left signal = Signal $ \fin -> do
    f <- unSignal signal fin
    pure $ \case
      Left a -> Left <$> f a
      Right d -> pure $ Right d

  right signal = Signal $ \fin -> do
    f <- unSignal signal fin
    pure $ \case
      Right a -> Right <$> f a
      Left d -> pure $ Left d

  signal1 +++ signal2 = Signal $ \fin -> do
    f1 <- unSignal signal1 fin
    f2 <- unSignal signal2 fin
    pure $ \case
      Left a -> Left <$> f1 a
      Right a -> Right <$> f2 a
  signal1 ||| signal2 = Signal $ \fin -> do
    f1 <- unSignal signal1 fin
    f2 <- unSignal signal2 fin
    pure $ \case
      Left a -> f1 a
      Right a -> f2 a
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance ArrowLoop Signal where
  loop (Signal sf) = Signal $ \fin -> do
    f <- sf fin
    ref <- newIORef undefined -- Initially, no value, can be any default value
    let looped a = do
          c <- readIORef ref -- Read the current feedback value
          ~(b, c') <- f (a, c) -- Apply function with the current input and feedback
          writeIORef ref c' -- Update the feedback value
          pure b -- Return the result
    pure looped
  {-# INLINE loop #-}

addFinalizer :: Finalizer -> IO () -> IO ()
addFinalizer (Finalizer ref) fin = modifyIORef' ref (fin >>)
{-# INLINE addFinalizer #-}

newFinalizer :: IO Finalizer
newFinalizer = Finalizer <$> newIORef (pure ())
{-# INLINE newFinalizer #-}

runFinalizer :: Finalizer -> IO ()
runFinalizer (Finalizer ref) = join $ readIORef ref
{-# INLINE runFinalizer #-}

withFinalizer :: (Finalizer -> IO a) -> IO a
withFinalizer = bracket newFinalizer runFinalizer
{-# INLINE withFinalizer #-}
