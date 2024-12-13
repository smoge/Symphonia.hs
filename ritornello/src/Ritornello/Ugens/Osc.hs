{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Ritornello.Ugens.Osc where

import Data.Fixed (mod')
import Data.IORef
import Ritornello.Signal
import Prelude hiding (id, repeat, (.))

data OscState = OscState
  { oscPhase :: !Double,
    oscFreq :: !Double,
    oscSampleRate :: !Double,
    oscOmega :: !Double -- Precalculated phase increment
  }

mkOscillator :: (OscState -> Double) -> Double -> Double -> Signal () Double
mkOscillator f freq sr = Signal $ \_ -> do
  let omega = 2 * pi * max 0 (min freq (sr / 2)) / sr
  state <-
    newIORef $!
      OscState
        { oscPhase = 0,
          oscFreq = freq,
          oscSampleRate = sr,
          oscOmega = omega
        }

  pure $ \() -> do
    s@OscState {..} <- readIORef state
    let !sample = f s
        !newPhase = (oscPhase + oscOmega) `mod'` (2 * pi)
    writeIORef state $! s {oscPhase = newPhase}
    pure sample
{-# INLINE mkOscillator #-}

generateSineWave :: Double -> Double -> Double -> Signal () Double
generateSineWave freq sr phase =
  mkOscillator (\OscState {..} -> sin (oscPhase + phase)) freq sr

generateSquareWave :: Double -> Double -> Double -> Double -> Signal () Double
generateSquareWave freq sr phase duty =
  mkOscillator squareFunc freq sr
  where
    squareFunc OscState {..} =
      let !normPhase = (oscPhase + phase) / (2 * pi)
          !p = normPhase `mod'` 1
          !base = if p < duty then 1 else (-1)
       in base - polyblepComp p duty (freq / sr)

generateSawWave :: Double -> Double -> Double -> Signal () Double
generateSawWave freq sr phase = mkOscillator sawGenerator freq sr
  where
    dt = freq / sr
    sawGenerator OscState {..} =
      let !normPhase = (oscPhase + phase) / (2 * pi)
          !p = normPhase `mod'` 1
          !raw = 2 * p - 1
       in raw - sawComp p dt

generateTriangleWave :: Double -> Double -> Double -> Signal () Double
generateTriangleWave freq sr phase = mkOscillator triGenerator freq sr
  where
    dt = freq / sr
    triGenerator OscState {..} =
      let !normPhase = (oscPhase + phase) / (2 * pi)
          !p = normPhase `mod'` 1
          !saw = 2 * p - 1
          !antiAliasedSaw = saw - sawComp p dt
       in 4 * abs (antiAliasedSaw / 2) - 1

sawComp :: Double -> Double -> Double
sawComp p dt
  | t < 1 = polyblep t
  | t > (1 / dt - 1) = -(polyblep (t - fromIntegral (floor t :: Integer)))
  | otherwise = 0
  where
    t = p / dt
{-# INLINE sawComp #-}

polyblep :: Double -> Double
polyblep t
  | t < 0 = 0
  | t > 1 = 0
  | t < 0.5 = let !t2 = t * t in 2 * t2
  | otherwise =
      let !t' = 1 - t
          !t2 = t' * t'
       in 1 - 2 * t2
{-# INLINE polyblep #-}

polyblepComp :: Double -> Double -> Double -> Double
polyblepComp p duty dt = compute t1 - compute t2
  where
    t1 = p / dt
    t2 = (p - duty) / dt
    compute t
      | t < 1 = polyblep t
      | t > (1 / dt - 1) = polyblep (t - fromIntegral (floor t :: Integer))
      | otherwise = 0
{-# INLINE polyblepComp #-}