{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- Ritornello.Operators
module Ritornello.Ugens.Operators where

import Control.Arrow
import Data.IORef
import qualified Data.Vector.Unboxed.Mutable as VM
import Ritornello.Basic
import Ritornello.Signal
import Ritornello.Ugens.Primitives
import Prelude

-- | Basic mixing of two signals
blend :: Signal (Double, Double) Double
blend = arr (uncurry (+))
{-# INLINE blend #-}

-- | Weighted mixing of multiple signals
mixN :: [Double] -> Signal [Double] Double
mixN weights = arr (\inputs -> sum $ zipWith (*) weights inputs)
{-# INLINE mixN #-}

-- | Simple gain control
gain :: Double -> Signal Double Double
gain factor = arr (* factor)
{-# INLINE gain #-}

-- | Amplitude modulation
amModulate :: Signal (Double, Double) Double
amModulate = arr (uncurry (*))
{-# INLINE amModulate #-}

-- | Ring modulation
ringModulate :: Signal (Double, Double) Double
ringModulate = arr (\(input, modulator) -> input * (1 + modulator))
{-# INLINE ringModulate #-}

-- | Simple one-pole DC blocker
dcBlock :: Double -> Signal Double Double
dcBlock coefficient = Signal $ \_ -> do
  lastY <- newIORef 0.0
  lastX <- newIORef 0.0
  pure $ \input -> do
    prevY <- readIORef lastY
    prevX <- readIORef lastX
    let !output = input - prevX + coefficient * prevY
    writeIORef lastY output
    writeIORef lastX input
    pure output
{-# INLINE dcBlock #-}

-- | Constant power panning
pan :: Double -> Signal Double (Double, Double)
pan position =
  let !sqrtPos = sqrt position
      !sqrtNegPos = sqrt (1 - position)
   in arr (\input -> (input * sqrtNegPos, input * sqrtPos))
{-# INLINE pan #-}

-- | Hard clipping
hardLimit :: Double -> Signal Double Double
hardLimit this_threshold = arr (\input -> max (-this_threshold) (min this_threshold input))
{-# INLINE hardLimit #-}

-- | Soft clipping using tanh
softLimit :: Signal Double Double
softLimit = arr tanh
{-# INLINE softLimit #-}

-- | Stereo width control
width :: Double -> Signal (Double, Double) (Double, Double)
width amount =
  let !midScale = 0.5
      !sideScale = 0.5 * amount
   in arr
        ( \(this_left, this_right) ->
            let !mid = (this_left + this_right) * midScale
                !side = (this_left - this_right) * sideScale
             in (mid + side, mid - side)
        )
{-# INLINE width #-}

-- | Phase inversion
negateSignal :: Signal Double Double
negateSignal = arr negate
{-# INLINE negateSignal #-}

-- | Dry/wet mix for effects
dryWet :: Double -> Signal (Double, Double) Double
dryWet mix =
  let !dryAmount = 1 - mix
      !wetAmount = mix
   in arr (\(dry, wet) -> dry * dryAmount + wet * wetAmount)
{-# INLINE dryWet #-}

-- | Exponential gain control (useful for volume controls)
expGain :: Double -> Signal Double Double
expGain factor =
  let !expFactor = exp (factor * log 2)
   in arr (* expFactor)
{-# INLINE expGain #-}

-- | Simple noise gate
noiseGate :: Double -> Double -> Signal Double Double
noiseGate this_threshold ratio =
  arr (\input -> if abs input < this_threshold then input * ratio else input)
{-# INLINE noiseGate #-}

-- | Monophonic to stereo conversion
monoToStereo :: Signal Double (Double, Double)
monoToStereo = arr dupe
{-# INLINE monoToStereo #-}

-- | Stereo to monophonic conversion (average)
stereoToMono :: Signal (Double, Double) Double
stereoToMono = arr (\(this_left, this_right) -> (this_left + this_right) * 0.5)
{-# INLINE stereoToMono #-}

-- | Balance control for stereo signals
balance :: Double -> Signal (Double, Double) (Double, Double)
balance pos = Signal $ \_ -> pure $ \(left, right) ->
  let !leftGain = Prelude.sqrt (1 - Prelude.max 0 pos)
      !rightGain = Prelude.sqrt (1 + Prelude.min 0 pos)
   in pure (left * leftGain, right * rightGain)
{-# INLINE balance #-}

-- | Linear interpolation between two signals
interpolate :: Double -> Signal (Double, Double) Double
interpolate t = Signal $ \_ -> pure $ \(a, b) ->
  let !output = a * (1 - t) + b * t
   in pure output
{-# INLINE interpolate #-}

-- | One-pole lowpass filter
simpleLowpass :: Double -> Signal Double Double
simpleLowpass coeff =
  let !oneMinusCoeff = 1 - coeff -- precompute this
   in Signal $ \_ -> do
        lastY <- newIORef 0.0
        pure $ \input -> do
          prevY <- readIORef lastY
          let !output = input * coeff + prevY * oneMinusCoeff
          writeIORef lastY output
          pure output
{-# INLINE simpleLowpass #-}

-- | Simple single-pole highpass filter
simpleHighpass :: Double -> Signal Double Double
simpleHighpass coeff = Signal $ \_ -> do
  lastX <- newIORef 0.0
  lastY <- newIORef 0.0
  pure $ \input -> do
    prevX <- readIORef lastX
    prevY <- readIORef lastY
    let !output = coeff * (prevY + input - prevX)
    writeIORef lastX input
    writeIORef lastY output
    pure output
{-# INLINE simpleHighpass #-}

-- | Absolute value of signal
magnitude :: Signal Double Double
magnitude = Signal $ \_ -> pure $ \input ->
  pure $! Prelude.abs input
{-# INLINE magnitude #-}

-- | Square of signal
squared :: Signal Double Double
squared = Signal $ \_ -> pure $ \input ->
  pure $! input * input
{-# INLINE squared #-}

-- | Root mean square level detection with incremental updates
rms :: Int -> Signal Double Double
rms windowSize = Signal $ \_ -> do
  buffer <- VM.replicate windowSize 0.0
  pos <- newIORef 0
  sumSquares <- newIORef 0.0
  pure $ \input -> do
    currentPos <- readIORef pos
    currentSquared <- VM.read buffer currentPos

    -- Update the running sum by removing the squared value at the current position
    modifyIORef' sumSquares (subtract currentSquared)

    -- Calculate the new squared value and write it to the buffer
    let newSquared = input * input
    VM.write buffer currentPos newSquared

    -- Add the new squared value to the running sum
    modifyIORef' sumSquares (+ newSquared)

    -- Update the position circularly
    let !nextPos = (currentPos + 1) `mod` windowSize
    writeIORef pos nextPos

    -- Read the current sum of squares and calculate RMS
    totalSum <- readIORef sumSquares
    pure $! Prelude.sqrt (totalSum / fromIntegral windowSize)
{-# INLINE rms #-}

-- | Hysteresis processor (schmitt trigger)
hysteresis :: Double -> Double -> Signal Double Double
hysteresis lowThresh highThresh = Signal $ \_ -> do
  state <- newIORef False
  pure $ \input -> do
    isHigh <- readIORef state
    let !newState =
          if isHigh
            then input > lowThresh
            else input > highThresh
        !output = if newState then 1 else -1
    writeIORef state newState
    pure output
{-# INLINE hysteresis #-}

-- | Signal rectification (half or full wave)
rectify :: Bool -> Signal Double Double
rectify fullWave = Signal $ \_ -> pure $ \input ->
  pure $!
    if fullWave
      then Prelude.abs input
      else Prelude.max 0 input
{-# INLINE rectify #-}

-- | Bipolar to unipolar signal conversion (from [-1,1] to [0,1])
bipolarToUnipolar :: Signal Double Double
bipolarToUnipolar = Signal $ \_ -> pure $ \input ->
  pure $! (input + 1) * 0.5
{-# INLINE bipolarToUnipolar #-}

-- | Unipolar to bipolar signal conversion (from [0,1] to [-1,1])
unipolarToBipolar :: Signal Double Double
unipolarToBipolar = Signal $ \_ -> pure $ \input ->
  pure $! input * 2 - 1
{-# INLINE unipolarToBipolar #-}

-- Infix Operators
(<+>) :: Signal (Double, Double) Double
(<+>) = blend

infixl 6 <+>

(*>) :: Double -> Signal Double Double
(*>) = gain

infixl 7 *>

(<*>) :: Signal (Double, Double) Double
(<*>) = amModulate

infixl 7 <*>

(<#>) :: Signal (Double, Double) Double
(<#>) = ringModulate

infixl 7 <#>

(!>) :: Double -> Signal Double Double
(!>) = hardLimit

infixl 8 !>

(~>) :: Signal Double Double
(~>) = softLimit

infixl 8 ~>
