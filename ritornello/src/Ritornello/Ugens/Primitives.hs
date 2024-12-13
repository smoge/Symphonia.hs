{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Ritornello.Ugens.Primitives where

import Data.Fixed (mod')
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Vector.Unboxed.Mutable as VM
import Ritornello.Basic
import Ritornello.Signal
import Prelude hiding (sum)

-- PRIMITIVES
addSignal :: Signal (Double, Double) Double
addSignal = Signal $ \_ -> pure $ \(a, b) -> pure $! a + b
{-# INLINE addSignal #-}

(.+.) :: Signal (Double, Double) Double
(.+.) = addSignal
{-# INLINE (.+.) #-}

mulSignal :: Signal (Double, Double) Double
mulSignal = Signal $ \_ -> pure $ \(a, b) -> pure $! a * b
{-# INLINE mulSignal #-}

(.*.) :: Signal (Double, Double) Double
(.*.) = mulSignal
{-# INLINE (.*.) #-}

divSignal :: Signal (Double, Double) Double
divSignal = Signal $ \_ -> pure $ \(a, b) -> pure $! a / b
{-# INLINE divSignal #-}

(./.) :: Signal (Double, Double) Double
(./.) = divSignal
{-# INLINE (./.) #-}

negateSignal :: Signal Double Double
negateSignal = Signal $ \_ -> pure $ \input -> pure $! -input
{-# INLINE negateSignal #-}

-- | Memory and delay primitives
delay1 :: Signal Double Double
delay1 = Signal $ \_ -> do
  lastX <- newIORef 0.0
  pure $ \input -> do
    prev <- readIORef lastX
    writeIORef lastX input
    pure prev

-- | Comparison primitives
gtSignal :: Signal (Double, Double) Double
gtSignal = Signal $ \_ -> pure $ \(a, b) ->
  pure $! if a > b then 1.0 else 0.0
{-# INLINE gtSignal #-}

ltSignal :: Signal (Double, Double) Double
ltSignal = Signal $ \_ -> pure $ \(a, b) ->
  pure $! if a < b then 1.0 else 0.0
{-# INLINE ltSignal #-}

eqSignal :: Signal (Double, Double) Double
eqSignal = Signal $ \_ -> pure $ \(a, b) ->
  pure $! if a == b then 1.0 else 0.0
{-# INLINE eqSignal #-}

-- | Signal routing primitives
passSignal :: Signal Double Double
passSignal = Signal $ \_ -> pure $ \input -> pure input
{-# INLINE passSignal #-}

splitSignal :: Signal Double (Double, Double)
splitSignal = Signal $ \_ -> pure $ \input -> pure (input, input)
{-# INLINE splitSignal #-}

selectSignal :: Signal (Double, Double, Double) Double
selectSignal = Signal $ \_ -> pure $ \(condition, a, b) ->
  pure $! if condition > 0 then a else b
{-# INLINE selectSignal #-}

-- | Core math primitives
sineSignal :: Signal Double Double
sineSignal = Signal $ \_ -> pure $ \input -> pure $! sin input
{-# INLINE sineSignal #-}

cosineSignal :: Signal Double Double
cosineSignal = Signal $ \_ -> pure $ \input -> pure $! cos input
{-# INLINE cosineSignal #-}

exponential :: Signal Double Double
exponential = Signal $ \_ -> pure $ \input -> pure $! Prelude.exp input
{-# INLINE exponential #-}

logarithm :: Signal Double Double
logarithm = Signal $ \_ -> pure $ \input -> pure $! Prelude.log input
{-# INLINE logarithm #-}

power :: Signal (Double, Double) Double
power = Signal $ \_ -> pure $ \(base, this_exponent) ->
  pure $! base ** this_exponent
{-# INLINE power #-}

squareRoot :: Signal Double Double
squareRoot = Signal $ \_ -> pure $ \input -> pure $! Prelude.sqrt input
{-# INLINE squareRoot #-}

-- | Clipping primitives
minimumSignal :: Signal (Double, Double) Double
minimumSignal = Signal $ \_ -> pure $ \(a, b) -> pure $! Prelude.min a b
{-# INLINE minimumSignal #-}

maximumSignal :: Signal (Double, Double) Double
maximumSignal = Signal $ \_ -> pure $ \(a, b) -> pure $! Prelude.max a b
{-# INLINE maximumSignal #-}

-- | Scale signal to a specific range
scaleRange :: (Double, Double) -> (Double, Double) -> Signal Double Double
scaleRange (inMin, inMax) (outMin, outMax) = Signal $ \_ -> pure $ \input ->
  let !normalized = (input - inMin) / (inMax - inMin)
      !scaled = normalized * (outMax - outMin) + outMin
   in pure scaled
{-# INLINE scaleRange #-}

-- | State primitives
accumulate :: Signal Double Double
accumulate = Signal $ \_ -> do
  state <- newIORef 0.0
  pure $ \input -> do
    current <- readIORef state
    let !next = current + input
    writeIORef state next
    pure next
{-# INLINE accumulate #-}

sampleAndHold :: Signal (Double, Double) Double
sampleAndHold = Signal $ \_ -> do
  held <- newIORef 0.0
  pure $ \(trigger, input) -> do
    current <- readIORef held
    let !next = if trigger > 0 then input else current
    writeIORef held next
    pure next

-- | Buffer primitives
readBuffer :: VM.IOVector Double -> Signal Double Double
readBuffer buffer = Signal $ \_ -> pure $ \index ->
  VM.read buffer (Prelude.floor index)

writeBuffer :: VM.IOVector Double -> Signal (Double, Double) Double
writeBuffer buffer = Signal $ \_ -> pure $ \(index, value) -> do
  VM.write buffer (Prelude.floor index) value
  pure value

-- | Random number primitives
randomUniform :: Signal (Double, Double) Double
randomUniform = Signal $ \_ -> do
  seed <- newIORef 1234 -- Initial seed
  pure $ \(min, max) -> do
    s <- readIORef seed
    let !newSeed = (1103515245 * s + 12345) `mod` 2147483648
        !value = min + (max - min) * fromIntegral newSeed / 2147483648
    writeIORef seed newSeed
    pure value

-- | Phase primitives
phasor :: Signal Double Double
phasor = Signal $ \_ -> do
  phase <- newIORef 0.0
  pure $ \freq -> do
    p <- readIORef phase
    let !newPhase = (p + freq) `mod'` 1.0
    writeIORef phase newPhase
    pure p

wrap :: Signal Double Double
wrap = Signal $ \_ -> pure $ \input ->
  pure $! input `mod'` 1.0

-- | Quantization primitives
floorSignal :: Signal Double Double
floorSignal = Signal $ \_ -> pure $ \input ->
  pure $! fromIntegral (Prelude.floor input :: Int)

ceilingSignal :: Signal Double Double
ceilingSignal = Signal $ \_ -> pure $ \input ->
  pure $! fromIntegral (Prelude.ceiling input :: Int)

roundSignal :: Signal Double Double
roundSignal = Signal $ \_ -> pure $ \input ->
  pure $! fromIntegral (Prelude.round input :: Int)

-- | Integration primitives
integrate :: Signal Double Double
integrate = Signal $ \_ -> do
  sum <- newIORef 0.0
  pure $ \input -> do
    current <- readIORef sum
    let !next = current + input
    writeIORef sum next
    pure next

-- | Edge detection primitives
risingEdge :: Signal Double Double
risingEdge = Signal $ \_ -> do
  prev <- newIORef 0.0
  pure $ \input -> do
    last <- readIORef prev
    writeIORef prev input
    pure $! if input > 0 && last <= 0 then 1.0 else 0.0

fallingEdge :: Signal Double Double
fallingEdge = Signal $ \_ -> do
  prev <- newIORef 0.0
  pure $ \input -> do
    last <- readIORef prev
    writeIORef prev input
    pure $! if input <= 0 && last > 0 then 1.0 else 0.0

-- | Counter primitives
counter :: Signal Double Double
counter = Signal $ \_ -> do
  count <- newIORef 0.0
  pure $ \trigger -> do
    current <- readIORef count
    let !next = if trigger > 0 then current + 1 else current
    writeIORef count next
    pure next

resetCounter :: Signal (Double, Double) Double
resetCounter = Signal $ \_ -> do
  count <- newIORef 0.0
  pure $ \(trigger, reset) -> do
    current <- readIORef count
    let !next = current + 1
    writeIORef count next
    pure next

-- | Threshold primitives
threshold :: Signal (Double, Double) Double
threshold = Signal $ \_ -> pure $ \(input, thresh) ->
  pure $! if input >= thresh then 1.0 else 0.0

schmittTrigger :: Signal (Double, Double, Double) Double
schmittTrigger = Signal $ \_ -> do
  state <- newIORef False
  pure $ \(input, low, high) -> do
    isHigh <- readIORef state
    let !newState =
          if isHigh
            then input > low
            else input > high
    writeIORef state newState
    pure $! if newState then 1.0 else 0.0

-- Arithmetic Operators

(.^) :: Signal (Double, Double) Double
(.^) = power
{-# INLINE (.^) #-}

-- Comparison Operators
(.>.) :: Signal (Double, Double) Double
(.>.) = gtSignal
{-# INLINE (.>.) #-}

(.<.) :: Signal (Double, Double) Double
(.<.) = ltSignal
{-# INLINE (.<.) #-}

(.==.) :: Signal (Double, Double) Double
(.==.) = eqSignal
{-# INLINE (.==.) #-}

-- Logical Operators for Thresholding
(.>=.) :: Signal (Double, Double) Double
(.>=.) = Signal $ \_ -> pure $ \(a, b) -> pure $! if a >= b then 1.0 else 0.0
{-# INLINE (.>=.) #-}

(.<=.) :: Signal (Double, Double) Double
(.<=.) = Signal $ \_ -> pure $ \(a, b) -> pure $! if a <= b then 1.0 else 0.0
{-# INLINE (.<=.) #-}

-- Clipping Operators
(.||.) :: Signal (Double, Double) Double
(.||.) = maximumSignal
{-# INLINE (.||.) #-}

(.&&.) :: Signal (Double, Double) Double
(.&&.) = minimumSignal
{-# INLINE (.&&.) #-}

-- Wrapping and Scaling
(.*|*.) :: (Double, Double) -> (Double, Double) -> Signal Double Double
(.*|*.) = scaleRange
{-# INLINE (.*|*.) #-}

-- Routing Operators
(>>>) :: Signal a b -> Signal b c -> Signal a c
(>>>) = (>>>)
{-# INLINE (>>>) #-}

(<<<) :: Signal b c -> Signal a b -> Signal a c
(<<<) = (<<<)
{-# INLINE (<<<) #-}
