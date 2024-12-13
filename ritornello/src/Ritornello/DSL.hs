{-# LANGUAGE Arrows #-}

module Ritornello.DSL where

import Control.Arrow
import Control.Category
import Ritornello.Basic
import Ritornello.Signal
import Ritornello.Ugens.Operators
import Ritornello.Ugens.Osc
import Ritornello.Ugens.Primitives
import Prelude hiding (id, (.))

-- Core constants
sampleRate :: Double
sampleRate = 44100.0

-- Oscillators
sine :: Double -> Signal () Double
sine freq = generateSineWave freq sampleRate 0

saw :: Double -> Signal () Double
saw freq = generateSawWave freq sampleRate 0

square :: Double -> Double -> Signal () Double
square freq = generateSquareWave freq sampleRate 0

triangle :: Double -> Signal () Double
triangle freq = generateTriangleWave freq sampleRate 0

-- addSig :: Signal Double Double -> Signal Double Double -> Signal Double Double
-- addSig s1 s2 = proc val -> do
--   x1 <- s1 -< val
--   x2 <- s2 -< val
--   returnA -< x1 + x2

-- mulSig :: Signal Double Double -> Signal Double Double -> Signal Double Double
-- mulSig s1 s2 = proc val -> do
--   x1 <- s1 -< val
--   x2 <- s2 -< val
--   returnA -< x1 * x2

addSig :: Signal Double Double -> Signal Double Double -> Signal Double Double
addSig = liftA2 (+)

mulSig :: Signal Double Double -> Signal Double Double -> Signal Double Double
mulSig = liftA2 (*)

-- Processing
dcblock :: Double -> Signal Double Double
dcblock = dcBlock

clip :: Double -> Signal Double Double
clip = hardLimit

delay :: Signal Double Double
delay = delay1

-- Routing
split :: Signal Double (Double, Double)
split = splitSignal

mix :: Signal (Double, Double) Double
mix = blend

select :: Signal (Double, Double, Double) Double
select = selectSignal

simpleOsc :: Double -> Signal () Double
simpleOsc freq = proc () -> do
  osc <- sine freq -< ()
  returnA -< osc * 0.5

oscWithFeedback :: Double -> Signal () Double
oscWithFeedback freq = proc () -> do
  input <- sine freq -< ()
  delayed <- delay -< input
  mixed <- mix -< (input, delayed)
  clipped <- clip 0.9 -< mixed
  returnA -< clipped

complexOsc :: Double -> Double -> Signal () Double
complexOsc freq1 freq2 = proc () -> do
  s1 <- sine freq1 -< ()
  s2 <- saw freq2 -< ()
  m <- mix -< (s1, s2)
  d <- dcblock 0.99 -< m
  returnA -< d

-- More complex example using the new helpers
modulatedOsc :: Double -> Double -> Signal () Double
modulatedOsc carrierFreq modFreq = proc () -> do
  carrier <- sine carrierFreq -< ()
  modulator <- sine modFreq -< ()
  modulated <- amModulate -< (carrier, modulator)
  clipped <- clip 0.9 -< modulated
  returnA -< clipped


-- Dual oscillator with ring modulation
ringModOsc :: Double -> Double -> Signal () Double
ringModOsc freq1 freq2 = proc () -> do
  main <- sine freq1 -< ()
  modulator <- triangle freq2 -< ()
  modulated <- ringModulate -< (main, modulator)
  dcFiltered <- dcblock 0.99 -< modulated
  returnA -< dcFiltered * 0.7

-- Sub oscillator with octave down
subOsc :: Double -> Signal () Double
subOsc freq = proc () -> do
  main <- sine freq -< ()
  sub <- square freq 0.5 -< () -- Square wave an octave down
  octaveDown <- gain 0.5 -< sub
  mixed <- mix -< (main, octaveDown)
  returnA -< mixed * 0.6

padSound :: Double -> Signal () Double
padSound freq = proc () -> do
  osc1 <- sine freq -< ()
  osc2 <- sine (freq * 1.01) -< () -- Slight detuning
  lfo <- sine 0.5 -< () -- Slow LFO
  mixed <- mix -< (osc1, osc2)
  modulated <- gain 0.3 -< mixed
  withLfo <- amModulate -< (modulated, lfo * 0.2 + 0.8)
  filtered <- simpleLowpass 0.2 -< withLfo

  returnA -< filtered

gatedOscillator :: Double -> Signal () Double
gatedOscillator freq = proc () -> do
  osc <- sine freq -< ()
  lfo <- triangle 0.5 -< () -- Gate control
  gated <- hysteresis 0.3 0.7 -< lfo
  output <- amModulate -< (osc, gated)
  returnA -< output * 0.6

-- Example of using addSig and mulSig for complex modulation
complexModulation :: Double -> Double -> Signal () Double
complexModulation freq1 freq2 = proc input -> do
  osc1 <- sine freq1 -< input
  osc2 <- sine freq2 -< input
  mod1 <- addSig (gain 0.5) delay1 -< osc1
  mod2 <- mulSig (dcblock 0.99) (gain 0.7) -< osc2
  final <- mix -< (mod1, mod2)
  returnA -< final * 0.5