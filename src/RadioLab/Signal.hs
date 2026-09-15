module RadioLab.Signal
    ( SampleRate
    , Seconds
    , Hertz
    , Signal
    , sine
    , square
    , sawtooth
    , triangle
    , chirp
    , sampleSignal
    , normalize
    , rms
    ) where

import Data.Fixed (mod')

type SampleRate = Double
type Seconds = Double
type Hertz = Double
type Signal = Double -> Double

sine :: Hertz -> Double -> Signal
sine frequency phase t = sin (2 * pi * frequency * t + phase)

square :: Hertz -> Signal
square frequency t = if sine frequency 0 t >= 0 then 1 else -1

sawtooth :: Hertz -> Signal
sawtooth frequency t = 2 * ((frequency * t) `mod'` 1) - 1

triangle :: Hertz -> Signal
triangle frequency t = 2 * abs (sawtooth frequency t) - 1

chirp :: Hertz -> Hertz -> Seconds -> Signal
chirp startHz endHz duration t =
    let k = (endHz - startHz) / max 1e-12 duration
        phase = 2 * pi * (startHz * t + 0.5 * k * t * t)
    in sin phase

sampleSignal :: SampleRate -> Int -> Signal -> [Double]
sampleSignal sampleRate count signal =
    [ signal (fromIntegral n / sampleRate) | n <- [0 .. count - 1] ]

normalize :: [Double] -> [Double]
normalize [] = []
normalize xs =
    let peak = maximum (map abs xs)
    in if peak < 1e-12 then xs else map (/ peak) xs

rms :: [Double] -> Double
rms [] = 0
rms xs = sqrt (sum (map (\x -> x * x) xs) / fromIntegral (length xs))
