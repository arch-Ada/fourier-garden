module RadioLab.Modulation
    ( am
    , dsbSc
    , fm
    , keyedCarrier
    , mix
    ) where

import RadioLab.Signal (Hertz, Signal)

am :: Hertz -> Double -> Signal -> Signal
am carrierHz depth message t =
    (1 + clamp 0 1 depth * message t) * cos (2 * pi * carrierHz * t)

dsbSc :: Hertz -> Signal -> Signal
dsbSc carrierHz message t = message t * cos (2 * pi * carrierHz * t)

fm :: Hertz -> Double -> Hertz -> Signal
fm carrierHz modulationIndex toneHz t =
    cos (2 * pi * carrierHz * t + modulationIndex * sin (2 * pi * toneHz * t))

keyedCarrier :: Hertz -> Hertz -> Signal
keyedCarrier carrierHz keyingHz t =
    let gate = if sin (2 * pi * keyingHz * t) >= 0 then 1 else 0
    in gate * cos (2 * pi * carrierHz * t)

mix :: Signal -> Signal -> Signal
mix a b t = a t * b t

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
