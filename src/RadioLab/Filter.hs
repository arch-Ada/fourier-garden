module RadioLab.Filter
    ( lowPass
    , highPass
    , bandPass
    , notch
    ) where

import Data.Complex
import RadioLab.Signal (SampleRate)

lowPass :: SampleRate -> Double -> [Complex Double] -> [Complex Double]
lowPass sampleRate cutoff = applyByFrequency sampleRate (\f -> abs f <= cutoff)

highPass :: SampleRate -> Double -> [Complex Double] -> [Complex Double]
highPass sampleRate cutoff = applyByFrequency sampleRate (\f -> abs f >= cutoff)

bandPass :: SampleRate -> Double -> Double -> [Complex Double] -> [Complex Double]
bandPass sampleRate low high = applyByFrequency sampleRate (\f -> abs f >= low && abs f <= high)

notch :: SampleRate -> Double -> Double -> [Complex Double] -> [Complex Double]
notch sampleRate center width = applyByFrequency sampleRate (\f -> abs (abs f - center) > width / 2)

applyByFrequency :: SampleRate -> (Double -> Bool) -> [Complex Double] -> [Complex Double]
applyByFrequency _ _ [] = []
applyByFrequency sampleRate keep bins =
    let n = length bins
        freq k
            | k <= n `div` 2 = fromIntegral k * sampleRate / fromIntegral n
            | otherwise = fromIntegral (k - n) * sampleRate / fromIntegral n
    in [ if keep (freq k) then z else 0 | (k, z) <- zip [0..] bins ]
