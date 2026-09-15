module RadioLab.Fourier
    ( SpectrumBin(..)
    , dft
    , idft
    , fft
    , ifft
    , spectrum
    , dominantFrequency
    ) where

import Data.Complex
import Data.List (maximumBy)
import Data.Ord (comparing)

import RadioLab.Signal (SampleRate)

data SpectrumBin = SpectrumBin
    { binFrequency :: Double
    , binMagnitude :: Double
    , binPhase     :: Double
    } deriving (Eq, Show)

dft :: [Complex Double] -> [Complex Double]
dft xs =
    let n = length xs
        n' = fromIntegral n
        coefficient k j = cis (-2 * pi * fromIntegral (k * j) / n')
    in [ sum [x * coefficient k j | (j, x) <- zip [0..] xs]
       | k <- [0 .. n - 1]
       ]

idft :: [Complex Double] -> [Complex Double]
idft [] = []
idft xs = map (/ (fromIntegral (length xs) :+ 0)) . map conjugate . dft . map conjugate $ xs

fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [x] = [x]
fft xs
    | not (isPowerOfTwo (length xs)) = dft xs
    | otherwise =
        let evens = fft (everyOther xs)
            odds  = fft (everyOther (drop 1 xs))
            n = length xs
            twiddle :: Int -> Complex Double
            twiddle k = cis (-2 * pi * fromIntegral k / fromIntegral n)
            firstHalf = zipWith3 (\e o k -> e + twiddle k * o) evens odds [0..]
            secondHalf = zipWith3 (\e o k -> e - twiddle k * o) evens odds [0..]
        in firstHalf ++ secondHalf

ifft :: [Complex Double] -> [Complex Double]
ifft [] = []
ifft xs =
    let n = fromIntegral (length xs) :+ 0
    in map ((/ n) . conjugate) (fft (map conjugate xs))

spectrum :: SampleRate -> [Double] -> [SpectrumBin]
spectrum _ [] = []
spectrum sampleRate xs =
    let transformed = fft (map (:+ 0) xs)
        n = length xs
        half = n `div` 2
        scale = fromIntegral n
        mkBin :: Int -> Complex Double -> SpectrumBin
        mkBin k z = SpectrumBin
            { binFrequency = fromIntegral k * sampleRate / scale
            , binMagnitude = (if k == 0 || (even n && k == half) then 1 else 2) * magnitude z / scale
            , binPhase = phase z
            }
    in zipWith mkBin [0..] (take (half + 1) transformed)

-- | Ignore silence and numerical residue below an absolute amplitude of 1e-12.
dominantFrequency :: [SpectrumBin] -> Maybe Double
dominantFrequency [] = Nothing
dominantFrequency bins =
    let strongest = maximumBy (comparing binMagnitude) bins
    in if binMagnitude strongest <= 1e-12 then Nothing else Just (binFrequency strongest)

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:xs) = x : everyOther (drop 1 xs)

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
    | n <= 0 = False
    | n == 1 = True
    | odd n = False
    | otherwise = isPowerOfTwo (n `div` 2)
