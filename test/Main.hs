module Main (main) where

import Control.Monad (unless)
import Data.Char (ord)
import Data.Complex
import System.Exit (exitFailure)
import Test.QuickCheck hiding (label, scale)

import RadioLab.Fourier
import RadioLab.Modulation
import RadioLab.Plot
import RadioLab.Signal

main :: IO ()
main = do
    ok1 <- check "FFT agrees with DFT" testFftMatchesDft
    ok2 <- check "IFFT round-trip" testRoundTrip
    ok3 <- check "sine spectrum peak" testSinePeak
    ok4 <- check "AM creates sidebands" testAmSidebands
    ok5 <- check "105-sample FM uses connected waveform" testConnectedFm
    ok6 <- check "dense waveform preserves excursions" testDenseWave
    ok7 <- check "waveform dimensions and edge cases" testWaveEdges
    ok8 <- check "connected waveform retains envelope guides" testEnvelope
    ok9 <- check "spectrum endpoint amplitudes and odd lengths" testSpectrumEndpoints
    ok10 <- check "silence and numerical residue have no dominant frequency" testSilence
    results <- mapM runProperty properties
    unless (and [ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10]
            && all isSuccess results) exitFailure

check :: String -> Bool -> IO Bool
check label ok = do
    putStrLn $ (if ok then "[PASS] " else "[FAIL] ") ++ label
    pure ok

testFftMatchesDft :: Bool
testFftMatchesDft = closeComplexList (fft input) (dft input)
  where input = map (:+ 0) [1, 2, 3, 4, 3, 2, 1, 0]

testRoundTrip :: Bool
testRoundTrip = closeComplexList input (ifft (fft input))
  where input = map (:+ 0) [0, 1, 0, -1, 0, 1, 0, -1]

testSinePeak :: Bool
testSinePeak = case dominantFrequency bins of
    Just f -> abs (f - 128) < 1e-9
    Nothing -> False
  where
    fs = 1024
    xs = sampleSignal fs 1024 (sine 128 0)
    bins = drop 1 (spectrum fs xs)

testAmSidebands :: Bool
testAmSidebands = all (`hasPeakNear` bins) [900, 1000, 1100]
  where
    fs = 8192
    xs = sampleSignal fs 8192 (am 1000 0.8 (sine 100 0))
    bins = spectrum fs xs
    hasPeakNear target = any (\b -> abs (binFrequency b - target) < 1e-9 && binMagnitude b > 0.1)

closeComplexList :: [Complex Double] -> [Complex Double] -> Bool
closeComplexList xs ys = length xs == length ys && and (zipWith close xs ys)
  where close a b = magnitude (a - b) < 1e-8

-- The reported close-zoom case should draw subcell strokes across the plot.
testConnectedFm :: Bool
testConnectedFm = all (any isStroke) columns
  where
    rows = plotWave 80 11 (sampleSignal 65536 105 (fm 12800 0.7 511))
    columns = [[row !! x | row <- rows] | x <- [0 .. 75]]
    isStroke c = ord c > 0x2800 && ord c <= 0x28ff

testDenseWave :: Bool
testDenseWave = plotWave 3 5 (concat (replicate 3 [-1, 0, 1])) == replicate 5 "│││"

testWaveEdges :: Bool
testWaveEdges =
    null (plotWave 0 5 [1]) && null (plotWave 5 0 [1])
    && plotWave 3 2 [] == ["   ", "   "]
    && and [length rows == h && all ((== w) . length) rows
           | w <- [1, 2, 76], h <- [1, 2, 11]
           , xs <- [[0], [1], [-1, 1], replicate 105 0, replicate 153 1]
           , let rows = plotWave w h xs]

testEnvelope :: Bool
testEnvelope = any (elem '·') (plotWaveWithEnvelope 10 5 [0, 0] (Just [1, 1]))

testSpectrumEndpoints :: Bool
testSpectrumEndpoints =
    null (spectrum 8 [])
    && matches [0] [3] (spectrum 8 [3])
    && matches [0, 1, 2, 3, 4] [1, 0, 0, 0, 0] (spectrum 8 (replicate 8 1))
    && matches [0, 1, 2, 3, 4] [0, 0, 0, 0, 1] (spectrum 8 [1,-1,1,-1,1,-1,1,-1])
    && matches [0, 1, 2, 3, 4] [0, 0, 1, 0, 0] (spectrum 8 (sampleSignal 8 8 (sine 2 0)))
    && matches [0, 1, 2] [0, 0, 1] (spectrum 5 (sampleSignal 5 5 (sine 2 0)))
  where
    matches frequencies amplitudes bins =
        length bins == length frequencies
        && and (zipWith close frequencies (map binFrequency bins))
        && and (zipWith close amplitudes (map binMagnitude bins))
    close a b = abs (a - b) < 1e-9

testSilence :: Bool
testSilence =
    dominantFrequency [] == Nothing
    && dominantFrequency (spectrum 8 (replicate 8 0)) == Nothing
    && dominantFrequency (drop 1 (spectrum 5 (replicate 5 1))) == Nothing
    && dominantFrequency [SpectrumBin 100 1e-12 0] == Nothing
    && dominantFrequency [SpectrumBin 100 1e-10 0] == Just 100

runProperty :: (String, Property) -> IO Result
runProperty (label, prop) = do
    putStrLn ("[PROPERTY] " ++ label)
    quickCheckWithResult stdArgs {maxSuccess = 200} prop

-- Small bounded inputs keep the quadratic DFT oracle fast and errors meaningful.
complexInputs :: Gen [(Double, Double)]
complexInputs = do
    n <- oneof [elements [0, 1, 2, 4, 8, 16, 32], chooseInt (3, 31)]
    vectorOf n ((,) <$> choose (-10, 10) <*> choose (-10, 10))

properties :: [(String, Property)]
properties =
    [ ("complex FFT agrees with DFT", complexProperty (\xs -> closeComplexList (fft xs) (dft xs)))
    , ("complex FFT/IFFT round-trip", complexProperty (\xs -> closeComplexList xs (ifft (fft xs))))
    , ("normalization preserves shape and is idempotent", forAllShrink realInputs shrink $ \xs ->
        let ys = normalize xs
            peak = maximum (0 : map abs xs)
            scale = if peak < 1e-12 then 1 else peak
        in length ys == length xs
           && all ((<= 1 + 1e-9) . abs) ys
           && and (zipWith (\x y -> abs (x - y * scale) < 1e-9) xs ys)
           && and (zipWith (\a b -> abs (a - b) < 1e-9) ys (normalize ys))
           && (peak < 1e-12 || abs (maximum (map abs ys) - 1) < 1e-9))
    , ("wave and bar plots preserve dimensions", forAll (chooseInt (1, 80)) $ \w ->
        forAll (chooseInt (1, 16)) $ \h ->
        forAllShrink realInputs shrink $ \xs ->
            conjoin [counterexample (show rows) (length rows == h && all ((== w) . length) rows)
                    | rows <- [plotWave w h xs, plotBars w h xs,
                               plotWaveWithEnvelope w h xs (Just (map abs xs))]])
    ]
  where
    complexProperty prop = forAllShrink complexInputs shrink $ \pairs ->
        classify (null pairs) "empty" $
        classify (length pairs `elem` [1, 2, 4, 8, 16, 32]) "radix-2 or singleton" $
        classify (length pairs `notElem` [0, 1, 2, 4, 8, 16, 32]) "DFT fallback" $
        prop [r :+ i | (r, i) <- pairs]
    realInputs = chooseInt (0, 180) >>= (\n -> vectorOf n (choose (-10, 10)))
