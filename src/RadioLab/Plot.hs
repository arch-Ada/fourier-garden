module RadioLab.Plot
    ( plotWave
    , plotWaveWithEnvelope
    , plotBars
    ) where

import Data.Bits ((.|.), bit)
import Data.Char (chr)
import Data.List (transpose)

plotWave :: Int -> Int -> [Double] -> [String]
plotWave width height samples = plotWaveWithEnvelope width height samples Nothing

-- | Connect samples on a 2-by-4 Braille grid at up to two samples per
-- terminal column. Denser views retain min/max buckets to preserve excursions.
-- The optional positive envelope is drawn symmetrically with '·'.
plotWaveWithEnvelope :: Int -> Int -> [Double] -> Maybe [Double] -> [String]
plotWaveWithEnvelope width height samples envelope
    | width <= 0 || height <= 0 = []
    | null samples = replicate height (replicate width ' ')
    | otherwise =
        let signalRanges = bucketRanges width samples
            envelopeRanges = fmap (bucketRanges width) envelope
            signalPeak = maximum (1e-12 : concatMap (\(lo, hi) -> [abs lo, abs hi]) signalRanges)
            envelopePeak = case envelopeRanges of
                Nothing -> 0
                Just rs -> maximum (0 : concatMap (\(lo, hi) -> [abs lo, abs hi]) rs)
            peak = max signalPeak envelopePeak
            toRow y = clamp 0 (height - 1) $ round ((1 - y / peak) * fromIntegral (height - 1) / 2)
            signalRows = map (\(lo, hi) -> ordered (toRow hi) (toRow lo)) signalRanges
            guideRows = fmap (map (\(_, hi) -> (toRow hi, toRow (-hi)))) envelopeRanges
            mid = (height - 1) `div` 2
            connected = length samples <= 2 * width
            wavePixels = connectedPixels width height peak samples
            drawColumn col (topRow, bottomRow) maybeGuide =
                let columnPixels = [(x `mod` 2, y) | (x, y) <- wavePixels, x `div` 2 == col]
                in
                [ let signalHere = row >= topRow && row <= bottomRow
                      signalPoint = topRow == bottomRow
                      guideHere = case maybeGuide of
                          Nothing -> False
                          Just (upperRow, lowerRow) -> row == upperRow || row == lowerRow
                      dots = foldr (.|.) 0
                          [ bit (dotIndex dx dy)
                          | (dx, y) <- columnPixels
                          , y `div` 4 == row
                          , let dy = y `mod` 4
                          ]
                  in if connected && dots /= 0 then chr (0x2800 + dots)
                     else if not connected && signalHere then if signalPoint then '•' else '│'
                     else if guideHere then '·'
                     else if row == mid then '─'
                     else ' '
                | row <- [0 .. height - 1]
                ]
            columns = case guideRows of
                Nothing -> zipWith (\col r -> drawColumn col r Nothing) [0..] signalRows
                Just gs -> zipWith3 (\col r g -> drawColumn col r (Just g)) [0..] signalRows gs
        in transpose columns

-- Rasterize every segment, including its endpoints, without inventing samples
-- between turns. Subcell resolution makes individual carrier cycles readable.
connectedPixels :: Int -> Int -> Double -> [Double] -> [(Int, Int)]
connectedPixels width height peak samples = case points of
    [] -> []
    [point] -> [(x, snd point) | x <- [0 .. 2 * width - 1]]
    _ -> concat (zipWith segment points (drop 1 points))
  where
    lastIndex = max 1 (length samples - 1)
    points =
        [ ( round (fromIntegral i * fromIntegral (2 * width - 1) / fromIntegral lastIndex :: Double)
          , clamp 0 (4 * height - 1) $ round ((1 - y / peak) * fromIntegral (4 * height - 1) / 2)
          )
        | (i, y) <- zip [0 :: Int ..] samples
        ]
    segment (x0, y0) (x1, y1) =
        let steps = max 1 (max (abs (x1 - x0)) (abs (y1 - y0)))
            interpolate a b i = round (fromIntegral a + fromIntegral (b - a) * fromIntegral i / fromIntegral steps :: Double)
        in [(interpolate x0 x1 i, interpolate y0 y1 i) | i <- [0 .. steps]]

-- Unicode Braille numbers the bottom pair after the upper three pairs.
dotIndex :: Int -> Int -> Int
dotIndex x y
    | y == 3 = 6 + x
    | otherwise = y + 3 * x

plotBars :: Int -> Int -> [Double] -> [String]
plotBars width height values
    | width <= 0 || height <= 0 = []
    | null values = replicate height (replicate width ' ')
    | otherwise =
        let xs = bucketMaxima width values
            peak = max 1e-12 (maximum xs)
            normalized = map (clamp 0 1 . (/ peak)) xs
            column x =
                let filled = round (x * fromIntegral height)
                in replicate (height - filled) ' ' ++ replicate filled '█'
        in transpose (map column normalized)

bucketRanges :: Int -> [Double] -> [(Double, Double)]
bucketRanges width xs =
    [ rangeFor i | i <- [0 .. width - 1] ]
  where
    n = length xs
    rangeFor i =
        let start = min (n - 1) (i * n `div` width)
            rawEnd = (i + 1) * n `div` width
            end = max (start + 1) rawEnd
            bucket = take (end - start) (drop start xs)
        in case bucket of
            [] -> (0, 0)
            ys -> (minimum ys, maximum ys)

bucketMaxima :: Int -> [Double] -> [Double]
bucketMaxima width xs = map (max 0 . snd) (bucketRanges width xs)

ordered :: Ord a => a -> a -> (a, a)
ordered a b = (min a b, max a b)

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
