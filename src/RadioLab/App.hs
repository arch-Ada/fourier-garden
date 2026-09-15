module RadioLab.App (runRadioLab) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Text.Printf (printf)
import System.IO (hFlush, stdout)

import RadioLab.Fourier
import RadioLab.Modulation
import RadioLab.Plot
import RadioLab.Signal

data Name = Viewport deriving (Eq, Ord, Show)

data Mode = PureTone | AM | DSB | FM | CW | Mixer | Harmonics | Aliasing deriving (Eq, Show, Enum, Bounded)

data Model = Model
    { modelMode :: Mode
    , carrierHz :: Double
    , messageHz :: Double
    , modulation :: Double
    , sampleRate :: Double
    , timeWindowMs :: Double
    , showEnvelope :: Bool
    }

analysisSampleCount :: Int
analysisSampleCount = 1024

runRadioLab :: IO ()
runRadioLab = do
    requestTerminalSize 38 85
    _ <- defaultMain app initialModel
    pure ()

requestTerminalSize :: Int -> Int -> IO ()
requestTerminalSize rows cols = do
    printf "\ESC[8;%d;%dt" rows cols
    hFlush stdout

initialModel :: Model
initialModel = Model
    { modelMode = AM
    , carrierHz = 1000
    , messageHz = 100
    , modulation = 0.7
    , sampleRate = 8192
    , timeWindowMs = 20
    , showEnvelope = True
    }

app :: App Model e Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = const attributes
    }

attributes :: AttrMap
attributes = attrMap V.defAttr
    [ (attrName "title", fg V.cyan `V.withStyle` V.bold)
    , (attrName "accent", fg V.yellow)
    , (attrName "dim", fg V.brightBlack)
    ]

drawUI :: Model -> [Widget Name]
drawUI model =
    [ centerLayer
        $ borderWithLabel (withAttr (attrName "title") (str " Fourier Garden · Radio Lab "))
        $ padAll 1
        $ vBox
            [ infoLine model
            , padTop (Pad 1) $ hBorder
            , hBox
                [ withAttr (attrName "accent") (str "TIME DOMAIN")
                , str $ printf "   %.3f ms window · %d samples" (sampledWindowMs model) (length timeSamples)
                , if envelopeVisible model then withAttr (attrName "dim") (str " · envelope") else emptyWidget
                ]
            , strLines (plotWaveWithEnvelope 80 11 timeSamples envelopeSamples)
            , hBorder
            , withAttr (attrName "accent") (str "FREQUENCY DOMAIN")
            , strLines (plotBars 80 11 magnitudes)
            , padTop (Pad 1) (spectrumFacts model bins)
            , padTop (Pad 1) (controls model)
            ]
    ]
  where
    signal = currentSignal model
    timeSamples = sampleForWindow model signal
    envelopeSamples = fmap (sampleForWindow model) (currentEnvelope model)
    analysisSamples = sampleSignal (sampleRate model) analysisSampleCount signal
    bins = spectrum (sampleRate model) analysisSamples
    magnitudes = map binMagnitude bins

infoLine :: Model -> Widget Name
infoLine model =
    hBox
        [ str "Mode "
        , withAttr (attrName "accent") . str $ modeName (modelMode model)
        , str $ printf "   carrier %.0f Hz   " (effectiveCarrierHz model)
        , infoValue (toneAdjustable model)
            (printf "tone %.0f Hz   " (messageHz model))
        , infoValue (modulationAdjustable model)
            (printf "modulation %.2f   " (modulation model))
        , str $ printf "Fs %.0f Hz" (sampleRate model)
        ]

infoValue :: Bool -> String -> Widget Name
infoValue True text = str text
infoValue False text = withAttr (attrName "dim") (str text)

spectrumFacts :: Model -> [SpectrumBin] -> Widget Name
spectrumFacts model bins =
    let deltaF = sampleRate model / fromIntegral analysisSampleCount
        nyquist = sampleRate model / 2
        resolution = printf "FFT %d · Δf %.2f Hz · Nyquist %.0f Hz" analysisSampleCount deltaF nyquist
    in case dominantFrequency (drop 1 bins) of
        Nothing -> str resolution
        Just f  -> str (resolution ++ printf " · strongest non-DC %.1f Hz" f)

controls :: Model -> Widget Name
controls model =
    vBox
        [ hBox
            [ control True "(m) mode    "
            , control True "([/]) time domain zoom    "
            , control (carrierAdjustable model) "(←/→) carrier    "
            , control (envelopeAdjustable model) "(e) envelope"
            ]
        , hBox
            [ control True "(q) quit    "
            , control True "(s/S) sample rate         "
            , control (toneAdjustable model) "(↑/↓) tone       "
            , control True "(r) reset"
            ]
        , hBox
            [ str "            "
            , control (modulationAdjustable model) "(+/-) modulation"
            ]
        ]

control :: Bool -> String -> Widget Name
control True text = str text
control False text = withAttr (attrName "dim") (str text)

handleEvent :: BrickEvent Name e -> EventM Name Model ()
handleEvent event = case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
    VtyEvent (V.EvKey (V.KChar 'm') []) -> modify nextMode

    VtyEvent (V.EvKey V.KRight []) ->
        modify (adjustWhen carrierAdjustable (adjustCarrier 50))
    VtyEvent (V.EvKey V.KLeft []) ->
        modify (adjustWhen carrierAdjustable (adjustCarrier (-50)))
    VtyEvent (V.EvKey V.KRight [V.MShift]) ->
        modify (adjustWhen carrierAdjustable (adjustCarrier 10))
    VtyEvent (V.EvKey V.KLeft [V.MShift]) ->
        modify (adjustWhen carrierAdjustable (adjustCarrier (-10)))
    VtyEvent (V.EvKey V.KRight [V.MCtrl]) ->
        modify (adjustWhen carrierAdjustable (adjustCarrier 1))
    VtyEvent (V.EvKey V.KLeft [V.MCtrl]) ->
        modify (adjustWhen carrierAdjustable (adjustCarrier (-1)))

    VtyEvent (V.EvKey V.KUp []) ->
        modify (adjustWhen toneAdjustable (adjustMessage 10))
    VtyEvent (V.EvKey V.KDown []) ->
        modify (adjustWhen toneAdjustable (adjustMessage (-10)))
    VtyEvent (V.EvKey V.KUp [V.MShift]) ->
        modify (adjustWhen toneAdjustable (adjustMessage 1))
    VtyEvent (V.EvKey V.KDown [V.MShift]) ->
        modify (adjustWhen toneAdjustable (adjustMessage (-1)))

    VtyEvent (V.EvKey (V.KChar '+') []) ->
        modify (adjustWhen modulationAdjustable (adjustModulation 0.1))
    VtyEvent (V.EvKey (V.KChar '-') []) ->
        modify (adjustWhen modulationAdjustable (adjustModulation (-0.1)))

    VtyEvent (V.EvKey (V.KChar '[') []) ->
        modify (\m -> m {timeWindowMs = max 0.05 (timeWindowMs m / 2)})
    VtyEvent (V.EvKey (V.KChar ']') []) ->
        modify (\m -> m {timeWindowMs = min 2000 (timeWindowMs m * 2)})

    VtyEvent (V.EvKey (V.KChar 'e') []) ->
        modify (adjustWhen envelopeAdjustable
            (\m -> m {showEnvelope = not (showEnvelope m)}))

    VtyEvent (V.EvKey (V.KChar 's') []) ->
        modify (\m -> m {sampleRate = min 65536 (sampleRate m * 2)})
    VtyEvent (V.EvKey (V.KChar 'S') []) ->
        modify (\m -> m {sampleRate = max 512 (sampleRate m / 2)})

    VtyEvent (V.EvKey (V.KChar 'r') []) ->
        put initialModel

    _ -> pure ()

adjustCarrier :: Double -> Model -> Model
adjustCarrier delta model = model {carrierHz = max 1 (carrierHz model + delta)}

adjustMessage :: Double -> Model -> Model
adjustMessage delta model = model {messageHz = max 1 (messageHz model + delta)}

nextMode :: Model -> Model
nextMode model = adjustModulation 0 (model {modelMode = succWrap (modelMode model)})

adjustModulation :: Double -> Model -> Model
adjustModulation delta model =
    model {modulation = clampValue 0 limit (modulation model + delta)}
  where
    limit = if modelMode model == AM then 1 else 5

succWrap :: (Eq a, Enum a, Bounded a) => a -> a
succWrap x = if x == maxBound then minBound else succ x

modeName :: Mode -> String
modeName PureTone = "Pure carrier"
modeName AM = "AM"
modeName DSB = "DSB-SC"
modeName FM = "FM"
modeName CW = "CW keying"
modeName Mixer = "Mixer products"
modeName Harmonics = "Harmonics"
modeName Aliasing = "Aliasing demo"

carrierAdjustable :: Model -> Bool
carrierAdjustable model =
    modelMode model /= Aliasing

toneAdjustable :: Model -> Bool
toneAdjustable model =
    modelMode model `elem` [AM, DSB, FM, CW, Mixer]

modulationAdjustable :: Model -> Bool
modulationAdjustable model =
    modelMode model `elem` [AM, FM]

envelopeAdjustable :: Model -> Bool
envelopeAdjustable model =
    modelMode model == AM

adjustWhen :: (Model -> Bool) -> (Model -> Model) -> Model -> Model
adjustWhen allowed action model
    | allowed model = action model
    | otherwise = model

currentSignal :: Model -> Signal
currentSignal model =
    let fc = effectiveCarrierHz model
        fm' = messageHz model
        depth = modulation model
        message = sine fm' 0
    in case modelMode model of
        PureTone -> sine fc 0
        AM -> am fc depth message
        DSB -> dsbSc fc message
        FM -> fm fc depth fm'
        CW -> keyedCarrier fc fm'
        Mixer -> mix (sine fc 0) (sine fm' 0)
        Harmonics -> square fc
        Aliasing -> sine fc 0

effectiveCarrierHz :: Model -> Double
effectiveCarrierHz model
    | modelMode model == Aliasing = 0.72 * sampleRate model
    | otherwise = carrierHz model

currentEnvelope :: Model -> Maybe Signal
currentEnvelope model
    | not (envelopeVisible model) = Nothing
    | otherwise = case modelMode model of
        AM ->
            let depth = clampValue 0 1 (modulation model)
                message = sine (messageHz model) 0
            in Just (\t -> 1 + depth * message t)
        _ -> Nothing

envelopeVisible :: Model -> Bool
envelopeVisible model = showEnvelope model && modelMode model == AM

sampleForWindow :: Model -> Signal -> [Double]
sampleForWindow model signal =
    sampleSignal (sampleRate model) (windowSampleCount model) signal

windowSampleCount :: Model -> Int
windowSampleCount model = clampValue 2 8192 requested
  where
    requested = ceiling (sampleRate model * timeWindowMs model / 1000)

-- Duration of the sampled block (N / Fs), including the final sample interval.
sampledWindowMs :: Model -> Double
sampledWindowMs model = 1000 * fromIntegral (windowSampleCount model) / sampleRate model

strLines :: [String] -> Widget n
strLines = vBox . map str

clampValue :: Ord a => a -> a -> a -> a
clampValue lo hi = max lo . min hi
