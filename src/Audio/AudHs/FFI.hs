module Audio.AudHs.FFI(module Audio.AudHs.FFI) where

newtype AudioNode = AudioNode Int
  deriving (Show, Eq, Ord)

masterOutput :: AudioNode
masterOutput = AudioNode 0

foreign import javascript unsafe "js_initAudio()"                     initAudio        :: IO ()
foreign import javascript unsafe "return js_currentTime()"            currentTime      :: IO Double
foreign import javascript unsafe "return js_createOscillator($1)"     createOscillator :: Int -> IO AudioNode
foreign import javascript unsafe "return js_createGain()"             createGain       :: IO AudioNode
foreign import javascript unsafe "js_connect($1, $2)"                 connectNodes     :: AudioNode -> AudioNode -> IO ()
foreign import javascript unsafe "js_setFrequency($1, $2, $3)"        setFrequency     :: AudioNode -> Double -> Double -> IO ()
foreign import javascript unsafe "js_setGain($1, $2, $3)"             setGain          :: AudioNode -> Double -> Double -> IO ()
foreign import javascript unsafe "js_rampGain($1, $2, $3)"            rampGain         :: AudioNode -> Double -> Double -> IO ()
foreign import javascript unsafe "js_startNode($1, $2)"               startNode        :: AudioNode -> Double -> IO ()
foreign import javascript unsafe "js_stopNode($1, $2)"                stopNode         :: AudioNode -> Double -> IO ()
