module Audio.AudHs.Sound(
  Waveform(..),
  Sound(..),
  silence,
  playSound,
  ) where
import Audio.AudHs.FFI

data Waveform = Sine | Square | Sawtooth | Triangle
  deriving (Show, Eq, Ord, Enum, Bounded)

data Sound = Sound
    { wave     :: Waveform
    , freq     :: Double               -- Hz
    , volume   :: Double               -- 0.0 - 1.0
    , duration :: Double               -- s
    }
    | Sounds [Sound]
    deriving (Show)

silence :: Sound
silence = Sounds []

playSound :: Sound -> IO ()
playSound (Sound w f vol dur) = do
    initAudio
    now <- currentTime
    
    osc  <- createOscillator (fromEnum w)
    gain <- createGain
    connectNodes osc gain
    connectNodes gain masterOutput
    
    setFrequency osc f now
    setGain gain vol now
    rampGain gain 0.0001 (now + dur)
    
    startNode osc now
    stopNode osc (now + dur)
playSound (Sounds ss) = mapM_ playSound ss
