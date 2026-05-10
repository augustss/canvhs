module Graphics.CanvHs.Demo where
import Data.Double
import Graphics.CanvHs

header :: String
header = "Source at https://github.com/augustss/canvhs"

headerINT :: String
headerINT = "Use CTRL-C to interrupt"

--------------------------------------------------------

demo1 :: IO ()
demo1 = do
  putStrLn header
  let scene = Pictures 
        [ Color blue (SolidRectangle 200 200)
        , Color red (SolidCircle 50)
        ]
  display scene

--------------------------------------------------------

demo2 :: IO ()
demo2 = do
  putStrLn header
  let background = Color darkGray (SolidRectangle 800 800)
  display (Pictures [background, lotusMandala])

-- Draws a 12-petal lotus flower
lotusMandala :: Picture
lotusMandala = Pictures [ Rotate (angle * 30) (petal angle) | angle <- [0..11] ]
  where
    -- Alternates colors based on the angle index
    petal a 
      | even (round a) = Color pink    (Translate 0 80 (SolidCircle 40))
      | otherwise      = Color magenta (Translate 0 80 (SolidCircle 40))


--------------------------------------------------------

demo3 :: IO ()
demo3 = do
  putStrLn header
  putStrLn headerINT
  animate solarSystem

solarSystem :: Double -> Picture
solarSystem t = 
  let
    -- Sun
    sun = Color yellow (SolidCircle 50)
    
    -- Earth
    earth = Rotate (t * 45.0) $ Translate 150 0 $ Pictures
      [ Color blue (SolidCircle 20) 
      , moon 
      ]
      
    -- Moon
    moon = Rotate (t * 180.0) $ Translate 40 0 $ 
           Color white (SolidCircle 8)
           
    -- Space
    background = Color black (SolidRectangle 2000 2000)
    
  in Pictures [background, sun, earth]

--------------------------------------------------------

data DemoState = DemoState
  { ballX     :: Double
  , ballY     :: Double
  , totalTime :: Double
  } deriving (Show)

initialState :: DemoState
initialState = DemoState 0 0 0

-- The Step Function (Logic)
stepDemo :: Double -> MouseState -> DemoState -> DemoState
stepDemo dt mouse state = 
  let 
    -- Always accumulate time so the pulsation never stops
    newTime = totalTime state + dt
    
    -- Only update the ball's target coordinates if the mouse is down
    (newX, newY) = if mouseIsDown mouse
                   then (mouseX mouse, mouseY mouse)
                   else (ballX state, ballY state)
                   
  in DemoState newX newY newTime

-- The Draw Function (Rendering)
drawDemo :: DemoState -> Picture
drawDemo state = 
  let
    pulse = 1.0 + 0.2 * sin (totalTime state * 5.0)
    
    ball = Translate (ballX state) (ballY state) $ 
           Scale pulse pulse $ 
           Color red (SolidCircle 30)
           
    -- Create the coordinate text
    coordString = "X: " ++ show (doubleToInt $ ballX state) ++ ", Y: " ++ show (doubleToInt $ ballY state)
    
    -- Position the text slightly above and to the right of the ball
    label = Translate (ballX state + 40) (ballY state + 40) $ 
            Color white (Text coordString)
           
    background = Color black (SolidRectangle 2000 2000)
    
  in Pictures [background, ball, label]

demo4 :: IO ()
demo4 = do
  putStrLn header
  putStrLn headerINT
  putStrLn "Push the mouse button and drag it around in the animation window."
  play initialState drawDemo stepDemo

--------------------------------------------------------

