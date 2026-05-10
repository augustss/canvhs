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
  { ball_X    :: Double
  , ball_Y    :: Double
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
                   else (ball_X state, ball_Y state)
                   
  in DemoState newX newY newTime

-- The Draw Function (Rendering)
drawDemo :: DemoState -> Picture
drawDemo state = 
  let
    pulse = 1.0 + 0.2 * sin (totalTime state * 5.0)
    
    ball = Translate (ball_X state) (ball_Y state) $ 
           Scale pulse pulse $ 
           Color red (SolidCircle 30)
           
    -- Create the coordinate text
    coordString = "X: " ++ show (doubleToInt $ ball_X state) ++ ", Y: " ++ show (doubleToInt $ ball_Y state)
    
    -- Position the text slightly above and to the right of the ball
    label = Translate (ball_X state + 40) (ball_Y state + 40) $ 
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

-- 1. The Game State
data PongState = PongState
  { ballX    :: Double
  , ballY    :: Double
  , ballVX   :: Double  -- Velocity X
  , ballVY   :: Double  -- Velocity Y
  , paddle1Y :: Double  -- Player (Left)
  , paddle2Y :: Double  -- AI (Right)
  , score1   :: Int
  , score2   :: Int
  } deriving (Show)

-- Start the ball moving to the left
initialPongState :: PongState
initialPongState = PongState 0 0 (-400) 250 0 0 0 0

-- Game Constants
boardWidth, boardHeight, paddleW, paddleH, ballSize :: Double
boardWidth  = 800
boardHeight = 600
paddleW     = 50
paddleH     = 100
ballSize    = 15

-- 2. The Physics & Logic (Step Function)
stepPong :: Double -> MouseState -> PongState -> PongState
stepPong dt mouse state = 
  let
    -- A. Player Paddle (Snaps directly to mouse Y)
    p1Y = mouseY mouse

    -- B. AI Paddle (Chases the ball with a maximum speed)
    aiSpeed = 350.0
    p2Diff  = ballY state - paddle2Y state
    -- signum gives direction (1 or -1), min limits the speed per frame
    p2Move  = signum p2Diff * min (abs p2Diff) (aiSpeed * dt)
    p2Y     = paddle2Y state + p2Move

    -- C. Move the Ball
    nextX = ballX state + ballVX state * dt
    nextY = ballY state + ballVY state * dt

    -- D. Wall Collisions (Bounce off Top and Bottom)
    (nextY', vy') 
      | nextY >  (boardHeight / 2) = ( boardHeight / 2, -ballVY state)
      | nextY < -(boardHeight / 2) = (-boardHeight / 2, -ballVY state)
      | otherwise                  = (nextY, ballVY state)

    -- E. Paddle Collisions
    -- Check if ball is intersecting the X-plane of the paddles, AND within their Y-height
    hitLeft  = nextX < (-boardWidth/2 + paddleW) && abs (nextY' - p1Y) < (paddleH/2)
    hitRight = nextX > ( boardWidth/2 - paddleW) && abs (nextY' - p2Y) < (paddleH/2)

    vx' 
      | hitLeft   = abs (ballVX state) + 20   -- Bounce right, speed up slightly
      | hitRight  = -(abs (ballVX state) + 20)  -- Bounce left, speed up slightly
      | otherwise = ballVX state

    -- F. Scoring
    p2Scored = nextX < -(boardWidth/2 + 50)
    p1Scored = nextX >  (boardWidth/2 + 50)

    (finalX, finalY, finalVX, finalVY, finalS1, finalS2)
      | p1Scored  = (0, 0, -400, 250, score1 state + 1, score2 state)
      | p2Scored  = (0, 0,  400, 250, score1 state, score2 state + 1)
      | otherwise = (nextX, nextY', vx', vy', score1 state, score2 state)

  in PongState finalX finalY finalVX finalVY p1Y p2Y finalS1 finalS2

-- 3. The Rendering (Draw Function)
drawPong :: PongState -> Picture
drawPong state = 
  let
    bg = Color black (SolidRectangle 2000 2000)
    
    -- A thin center dividing line
    centerLine = Color white (SolidRectangle 2 boardHeight)

    -- The Paddles
    p1 = Translate (-boardWidth/2) (paddle1Y state) $ Color white (SolidRectangle paddleW paddleH)
    p2 = Translate ( boardWidth/2) (paddle2Y state) $ Color white (SolidRectangle paddleW paddleH)

    -- The Ball (Classic square shape)
    ball = Translate (ballX state) (ballY state) $ Color white (SolidRectangle ballSize ballSize)

    -- The Scores (Snapping to integers to avoid any lingering sub-pixel rendering issues)
    s1Text = Translate (-100) (boardHeight/2 - 40) $ Color white (Text (show $ score1 state))
    s2Text = Translate ( 100) (boardHeight/2 - 40) $ Color white (Text (show $ score2 state))

  in Pictures [bg, centerLine, p1, p2, ball, s1Text, s2Text]

-- 4. Execute
demo5 :: IO ()
demo5 = do
  putStrLn header
  putStrLn headerINT
  play initialPongState drawPong stepPong
