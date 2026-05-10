module Graphics.CanvHs.Picture(
  Picture(..),
  MouseState(..),
  display,
  animate,
  play,
  ) where
import Graphics.CanvHs.Color
import Foreign.C.String(withCAString, CString)

-- | Standard Gloss/Shine Picture primitives
data Picture
  = Blank
  | Line [(Double, Double)]          -- A stroked path
  | Polygon [(Double, Double)]       -- A filled polygon
  | Circle Double                    -- A stroked circle
  | SolidCircle Double               -- A filled circle
  | Rectangle Double Double          -- Width, Height (centered)
  | SolidRectangle Double Double     -- Width, Height (centered)
  | Color Color Picture              -- Applies color to child
  | Translate Double Double Picture  -- X, Y shift
  | Rotate Double Picture            -- Angle in degrees (clockwise)
  | Scale Double Double Picture      -- X, Y scaling
  | Pictures [Picture]               -- Group of pictures
  | Text String                      -- text

-- JavaScript FFI Bindings

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.save();"
  js_save :: IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.restore();"
  js_restore :: IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.translate($0, $1);"
  js_translate :: Double -> Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.rotate($0);"
  js_rotate :: Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.scale($0, $1);"
  js_scale :: Double -> Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.beginPath();"
  js_beginPath :: IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.closePath();"
  js_closePath :: IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.moveTo($0, $1);"
  js_moveTo :: Double -> Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.lineTo($0, $1);"
  js_lineTo :: Double -> Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.fill();"
  js_fill :: IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.stroke();"
  js_stroke :: IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.arc(0, 0, $0, 0, 2 * Math.PI); c.stroke();"
  js_strokeCircle :: Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.arc(0, 0, $0, 0, 2 * Math.PI); c.fill();"
  js_fillCircle :: Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.rect(-($0/2), -($1/2), $0, $1); c.stroke();"
  js_strokeRect :: Double -> Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); c.rect(-($0/2), -($1/2), $0, $1); c.fill();"
  js_fillRect :: Double -> Double -> IO ()

foreign import javascript unsafe "var c = document.getElementById('drawing-canvas').getContext('2d'); var col = 'rgba(' + Math.round($0*255) + ',' + Math.round($1*255) + ',' + Math.round($2*255) + ',' + $3 + ')'; c.fillStyle = col; c.strokeStyle = col;"
  js_setColor :: Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "var cvs = document.getElementById('drawing-canvas'); var c = cvs.getContext('2d'); c.clearRect(0, 0, cvs.width, cvs.height);"
  js_clearCanvas :: IO ()

foreign import javascript unsafe "var cvs = document.getElementById('drawing-canvas'); var c = cvs.getContext('2d'); c.translate(cvs.width / 2, cvs.height / 2); c.scale(1, -1);"
  js_setupCoordinates :: IO ()

foreign import javascript unsafe "window.showCanvas();"
  js_showCanvas :: IO ()

-- We save, flip Y back to normal, draw the text, and restore.
foreign import javascript unsafe "window.drawHaskellText($0);"
  js_fillText :: CString -> IO ()

foreign import javascript unsafe "console.log('js_log=', $0);"
  js_log :: Double -> IO ()

-- Rendering

drawPath :: [(Double, Double)] -> Bool -> IO ()
drawPath [] _ = return ()
drawPath ((startX, startY):pts) close = do
  js_beginPath
  js_moveTo startX startY
  mapM_ (\(x, y) -> js_lineTo x y) pts
  if close
    then do
      js_closePath
      js_fill
    else
      js_stroke

renderPicture :: Picture -> IO ()
renderPicture Blank = return ()
renderPicture (Pictures ps) = mapM_ renderPicture ps

renderPicture (Translate x y p) = do
  js_save
  js_translate x y
  renderPicture p
  js_restore

renderPicture (Scale x y p) = do
  js_save
  js_scale x y
  renderPicture p
  js_restore

renderPicture (Rotate deg p) = do
  js_save
  let radians = -(deg * pi / 180.0)
  js_rotate radians
  renderPicture p
  js_restore

renderPicture (Color (RGBA r g b a) p) = do
  js_save
  js_setColor r g b a
  renderPicture p
  js_restore

renderPicture (Line pts) = drawPath pts False
renderPicture (Polygon pts) = drawPath pts True

renderPicture (Circle r) = do
  js_beginPath
  js_strokeCircle r

renderPicture (SolidCircle r) = do
  js_beginPath
  js_fillCircle r

renderPicture (Rectangle w h) = do
  js_beginPath
  js_strokeRect w h

renderPicture (SolidRectangle w h) = do
  js_beginPath
  js_fillRect w h

renderPicture (Text str) =
  withCAString str js_fillText

display :: Picture -> IO ()
display pic = do
  js_showCanvas
  js_save
  js_clearCanvas
  js_setupCoordinates
  renderPicture pic
  js_restore

--------------------------------

-- Add this if you haven't already
foreign import javascript unsafe "return performance.now()"
  js_now :: IO Double

foreign import ccall unsafe "emscripten_sleep"
  emscripten_sleep :: Int -> IO ()

animate :: (Double -> Picture) -> IO ()
animate frameFunc = do
  js_showCanvas
  startTime <- js_now

  let loop = do
        -- 1. Mark the start of the frame
        frameStart <- js_now
        let t = (frameStart - startTime) / 1000.0

        -- 2. Do the heavy lifting
        js_save
        js_clearCanvas
        js_setupCoordinates
        renderPicture (frameFunc t)
        js_restore

        -- 3. Calculate how long the drawing took
        frameEnd <- js_now
        let drawTime = frameEnd - frameStart

            -- 1000ms / 60 frames = ~16.66ms per frame
            targetFrameTime = 16.66

            -- Calculate remaining time. Use `round` to convert Double to Int
            sleepTime = round (targetFrameTime - drawTime)

        -- 4. Sleep only the remaining time.
        -- If drawing took longer than 16ms, we sleep 0 (which still forces
        -- Emscripten to yield to the browser so it can paint the canvas).
        emscripten_sleep (max 0 sleepTime)

        -- 5. Repeat
        loop

  loop

---------------------------------

foreign import javascript unsafe "return window.mouseState.x;"
  js_mouseX :: IO Double

foreign import javascript unsafe "return window.mouseState.y;"
  js_mouseY :: IO Double

foreign import javascript unsafe "return window.mouseState.down;"
  js_mouseDown :: IO Int

data MouseState = MouseState
  { mouseX      :: Double
  , mouseY      :: Double
  , mouseIsDown :: Bool
  } deriving (Show, Eq)

play :: world
     -> (world -> Picture)
     -> (Double -> MouseState -> world -> world)
     -> IO ()
play initialWorld drawFunc stepFunc = do
  js_showCanvas

  -- Get the absolute start time to kick off the loop
  startTime <- js_now

  let loop lastTime world = do
        -- 1. Calculate Delta Time (dt) using the passed-in lastTime
        currentTime <- js_now
        let dt = (currentTime - lastTime) / 1000.0

        -- 2. Read the Mouse Billboard
        mState <- MouseState <$> js_mouseX <*> js_mouseY <*> ((/= 0) <$> js_mouseDown)

        -- 3. Update the World State
        let newWorld = stepFunc dt mState world

        -- 4. Draw the new World
        frameStart <- js_now
        js_save
        js_clearCanvas
        js_setupCoordinates
        renderPicture (drawFunc newWorld)
        js_restore

        -- 5. Calculate sleep time for 60 FPS (~16.66ms per frame)
        frameEnd <- js_now
        let drawTime = frameEnd - frameStart
            targetFrameTime = 16.66
            sleepTime = round (targetFrameTime - drawTime)

        -- 6. Sleep and recurse with the NEW time and NEW world
        emscripten_sleep (max 0 sleepTime)
        loop currentTime newWorld

  loop startTime initialWorld
