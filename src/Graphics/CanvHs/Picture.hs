--
-- A lot of this code was written by Gemini
--
module Graphics.CanvHs.Picture(
  Picture(..),
  MouseState(..),
  display,
  animate,
  playWithSound,
  play,
  reset,
  ) where
import Control.Exception(bracket_)
import Foreign.C.String(withCAString, CString)
import Graphics.CanvHs.Color
import Audio.AudHs.Sound

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

data MouseState = MouseState
  { mouseX      :: Double
  , mouseY      :: Double
  , mouseIsDown :: Bool
  } deriving (Show, Eq)

------------------------------------------------------------
-- JavaScript FFI Bindings

foreign import javascript unsafe "window.ctx.save();"
  js_save :: IO ()

foreign import javascript unsafe "window.ctx.restore();"
  js_restore :: IO ()

foreign import javascript unsafe "window.ctx.translate($0, $1);"
  js_translate :: Double -> Double -> IO ()

foreign import javascript unsafe "window.ctx.rotate($0);"
  js_rotate :: Double -> IO ()

foreign import javascript unsafe "window.ctx.scale($0, $1);"
  js_scale :: Double -> Double -> IO ()

foreign import javascript unsafe "window.ctx.beginPath();"
  js_beginPath :: IO ()

foreign import javascript unsafe "window.ctx.closePath();"
  js_closePath :: IO ()

foreign import javascript unsafe "window.ctx.moveTo($0, $1);"
  js_moveTo :: Double -> Double -> IO ()

foreign import javascript unsafe "window.ctx.lineTo($0, $1);"
  js_lineTo :: Double -> Double -> IO ()

foreign import javascript unsafe "window.ctx.fill();"
  js_fill :: IO ()

foreign import javascript unsafe "window.ctx.stroke();"
  js_stroke :: IO ()

foreign import javascript unsafe "var c = window.ctx; c.arc(0, 0, $0, 0, 2 * Math.PI); c.stroke();"
  js_strokeCircle :: Double -> IO ()

foreign import javascript unsafe "var c = window.ctx; c.arc(0, 0, $0, 0, 2 * Math.PI); c.fill();"
  js_fillCircle :: Double -> IO ()

foreign import javascript unsafe "var c = window.ctx; c.rect(-($0/2), -($1/2), $0, $1); c.stroke();"
  js_strokeRect :: Double -> Double -> IO ()

foreign import javascript unsafe "var c = window.ctx; c.rect(-($0/2), -($1/2), $0, $1); c.fill();"
  js_fillRect :: Double -> Double -> IO ()

foreign import javascript unsafe "var col = 'rgba(' + Math.round($0*255) + ',' + Math.round($1*255) + ',' + Math.round($2*255) + ',' + $3 + ')'; var c = window.ctx; c.fillStyle = col; c.strokeStyle = col;"
  js_setColor :: Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "window.ctx.clearRect(0, 0, window.cvs.width, window.cvs.height);"
  js_clearCanvas :: IO ()

foreign import javascript unsafe "var c = window.ctx; c.translate(window.cvs.width / 2, window.cvs.height / 2); c.scale(1, -1);"
  js_setupCoordinates :: IO ()

foreign import javascript unsafe "window.showCanvas();"
  js_showCanvas :: IO ()

-- We save, flip Y back to normal, draw the text, and restore.
foreign import javascript unsafe "window.drawHaskellText($0);"
  js_fillText :: CString -> IO ()

foreign import javascript unsafe "window.ctx.resetTransform();"
  js_resetTransform :: IO ()

foreign import javascript unsafe "console.log('js_log=', $0);"
  js_log :: Double -> IO ()

foreign import javascript unsafe "return performance.now()"
  js_now :: IO Double

foreign import javascript unsafe "return window.mouseState.x;"
  js_mouseX :: IO Double

foreign import javascript unsafe "return window.mouseState.y;"
  js_mouseY :: IO Double

foreign import javascript unsafe "return window.mouseState.down;"
  js_mouseDown :: IO Int

foreign import ccall unsafe "emscripten_sleep"
  emscripten_sleep :: Int -> IO ()

foreign import capi "value extern void c_begin_draw(void); c_begin_draw()"
  c_begin_draw :: IO ()

foreign import capi "value extern void c_end_draw(void); c_end_draw()"
  c_end_draw :: IO ()

foreign import capi "value extern void c_waitForFrame(void); c_waitForFrame()"
  js_waitForFrame :: IO ()

----------------------------------------------------------------

{-
withSaved :: IO () -> IO ()
withSaved = bracket_ js_save js_restore

withDrawLock :: IO a -> IO a
withDrawLock = bracket_ c_begin_draw c_end_draw
-}

drawing :: IO a -> IO a
drawing = bracket_ (c_begin_draw >> js_save) (js_restore >> c_end_draw)

-- Reset in case the transform stack gets messed up.
reset :: IO ()
reset = do
  js_resetTransform
  js_clearCanvas

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
  drawing $ do
    js_clearCanvas
    js_setupCoordinates
    renderPicture pic

--------------------------------

animate :: (Double -> Picture) -> IO ()
animate frameFunc = do
  js_showCanvas
  startTime <- js_now

  let loop = do
        currentTime <- js_now
        let dt = (currentTime - startTime) / 1000.0

        drawing $ do
          js_clearCanvas
          js_setupCoordinates
          renderPicture (frameFunc dt)

        js_waitForFrame

        loop

  loop

---------------------------------

playWithSound :: world
     -> (world -> Picture)
     -> (world -> Sound)
     -> (Double -> MouseState -> world -> world)
     -> IO ()
playWithSound initialWorld drawFunc soundFunc stepFunc = do
  js_showCanvas
  startTime <- js_now

  let loop lastTime world = do
        currentTime <- js_now
        let dt = (currentTime - lastTime) / 1000.0

        -- Get the mouse billboard
        mState <- MouseState <$> js_mouseX <*> js_mouseY <*> ((/= 0) <$> js_mouseDown)

        let newWorld = stepFunc dt mState world

        drawing $ do
          js_clearCanvas
          js_setupCoordinates
          renderPicture (drawFunc newWorld)

        js_waitForFrame

        playSound (soundFunc newWorld)

        loop currentTime newWorld

  loop startTime initialWorld

play :: world
     -> (world -> Picture)
     -> (Double -> MouseState -> world -> world)
     -> IO ()
play initialWorld drawFunc stepFunc = playWithSound initialWorld drawFunc (const silence) stepFunc

