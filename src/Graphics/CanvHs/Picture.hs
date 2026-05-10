module Graphics.CanvHs.Picture(
  Picture(..),
  display,
  ) where
import Graphics.CanvHs.Color

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

display :: Picture -> IO ()
display pic = do
  js_showCanvas
  js_save
  js_clearCanvas
  js_setupCoordinates
  renderPicture pic
  js_restore
