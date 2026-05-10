module Graphics.CanvHs.Demo where
import Graphics.CanvHs

demo1 :: IO ()
demo1 = do
  let scene = Pictures 
        [ Color blue (SolidRectangle 200 200)
        , Color red (SolidCircle 50)
        ]
  display scene

demo2 :: IO ()
demo2 = do
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
