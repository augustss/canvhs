module Graphics.CanvHs.Color(module Graphics.CanvHs.Color) where

-- | A basic RGBA color representation (values from 0.0 to 1.0)
data Color = RGBA Double Double Double Double

-- Grayscale
black, darkGray, gray, lightGray, white :: Color
black     = RGBA 0.0 0.0 0.0 1.0
darkGray  = RGBA 0.2 0.2 0.2 1.0
gray      = RGBA 0.5 0.5 0.5 1.0
lightGray = RGBA 0.8 0.8 0.8 1.0
white     = RGBA 1.0 1.0 1.0 1.0

-- Primary & Secondary
red, green, blue, yellow, cyan, magenta :: Color
red       = RGBA 1.0 0.0 0.0 1.0
green     = RGBA 0.0 1.0 0.0 1.0
blue      = RGBA 0.0 0.0 1.0 1.0
yellow    = RGBA 1.0 1.0 0.0 1.0
cyan      = RGBA 0.0 1.0 1.0 1.0
magenta   = RGBA 1.0 0.0 1.0 1.0

-- Extended Palette
orange, purple, pink, brown, lime, teal :: Color
orange    = RGBA 1.0 0.5 0.0 1.0
purple    = RGBA 0.5 0.0 0.5 1.0
pink      = RGBA 1.0 0.4 0.7 1.0
brown     = RGBA 0.6 0.3 0.1 1.0
lime      = RGBA 0.5 1.0 0.0 1.0
teal      = RGBA 0.0 0.5 0.5 1.0

-- Transparent/Utilities
transparent :: Color
transparent = RGBA 0.0 0.0 0.0 0.0

-- Helper function to quickly create colors with custom opacity (alpha)
withAlpha :: Double -> Color -> Color
withAlpha a (RGBA r g b _) = RGBA r g b a
