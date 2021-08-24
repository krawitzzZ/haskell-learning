module Modules.Geometry.Sphere
  ( volume
  , area
  )
where

volume :: Floating a => a -> a
volume radius = (4.0 / 3.0) * pi * (radius ^ (3 :: Integer))

area :: Floating a => a -> a
area radius = 4 * pi * (radius ^ (2 :: Integer))
