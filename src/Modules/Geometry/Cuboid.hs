module Modules.Geometry.Cuboid
  ( volume
  , area
  )
where


area :: Float -> Float -> Float -> Float
area a b c = rectangeArea a b * 2 + rectangeArea a c * 2 + rectangeArea c b * 2

volume :: Float -> Float -> Float -> Float
volume a b c = rectangeArea a b * c

rectangeArea :: Float -> Float -> Float
rectangeArea a b = a * b
