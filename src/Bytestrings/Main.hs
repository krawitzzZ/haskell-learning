module Bytestrings.Main where

import qualified Data.ByteString.Lazy          as LazyBytes
import qualified Data.ByteString               as StrictBytes

fromStrictToLazy :: LazyBytes.ByteString
fromStrictToLazy = LazyBytes.fromChunks
  [ StrictBytes.pack [40, 41, 42]
  , StrictBytes.pack [43, 44, 45]
  , StrictBytes.pack [46, 47, 48]
  ]

lazyChunck :: LazyBytes.ByteString
lazyChunck = LazyBytes.cons 85 $ LazyBytes.pack [80, 81, 82, 84]

strictChunck :: LazyBytes.ByteString
strictChunck = LazyBytes.cons' 85 $ LazyBytes.pack [80, 81, 82, 84]

lazyChunck' :: LazyBytes.ByteString
lazyChunck' = foldr LazyBytes.cons LazyBytes.empty [50 .. 60]

strictChunck' :: LazyBytes.ByteString
strictChunck' = foldr LazyBytes.cons' LazyBytes.empty [50 .. 60]
