module Modules.Map where

import qualified Data.Set                      as Set

text1 :: String
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 :: String
text2 =
  "The old man left his garbage can out and now his trash is all over my lawn!"

set1 :: Set.Set Char
set1 = Set.fromList text1

set2 :: Set.Set Char
set2 = Set.fromList text2

inters :: Set.Set Char
inters = Set.intersection set1 set2

difers :: Set.Set Char
difers = Set.difference set1 set2

-- null, size, member, empty, singleton, insert and delete: some other functions on sets
