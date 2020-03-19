module Exercises.DaPhone where

import           Data.List
import           Data.Maybe
import qualified Data.Char                     as Ch

-- validButtons = "1234567890*#.,+ "
type Symbol = Char

-- Valid presses: 1 and up
type Presses = Int

-- [('2', 1)]
type KeyPress = (Symbol, Presses)

class DaPhoneButton a where
  hasSymbol :: a -> Symbol -> Bool
  nextSymbol :: a -> Symbol -> Symbol
  getKeyPresses :: a -> Symbol -> KeyPress
  getLetter :: a -> KeyPress -> Symbol

data DaPhone = DaPhone
  {
    one :: PhoneButton
  , two :: PhoneButton
  , three :: PhoneButton
  , four :: PhoneButton
  , five :: PhoneButton
  , six :: PhoneButton
  , seven :: PhoneButton
  , eight :: PhoneButton
  , nine :: PhoneButton
  , zero :: PhoneButton
  , asterix :: PhoneButton
  , hashtag :: PhoneButton
  }
  deriving (Eq, Show, Ord)

newtype PhoneButton = Button [Symbol] deriving (Eq, Show, Ord)

instance DaPhoneButton PhoneButton where
  hasSymbol (Button keys) s = Ch.toLower s `elem` keys

  nextSymbol (Button keys) s = nextSymbol' keys keys
   where
     -- TODO: handle more gracefully
    nextSymbol' [] _ = error "nextSymbol -> PhoneButton without symbols"
    nextSymbol' [s'] acc = if s' == s then head acc else nextSymbol' [] acc
    nextSymbol' (s' : ss) acc = if s' == s then head ss else nextSymbol' ss acc

  getKeyPresses (Button []) _ =
    error "getKeyPresses -> PhoneButton without symbols"
  getKeyPresses (Button [s']) c =
    if c == s' then (s', 1) else error "getKeyPresses -> no such symbol"
  getKeyPresses btn@(Button (s' : ss)) c =
    (s', getPresses (head ss) (1 :: Int))
   where
    getPresses c' presses = if Ch.toLower c == c'
      then presses
      else getPresses (nextSymbol btn c') presses + 1

  getLetter _btn@(Button keys) (_, presses) =
    if length keys == presses then head keys else keys !! presses

daPhone :: DaPhone
daPhone = DaPhone { one     = Button ['1', '.', ',']
                  , two     = Button ['2', 'a', 'b', 'c']
                  , three   = Button ['3', 'd', 'e', 'f']
                  , four    = Button ['4', 'g', 'h', 'i']
                  , five    = Button ['5', 'j', 'k', 'l']
                  , six     = Button ['6', 'm', 'n', 'o']
                  , seven   = Button ['7', 'p', 'q', 'r', 's']
                  , eight   = Button ['8', 't', 'u', 'v']
                  , nine    = Button ['9', 'w', 'x', 'y', 'z']
                  , zero    = Button ['0', '+', ' ']
                  , hashtag = Button ['#']
                  , asterix = Button ['*']
                  }



buttons :: DaPhone -> [PhoneButton]
buttons ph =
  [ one ph
  , two ph
  , three ph
  , four ph
  , five ph
  , six ph
  , seven ph
  , eight ph
  , nine ph
  , zero ph
  , hashtag ph
  , asterix ph
  ]

buttonForSymbol :: DaPhone -> Symbol -> PhoneButton
buttonForSymbol ph s =
  fromMaybe (error $ "buttonForSymbol -> No such symbol in the phone: " ++ [s])
    $ find (`hasSymbol` s) (buttons ph)

buttonForKeyPress :: DaPhone -> KeyPress -> PhoneButton
buttonForKeyPress ph (s, _) =
  fromMaybe (error $ "buttonForKeyPress -> No such symbol in the phone" ++ [s])
    $ find (`hasSymbol` s) (buttons ph)

toUpper :: [KeyPress] -> [KeyPress]
toUpper kp = ('*', 1) : kp

-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
parseLetter :: DaPhone -> Char -> [KeyPress]
parseLetter ph c | Ch.isUpper c = toUpper [keyPresses]
                 | otherwise    = [keyPresses]
  where keyPresses = getKeyPresses (buttonForSymbol ph c) c

parseSentence :: DaPhone -> String -> [KeyPress]
parseSentence ph st = concatMap (parseLetter ph) st

writeLetter :: DaPhone -> KeyPress -> Char
writeLetter ph kp = getLetter (buttonForKeyPress ph kp) kp

writeSentence :: DaPhone -> [KeyPress] -> String
writeSentence ph kp = map (writeLetter ph) kp

conversation :: [String]
conversation =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  , "lol wtf lol hah"
  ]

fingerTaps :: [KeyPress] -> Presses
fingerTaps = foldr (\(_, cost) count -> cost + count) 0

mostPopularLetter :: String -> Char
mostPopularLetter str =
  let (popularLetter, _) = maximumBy compareCountResult $ mapAndCount str
      compareCountResult (_, a) (_, a') = compare (a :: Int) (a' :: Int)
      mapAndCount ls = map (mapLetters ls) ls
      mapLetters ls l = foldl countLetters (l, 0) ls
      countLetters (l, count) l' =
          if l' `notElem` ". ," && l == l' then (l, count + 1) else (l, count)
  in  popularLetter

mostPopularLetterCost :: Char -> Int
mostPopularLetterCost c = fingerTaps $ parseLetter daPhone c

coolestLetter :: [String] -> Char
coolestLetter strs = mostPopularLetter $ concat strs
