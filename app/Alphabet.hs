module Alphabet (incrementWord) where

import           Data.Char (chr, ord)

-- | Increment the word. a -> b -> ... -> z -> A -> B -> ... -> Z -> 0 -> 1 -> ... -> 9 -> aa -> ab -> ...
incrementWord :: String -> String
incrementWord word = a $ b word

-- | Prepend the letter 'a' to the string if there is a carry.
a :: (String, Bool) -> String
a (text, True)  = 'a':text
a (text, False) = text

-- | Increment the word and tell if there is a carry.
b :: String -> (String, Bool)
b ['z']  = (['A'], False)
b ['Z']  = (['0'], False)
b ['9']  = (['a'], True)
b [x]    = ([chr (ord x + 1)], False)
b (x:xs) = c x $ b xs
b ""     = ("a", False)

-- | Consume the carry to increment the character and tell if there is still a carry.
c :: Char -> (String, Bool) -> (String, Bool)
c 'z' (incremented, True) = ('A':incremented, False)
c 'Z' (incremented, True) = ('0':incremented, False)
c '9' (incremented, True) = ('a':incremented, True)
c x (incremented, True)   = (chr (ord x + 1):incremented, False)
c x (incremented, False)  = (x:incremented, False)
