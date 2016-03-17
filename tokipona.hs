-- | Toki Pona is a contructed language of only around ~120 words.
-- The functions below allow one to count the number of *possible* words,
-- given the phonological rules presented the official Toki Pona manual.
--
-- Note: While words typically follow the "C+V+C+V..." pattern, words
-- like `tenpo` (time) and `kepeken` (to use) tell us we need to consider
-- all permutations of "C+V+n" syllables as well.
--
-- Note: The longest Toki Pona words are only three syllables long.

module TokiPona where

---

vowels :: String
vowels = "aiueo"

consonants :: String
consonants = "ptkmnslwj"

-- | All single syllables, except those that end in `n`.
singles :: [String]
singles = withoutN ++ withN
  where withoutN = (\a b -> [a, b]) <$> consonants <*> vowels
        withN = map (++ "n") withoutN

-- | All two-syllable words.
twos :: [String]
twos = (++) <$> singles <*> singles

-- | All three-syllable words.
threes :: [String]
threes = (\a b c -> concat [a,b,c]) <$> singles <*> singles <*> singles

-- | The number of possible Toki Pona words.
total :: Int
total = length singles + length twos + length threes
