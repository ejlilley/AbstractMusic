{-# LANGUAGE GADTs #-}


module NGrams where


import Music (AbstractPhrase(..))

import Shortcuts

ngrams n [] = []
ngrams n xs = if (length xs) < n
              then []
              else (take n xs) : (ngrams n (tail xs))

apply2 f (x:y:[]) = f x y

horizontalIntervals = (map (apply2 (.-.))) . (ngrams 2)

-- todo: vertical intervals








