{- |
Module      :  Text.Ordinal
Copyright   :  (c) 2014 Elliot Robinson
               (c) 2014 Argiope Technical Solutions
License     :  BSD3

Maintainer  :  Elliot Robinson <elliot.robinson@argiopetech.com>
Stability   :  experimental
Portability :  portable

Provides conversion from 'Integral' types to their ordinal ("1st", "2nd", "3rd", etc) counterparts in 'Text' form.
-}

module Text.Ordinal where

import Data.Text.Lazy             (Text(), pack, append)
import Data.Text.Lazy.Builder     (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)

-- | Provides the ordinal form of an `Integral` type as `Text`
--
-- >>> showOrdinal 1001
-- "1001st"
showOrdinal :: Integral a => a -> Text
showOrdinal i = let pref = toLazyText $ decimal i
                    suff = ordinalSuffix i
                in pref `append` suff

-- | Returns the appropriate ordinal suffix for the provided `Integral`
--
-- >>> map ordinalSuffix [101, 102, 103, 113]
-- ["st", "nd", "rd", "th"]
ordinalSuffix :: Integral a => a -> Text
ordinalSuffix i = let i' = (abs i) `mod` 100
                  in if i' > 10 && i' < 20
                       then pack "th"
                       else pack $ go $ i' `mod` 10
    where go 1  = "st"
          go 2  = "nd"
          go 3  = "rd"
          go _  = "th"
