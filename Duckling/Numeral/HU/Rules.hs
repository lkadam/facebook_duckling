-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.HU.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "nulla", 0 )
  , ( "zéró", 0 )
  , ( "egy", 1 )
  , ( "kettő", 2 )
  , ( "három", 3 )
  , ( "négy", 4 )
  , ( "öt", 5)
  , ( "hat", 6)
  , ( "hét", 7)
  , ( "nyolc", 8)
  , ( "kilenc", 9)
  , ( "tíz", 10)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(nulla|zéró|egy|kettő|három|négy|öt|hat|hét|nyolc|kilenc|tíz)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "tizenegy", 11 )
  , ( "tizenkettő", 12 )
  , ( "tizenhárom", 13 )
  , ( "tizennégy", 14 )
  , ( "tizenöt", 15 )
  , ( "tizenhat", 16 )
  , ( "tizenhét", 17 )
  , ( "tizennyolc", 18 )
  , ( "tizenkilenc", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(tizenegy|tizenkettő|tizenhárom|tizennégy|tizenöt|tizenhat|tizenhét|tizennyolc|tizenkilenc)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "huszonegy", 21 )
  , ( "huszonkettő", 22 )
  , ( "huszonhárom", 23 )
  , ( "huszonnégy", 24 )
  , ( "huszonöt", 25 )
  , ( "huszonhat", 26 )
  , ( "huszonhét", 27 )
  , ( "huszonnyolc", 28 )
  , ( "huszonkilenc", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(huszonegy|huszonkettő|huszonhárom|huszonnégy|huszonöt|huszonhat|huszonhét|huszonnyolc|huszonkilenc)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "húsz", 20 )
  , ( "harminc", 30 )
  , ( "negyven", 40 )
  , ( "ötven", 50 )
  , ( "hatvan", 60 )
  , ( "hetven", 70 )
  , ( "nyolcvan", 80 )
  , ( "kilencven", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(húsz|harminc|negyven|ötven|hatvan|hetven|nyolcvan|kilencven)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([3-9][1-9])"
  , pattern =
    [ regex "(harminc|negyven|ötven|hatvan|hetven|nyolcvan|kilencven)(egy|kettő|három|négy|öt|hat|hét|nyolc|kilenc)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- HashMap.lookup (Text.toLower m1) tensMap
        v2 <- HashMap.lookup (Text.toLower m2) ruleNumeralMap
        integer $ v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeral
  , ruleElevenToNineteen
  , ruleTwentyoneToTwentynine
  , ruleTens
  , ruleCompositeTens
  ]
