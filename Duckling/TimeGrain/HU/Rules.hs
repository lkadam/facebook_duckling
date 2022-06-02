-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.HU.Rules
  ( rules ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain) ", "másodperc(ek)?|mp",    TG.Second)
         , ("minute (grain)" , "perc(ek)?",                 TG.Minute)
         , ("hour (grain)"   , "óra|órák",   TG.Hour)
         , ("day (grain)"    , "nap(ok)?",                  TG.Day)
         , ("week (grain)"   , "hét|hetek",            TG.Week)
         , ("month (grain)"  , "hónap|hónapok",   TG.Month)
         , ("quarter (grain)", "negyed\\s?év(ek)?",    TG.Quarter)
         , ("year (grain)"   , "év(ek)?",              TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
