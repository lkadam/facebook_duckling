-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HU.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Time.Helpers
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG


ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) -> Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("right now",            TG.Second, 0 , "mostani(t)?|((épp )?most)|azonnal"                      )
  , ("today",                TG.Day,    0 , "m(á|a)(i( nap(o(t|n)|it?)?|t)?|ma)?"                         )
  , ("tomorrow",             TG.Day,    1 , "holnap(i(t)?)?"                                              )
  , ("day after tomorrow",   TG.Day,    2 , "holnapután(i(t)?)?"                                     )
  , ("yesterday",            TG.Day,    -1, "tegnap(i(t)?)?"                                              )
  , ("day before yesterday", TG.Day,    -2, "tegnapelőtt(i(t)?)?"                                    )
  , ("end of month",         TG.Month,  1 , "(a )?hónap vége|hó vég(e|i(t)?|én)" )
  , ("end of year",          TG.Year,   1 , "(az )?év vég(e|i(t)?|én)"                     )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "hétfő(n|t|i(t)?)?|hét\\.?"                   )
  , ( "Tuesday"  , "kedd(en|et|i(t)?)?"                                         )
  , ( "Wednesday", "szerda(i(t)?)?|szerdá(n|t)|szer\\.?"                        )
  , ( "Thursday" , "csütörtök(ö(n|t)|i(t)?)?|csüt\\.?" )
  , ( "Friday"   , "péntek(e(n|t)|i(t)?)?|pén\\.?"                    )
  , ( "Saturday" , "szombat(o(n|t)|i(t)?)?|szom\\.?"                            )
  , ( "Sunday"   , "vasárnap(ot|i(t)?)?|vas\\.?"                           )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"  , "január(ban|i(t)?)?|jan\\.?"         )
  , ( "February" , "február(ban|i(t)?)?|febr?\\.?"      )
  , ( "March"    , "március(ban|i(t)?)?|márc?\\.?" )
  , ( "April"    , "április(ban|i(t)?)?|ápr\\.?"   )
  , ( "May"      , "május(ban|i(t)?)?|máj\\.?"     )
  , ( "June"     , "június(ban|i(t)?)?|jún\\.?"    )
  , ( "July"     , "július(ban|i(t)?)?|júl\\.?"    )
  , ( "August"   , "augusztus(ban|i(t)?)?|aug\\.?"           )
  , ( "September", "szeptember(ben|i(t)?)?|szept?\\.?"       )
  , ( "October"  , "október(ben|i(t)?)?|okt\\.?"             )
  , ( "November" , "november(ben|i(t)?)?|nov\\.?"            )
  , ( "December" , "december(ben|i(t)?)?|dec\\.?"            )
  ]

ruleMonthDOMNumeral :: Rule
ruleMonthDOMNumeral = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleMonthDOMNumeralSuffix :: Rule
ruleMonthDOMNumeralSuffix = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    , regex "(-?(jei|jén|án|én|ai|ei|je|ji|a|e|i))"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleMonthDOMOrdinal :: Rule
ruleMonthDOMOrdinal = Rule
  { name = "<named-month> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleCycleThisLastNext :: Rule
ruleCycleThisLastNext = Rule
  { name = "this|last|next <cycle>"
  , pattern =
    [ regex "(most|előző|múlt|következő|jövő)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:_) ->
        case Text.toLower match of
          "most"                -> tt $ cycleNth grain 0
          "előző"     -> tt . cycleNth grain $ - 1
          "múlt"           -> tt . cycleNth grain $ - 1
          "következő" -> tt $ cycleNth grain 1
          "jövő"      -> tt $ cycleNth grain 1
          _ -> Nothing
      _ -> Nothing
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "next <day-of-week>"
  , pattern =
    [ regex "jövő"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 1 True td
      _ -> Nothing
  }

ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }

ruleHHMMSS :: Rule
ruleHHMMSS = Rule
  { name = "hh:mm:ss"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond True h m s
      _ -> Nothing
  }

ruleTODLatent :: Rule
ruleTODLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour True n
      _ -> Nothing
  }

ruleTODAM :: Rule
ruleTODAM = Rule
  { name = "am <time-of-day>"
  , pattern =
    [ regex "(de\\.?|délelőtt)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ timeOfDayAMPM True td
      _ -> Nothing
  }

ruleTODPM :: Rule
ruleTODPM = Rule
  { name = "pm <time-of-day>"
  , pattern =
    [ regex "(du\\.?|délután)"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ timeOfDayAMPM False td
      _ -> Nothing
  }

ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
  { name = "yyyy.mm.dd"
  , pattern =
    [ regex "(\\d{2,4})\\s?[-\\.]\\s?(0?[1-9]|1[0-2])\\s?[-\\.]\\s?(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleMMDD :: Rule
ruleMMDD = Rule
  { name = "mm.dd"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])\\s?[-\\.]\\s?(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(reggel(i(t)?)?|délelőtt(i(t)?)?|délben|déli(t)?|délután(i(t)?)?|est(e|i(t)?)|éjszaka(i(t)?)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case Text.toLower match of
              "reggel"               -> (hour False 6, hour False 10)
              "reggeli"              -> (hour False 6, hour False 10)
              "reggelit"             -> (hour False 6, hour False 10)
              "délelőtt"   -> (hour False 08, hour False 12)
              "délelőtti"  -> (hour False 08, hour False 12)
              "délelőttit" -> (hour False 08, hour False 12)
              "délben"          -> (hour False 12, hour False 13)
              "déli"            -> (hour False 12, hour False 13)
              "délit"           -> (hour False 12, hour False 13)
              "délután"    -> (hour False 12, hour False 18)
              "délutáni"   -> (hour False 12, hour False 18)
              "délutánit"  -> (hour False 12, hour False 18)
              "este"                 -> (hour False 16, hour False 20)
              "esti"                 -> (hour False 16, hour False 20)
              "estit"                -> (hour False 16, hour False 20)
              _                      -> (hour False 20, hour False 23)
        td <- interval TTime.Open start end
        tt . partOfDay $ td
      _ -> Nothing
  }

-- Since part of days are latent, general time intersection is blocked
ruleTimePOD :: Rule
ruleTimePOD = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Time pod:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

ruleSeasons :: Rule
ruleSeasons = Rule
  { name = "seasons"
  , pattern =
    [ regex "(nyár(i(t)?|on)?|ősz(i(t)?)?|ősszel|tél(i(t)?|en)?|tavasz(i(t)?)?|tavasszal)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        start <- case Text.toLower match of
          "nyár"      -> Just $ monthDay 6 21
          "nyári"     -> Just $ monthDay 6 21
          "nyárit"    -> Just $ monthDay 6 21
          "nyáron"    -> Just $ monthDay 6 21
          "ősz"       -> Just $ monthDay 9 23
          "őszi"      -> Just $ monthDay 9 23
          "őszit"     -> Just $ monthDay 9 23
          "ősszel"    -> Just $ monthDay 9 23
          "tél"       -> Just $ monthDay 12 21
          "téli"      -> Just $ monthDay 12 21
          "télit"     -> Just $ monthDay 12 21
          "télen"     -> Just $ monthDay 12 21
          "tavasz"         -> Just $ monthDay 3 20
          "tavaszi"        -> Just $ monthDay 3 20
          "tavaszit"       -> Just $ monthDay 3 20
          "tavasszal"      -> Just $ monthDay 3 20
          _ -> Nothing
        end <- case Text.toLower match of
          "nyár"      -> Just $ monthDay 9 23
          "nyári"     -> Just $ monthDay 9 23
          "nyárit"    -> Just $ monthDay 9 23
          "nyáron"    -> Just $ monthDay 9 23
          "ősz"       -> Just $ monthDay 12 21
          "őszi"      -> Just $ monthDay 12 21
          "őszit"     -> Just $ monthDay 12 21
          "ősszel"    -> Just $ monthDay 12 21
          "tél"       -> Just $ monthDay 3 20
          "téli"      -> Just $ monthDay 3 20
          "télit"     -> Just $ monthDay 3 20
          "télen"     -> Just $ monthDay 3 20
          "tavasz"         -> Just $ monthDay 6 21
          "tavaszi"        -> Just $ monthDay 6 21
          "tavaszit"       -> Just $ monthDay 6 21
          "tavasszal"      -> Just $ monthDay 6 21
          _ -> Nothing
        Token Time <$> interval TTime.Open start end
      _ -> Nothing

  }

rules :: [Rule]
rules =
  [ ruleIntersect
  , ruleMonthDOMNumeral
  , ruleMonthDOMNumeralSuffix
  , ruleMonthDOMOrdinal
  , ruleCycleThisLastNext
  , ruleNextDOW
  , ruleHHMM
  , ruleHHMMSS
  , ruleTODLatent
  , ruleTODAM
  , ruleTODPM
  , ruleYYYYMMDD
  , ruleMMDD
  , rulePartOfDays
  , ruleTimePOD
  , ruleSeasons
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
