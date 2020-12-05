{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Day04regex where

import Lib ( count )
import Control.Lens.Regex.Text ( match, regex )
import Control.Lens ( (^..) )
import qualified Data.Text as T

-- Solutions with regex are supposed to be unreadable anyway, right?
main :: IO ()
main = do
  input <- T.splitOn "\n\n" . T.pack <$> readFile "../data/day04.in"
  let passports r = count ((==7) . length) $ map r input
      r1 s = s ^.. [regex|byr:|iyr:|eyr:|hgt:|hcl:|ecl:|pid:|] . match
      r2 s = s ^.. [regex|(byr:(19[2-9][0-9]|200[0-2]))|(iyr:(201[0-9]|2020))|(eyr:(202[0-9]|2030))|(hgt:(1[5-8][0-9]|19[0-3])cm)|(hgt:(5[8-9]|6[0-9]|7[0-6])in)|(hcl:#[0-9a-f]{6})|(ecl:(amb|blu|brn|gry|grn|hzl|oth))|(pid:[0-9]{9}(\n| |$))|] . match
  print $ passports r1
  print $ passports r2

-- 219
-- 127
