module Main where

import Text.CSL.Extra.EndNote (processCitesEndNote')
import Text.Pandoc.JSON (toJSONFilter)

main :: IO ()
main = toJSONFilter processCitesEndNote'
