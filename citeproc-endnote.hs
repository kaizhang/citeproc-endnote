import Text.CSL.Extra.EndNote (processCitesEndNote')
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter processCitesEndNote'
