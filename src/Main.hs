module Main where

import qualified Data.ByteString.Lazy as B
import Text.Pandoc
import Text.Pandoc.Walk
import System.Environment
import Text.CSL.Pandoc (processCitesEndNote')
import qualified Text.Pandoc.UTF8 as UTF8
import System.IO

readDoc :: String -> Pandoc
readDoc s = case readLaTeX def s of
    Right doc -> doc
    Left err  -> error (show err)

writeDoc :: Pandoc -> IO B.ByteString
writeDoc doc = do
    doc' <- processCitesEndNote' doc
    let warnings = query findWarnings doc'
    mapM_ (UTF8.hPutStrLn stderr) warnings
    writeDocx def doc'

findWarnings :: Inline -> [String]
findWarnings (Span (_,["citeproc-not-found"],[("data-reference-id",ref)]) _) =
      ["pandoc-citeproc: reference " ++ ref ++ " not found" | ref /= "*"]
findWarnings (Span (_,["citeproc-no-output"],_) _) =
      ["pandoc-citeproc: reference with no printed form"]
findWarnings _ = []

main :: IO ()
main = do
    [input, output] <- getArgs
    txt <- readFile input
    doc <- writeDoc $ readDoc txt
    B.writeFile output doc
