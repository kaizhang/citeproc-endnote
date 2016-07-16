{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.CSL.Extra.EndNote (processCitesEndNote') where

import qualified Data.Map                 as M
import           Data.Maybe               (mapMaybe)
import           Text.CSL.Input.Bibutils  (convertRefs, readBiblioFile)
import           Text.CSL.Reference       hiding (Value, processCites)
import           Text.HTML.TagSoup.Entity (lookupEntity, escapeXML)
import           Text.Pandoc
import           Text.Pandoc.Shared       (stringify)

import           System.Directory         (doesFileExist)
import           System.FilePath
import Text.XML.Light

import           Text.CSL.Extra.Utils

fldBegin :: String
fldBegin = "<w:r><w:fldChar w:fldCharType=\"begin\"/></w:r>"

fldEnd :: String
fldEnd = "<w:r><w:fldChar w:fldCharType=\"separate\"/></w:r>" ++
    "<w:r><w:rPr><w:noProof/></w:rPr><w:t>[+]</w:t></w:r>" ++
    "<w:r><w:fldChar w:fldCharType=\"end\"/></w:r>"

-- EndNote traveling reference format:
-- ADDIN IN.CITE
mkEndNoteRef :: [Reference] -> Inline
mkEndNoteRef refs = RawInline (Format "openxml") $ fldBegin ++ endnoteRefs ++ fldEnd
  where
    endnoteRefs = "<w:r><w:instrText xml:space=\"preserve\"> ADDIN EN.CITE " ++
        refs' ++ "</w:instrText></w:r>"

    refs' = escapeXML $ showElement $ refsToXml refs

processCitesEndNote :: [Reference] -> Pandoc -> Pandoc
processCitesEndNote refs doc = topDown (procC refs_map) doc
  where
    refs_map = M.fromList $ map (\r -> (unLiteral $ refId r, r)) refs
    procC refMap (Cite ts _) = mkEndNoteRef $
        mapMaybe (\t -> M.lookup (citationId t) refMap) ts
    procC _ x = x

processCitesEndNote' :: Pandoc -> IO Pandoc
processCitesEndNote' (Pandoc meta blocks) = do
  let inlineRefError s = error $ "Error parsing references: " ++ s
  let inlineRefs = either inlineRefError id
                   $ convertRefs $ lookupMeta "references" meta
  bibRefs <- getBibRefs $ maybe (MetaList []) id
                        $ lookupMeta "bibliography" meta
  let refs = inlineRefs ++ bibRefs
  return $ processCitesEndNote refs $ Pandoc meta blocks

getBibRefs :: MetaValue -> IO [Reference]
getBibRefs (MetaList xs) = concat `fmap` mapM getBibRefs xs
getBibRefs (MetaInlines xs) = getBibRefs (MetaString $ stringify xs)
getBibRefs (MetaString s) = do
  path <- findFile ["."] s >>= maybe (error $ "Could not find " ++ s) return
  map unescapeRefId `fmap` readBiblioFile path
getBibRefs _ = return []

-- unescape reference ids, which may contain XML entities, so
-- that we can do lookups with regular string equality
unescapeRefId :: Reference -> Reference
unescapeRefId ref = ref{ refId = Literal $ decodeEntities (unLiteral $ refId ref) }

findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile [] _ = return Nothing
findFile (p:ps) f = do
  exists <- doesFileExist (p </> f)
  if exists
     then return $ Just (p </> f)
     else findFile ps f

decodeEntities :: String -> String
decodeEntities [] = []
decodeEntities ('&':xs) =
  let (ys,zs) = break (==';') xs
  in  case zs of
           ';':ws -> case lookupEntity ('&':ys ++ ";") of
                                       Just s  -> s ++ decodeEntities ws
                                       Nothing -> '&' : decodeEntities xs
           _      -> '&' : decodeEntities xs
decodeEntities (x:xs) = x : decodeEntities xs
