{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.CSL.Extra.Utils  where

import Text.XML.Light
import           Text.CSL.Output.Plain
import           Text.CSL.Reference    hiding (Value, processCites)
import           Text.CSL.Style        hiding (Element)
import Text.Printf (printf)

refsToXml :: [Reference] -> Element
refsToXml = unode "EndNote" . map refToXml

refToXml :: Reference -> Element
refToXml ref = case refType ref of
    ArticleJournal -> unode "Cite" [recnum_, author_, year_, record_]
    x -> error $ printf "Reference with type \"%s\" has NOT been implemented." (show x)
  where
    author_ = unode "Author" $ renderPlain $ familyName $ head $ author ref
    year_ = unode "Year" $ getYear ref
    recnum_ = unode "RecNum" $ getId ref
    rec_number_ = unode "rec-number" $ getId ref
    record_ = unode "record"
        [ rec_number_, foreign_keys_, ref_type_, contributors_, titles_, pages_
        , volume_, number_, dates_, isbn_, electronic_resource_num_ ]
    foreign_keys_ = unode "foreign-keys" $ unode "key"
        ( [ Attr (unqual "app") "\"EN\""
          , Attr (unqual "db-id") $ show (getId ref) ], getId ref )
    ref_type_ = unode "ref-type" (Attr (unqual "name") "Journal Article", "17")
    contributors_ = unode "contributors" $ unode "authors" $
        map (unode "author") $ getAuthors ref
    titles_ = unode "titles" [ unode "title" $ renderPlain $ title ref
                             , unode "secondary-title" $ getContainerTitle ref ]
    pages_ = unode "pages" $ renderPlain $ page ref
    volume_ = unode "volume" $ renderPlain $ volume ref
    number_ = unode "number" $ renderPlain $ issue ref
    dates_ = unode "dates" [ unode "year" $ getYear ref
                           , unode "pub_dates" $ unode "date" $ getDate ref ]
    isbn_ = unode "isbn" $ unLiteral $ isbn ref
    electronic_resource_num_ = unode "electronic-resource-num" $ unLiteral $ doi ref

getId :: Reference -> String
getId = unLiteral . refId

getContainerTitle :: Reference -> String
getContainerTitle = renderPlain . containerTitle

-- | Display author names as "Full, X. X."
getAuthor :: Agent -> String
getAuthor x = renderPlain (familyName x) ++ ", " ++ unwords
        (map ((:".") . head . renderPlain) $ givenName x)

getAuthors :: Reference -> [String]
getAuthors = map getAuthor . author

getYear :: Reference -> String
getYear = unLiteral . year . head . issued

getDate :: Reference -> String
getDate ref = getMonth ref ++ " " ++ getDay ref

getMonth :: Reference -> String
getMonth = unLiteral . month . head . issued

getDay :: Reference -> String
getDay = unLiteral . day . head . issued
