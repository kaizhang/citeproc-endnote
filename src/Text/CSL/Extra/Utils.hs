{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.CSL.Extra.Utils  where

import           Control.Monad.Reader
import           Text.CSL.Output.Plain
import           Text.CSL.Reference    hiding (Value, processCites)
import           Text.CSL.Style        hiding (Citation (..), Cite (..))
import           Text.Printf           (printf)

infixl 5 <+>
(<+>) :: Reader Reference String -> Reader Reference String -> Reader Reference String
(<+>) x y = do
    x' <- x
    y' <- y
    return $ x' ++ y'

tag :: String -> String -> String
tag name x = printf "&lt;%s&gt;%s&lt;/%s&gt;" name x name

tagM :: String -> Reader Reference (String -> String)
tagM name = return $ \x -> printf "&lt;%s&gt;%s&lt;/%s&gt;" name x name

tag' :: String -> [String] -> String -> String
tag' name p x = printf "&lt;%s %s&gt;%s&lt;/%s&gt;" name (unwords p) x name

title_ :: Reader Reference String
title_ = reader $ tag "title" . renderPlain . title

author_ :: Reader Reference String
author_ = reader $ tag "Author" . renderPlain . familyName . head . author

authors_ :: Reader Reference String
authors_ = reader $ tag "authors" . concatMap (tag "author") . getAuthors

volume_ :: Reader Reference String
volume_ = reader $ tag "volume" . renderPlain . volume

isbn_ :: Reader Reference String
isbn_ = reader $ tag "isbn" . unLiteral . isbn

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
