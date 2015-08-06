{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Hakyll
import BasicPrelude
import Config
import Text.Pandoc
import Text.Pandoc.Shared (stringify)

main :: IO ()
main =
  hakyll $
  do imagesR
     cssR
     staticPagesR
     postsR
     archiveR
     indexR
     templatesR

---------------- Rules

imagesR :: Rules ()
imagesR =
  match "images/*" $
  do route idRoute
     compile copyFileCompiler

cssR :: Rules ()
cssR =
  match "css/*" $
  do route idRoute
     compile compressCssCompiler

staticPagesR :: Rules ()
staticPagesR =
  match (fromList ["about.rst", "contact.markdown"]) $
  do route $ setExtension "html"
     compile $
       pandocCompiler >>=
       loadAndApplyTemplate "templates/default.html" defaultContext >>=
       relativizeUrls

postsR :: Rules ()
postsR =
  match "posts/*" $
  do route $ setExtension "html"
     compile $
       pandocCompiler >>=
       loadAndApplyTemplate "templates/post.html" postCtx >>=
       loadAndApplyTemplate "templates/default.html" postCtx >>=
       relativizeUrls

archiveR :: Rules ()
archiveR =
  create ["archive.html"] $
  do route idRoute
     compile $
       do posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx' = archiveCtx posts
          makeItem "" >>=
            loadAndApplyTemplate "templates/archive.html" archiveCtx' >>=
            loadAndApplyTemplate "templates/default.html" archiveCtx' >>=
            relativizeUrls

indexR :: Rules ()
indexR =
  match "index.org" $
  do route (setExtension "html")
     compile $
       do posts <- recentFirst =<< loadAll "posts/*"
          title <- getOrgMetaField' "title" =<< getResourceBody
          let indexCtx' = indexCtx title posts
          pandocCompiler >>=
            applyAsTemplate indexCtx' >>=
            loadAndApplyTemplate "templates/default.html" indexCtx' >>=
            relativizeUrls

templatesR :: Rules ()
templatesR =
  match "templates/*" $
  compile templateCompiler

---------------- Contexts

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  defaultContext

archiveCtx :: [Item String] -> Context String
archiveCtx posts =
  listField "posts" postCtx (return posts) <>
  constField "title" archivePageTitle <>
  defaultContext

indexCtx :: Maybe String -> [Item String] -> Context String
indexCtx title posts =
  listField "posts" postCtx (return $ take indexPagePosts posts) <>
  constField "title" (fromMaybe indexPageDefaultTitle title) <>
  defaultContext

---------------- Helper functions

dropFirstDir :: FilePath -> FilePath
dropFirstDir = dropFirstDir' . splitBy '/'
  where dropFirstDir' :: [FilePath] -> FilePath
        dropFirstDir' [] = []
        dropFirstDir' [x] = x
        dropFirstDir' (_:xs) = intercalate "/" xs

splitBy :: (Eq a, Foldable f) => a -> f a -> [[a]]
splitBy delimiter = foldr f [[]]
  where f c l@(x:xs)
          | c == delimiter = [] : l
          | otherwise = (c : x) : xs

getOrgMetaField' :: MonadMetadata m => String -> Item String -> m (Maybe String)
getOrgMetaField' key = return . getOrgMetaField key . itemBody

getOrgMetaField :: String -> String -> Maybe String
getOrgMetaField key body = fromRight content >>= getPandocMetaField key
  where content = readOrg def body

getPandocMetaField :: String -> Pandoc -> Maybe String
getPandocMetaField key = fmap stringify . lookupMeta key . getMeta
  where getMeta (Pandoc meta _) = meta

fromRight :: Either a b -> Maybe b
fromRight (Left _) = Nothing
fromRight (Right v) = Just v
