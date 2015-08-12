{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import BasicPrelude
import Config
import Hakyll
import Compiler.Org

main :: IO ()
main =
  hakyll $
  do imagesR
     cssR
     fontsR
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

fontsR :: Rules ()
fontsR =
  match "fonts/*" $
  do route idRoute
     compile copyFileCompiler

staticPagesR :: Rules ()
staticPagesR =
  match (fromList ["about.rst", "contact.markdown"]) $
  do route $ setExtension "html"
     compile $
       pandocCompiler >>=
       defaultTemplate defaultContext >>=
       relativizeUrls

postsR :: Rules ()
postsR =
  match "posts/*" $
  do route $ setExtension "html"
     compile $
       orgCompiler >>=
         loadAndApplyTemplate "templates/post.html" postCtx >>=
         defaultTemplate postCtx >>=
         relativizeUrls

archiveR :: Rules ()
archiveR =
  create ["archive.html"] $
  do route idRoute
     compile $
       do posts <- loadPosts
          let archiveCtx' = archiveCtx posts
          makeItem "" >>=
            loadAndApplyTemplate "templates/archive.html" archiveCtx' >>=
            defaultTemplate archiveCtx' >>=
            relativizeUrls

indexR :: Rules ()
indexR =
  match "index.org" $
  do route (setExtension "html")
     compile $
       do posts <- loadPosts
          let indexCtx' = indexCtx posts
          orgCompiler >>=
            applyAsTemplate indexCtx' >>=
            defaultTemplate indexCtx' >>=
            relativizeUrls

templatesR :: Rules ()
templatesR =
  match "templates/*" $
  compile templateCompiler

---------------- Templates

defaultTemplate :: Context String -> Item String -> Compiler (Item String)
defaultTemplate = loadAndApplyTemplate "templates/default.html" . (titleCtx <>)
  where titleCtx = constField "blogTitle" blogTitle

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

indexCtx :: [Item String] -> Context String
indexCtx posts =
  listField "posts" postCtx (return $ take indexPagePosts posts) <>
  defaultContext

loadPosts :: Compiler ([Item String])
loadPosts = loadAll "posts/*" >>= recentFirst
