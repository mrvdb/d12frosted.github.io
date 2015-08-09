{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pandoc (pandocCompilerWithMetadata) where

import BasicPrelude
import Data.Map.Lazy (foldMapWithKey)
import Hakyll
import Text.Pandoc (Pandoc(..), lookupMeta, unMeta, MetaValue(..))
import Text.Pandoc.Builder (doc, plain, str)
import Text.Pandoc.Shared (stringify)

pandocCompilerWithMetadata :: Compiler (Item String)
pandocCompilerWithMetadata =
  do itemPandoc <- readPandocWith ropt =<< getResourceString
     reloadMetadata . writePandocWith wopt . fmap transformPandoc $ itemPandoc
     return $ writePandocWith wopt itemPandoc
  where ropt = defaultHakyllReaderOptions
        wopt = defaultHakyllWriterOptions

transformPandoc :: Pandoc -> Pandoc
transformPandoc p = meta <> p
  where getMetaMap (Pandoc m _ ) = unMeta m
        meta = transformMeta . getMetaMap $ p

transformMeta :: Map String MetaValue -> Pandoc
transformMeta = doc . wrapInBlock . foldMapWithKey (\k v -> toMeta k . maybeConvert k . stringify $ v)
  where blockSep = str "---\n"
        toMeta k v = str k <> str ": " <> str v <> str "\n"
        wrapInBlock inl = plain $ blockSep <> inl <> blockSep
        maybeConvert k v =
          if k == "date"
             then convert v
             else v
        convert = stripLeft "<" . stripRight ">"

stripLeft :: String -> String -> String
stripLeft a b = if a `isPrefixOf` b then drop (length a) b else b

stripRight :: String -> String -> String
stripRight a b = if a `isSuffixOf` b then take (length b - length a) b else b
