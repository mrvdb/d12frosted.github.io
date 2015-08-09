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
transformMeta = doc . wrapInBlock . foldMapWithKey (\k v -> toMeta k $ stringify v)
  where blockSep = str "---\n"
        toMeta k v = str k <> str ": " <> str v <> str "\n"
        wrapInBlock inl = plain $ blockSep <> inl <> blockSep
