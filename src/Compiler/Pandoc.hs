{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.Pandoc (pandocMetadataCompiler
                       ,pandocMetaCompiler) where

import BasicPrelude
import Data.Map.Lazy (foldMapWithKey)
import Hakyll
import Text.Pandoc (Pandoc(..), unMeta, MetaValue(..))
-- import Text.Pandoc.Builder (doc, plain, str)
import Text.Pandoc.Shared (stringify)

-- pandocMetadataCompilerWith :: (Pandoc -> Pandoc) -> Compiler (Item String)
-- pandocMetadataCompilerWith t =
--   do itemPandoc <- return . fmap t =<< readPandocWith ropt =<< getResourceString
--      _ <- reloadMetadata . writePandocWith wopt . fmap transformPandoc $ itemPandoc
--      return . writePandocWith wopt $ itemPandoc
--   where ropt = defaultHakyllReaderOptions
--         wopt = defaultHakyllWriterOptions

-- pandocMetadataCompilerWith' :: ReaderOptions
--                             -> WriterOptions
--                             -> (Pandoc -> Pandoc)
--                             -> Compiler (Item String)
-- pandocMetadataCompilerWith' ropt wopt t =
--   do itemPandoc <- return . fmap t =<< readPandocWith ropt =<< getResourceString
--      fp <- getResourceFilePath
--      -- writeFile (fp ++ ".metadata") undefined
--      return . writePandocWith wopt $ itemPandoc

-- transformPandoc :: Pandoc -> Pandoc
-- transformPandoc p = meta <> p
--   where getMetaMap (Pandoc m _ ) = unMeta m
--         meta = transformMeta . getMetaMap $ p

-- transformMeta :: Map String MetaValue -> Pandoc
-- transformMeta = doc . wrapInBlock . foldMapWithKey (\k -> toMeta k . stringify)
--   where blockSep = str "---\n"
--         toMeta k v = str k <> str ": " <> str v <> str "\n"
--         wrapInBlock inl = plain $ blockSep <> inl <> blockSep

-- reloadMetadata = undefined

pandocMetadataCompiler :: (Pandoc -> Pandoc) -> Compiler (Item String)
pandocMetadataCompiler t =
  do itemPandoc <- return . fmap t =<< readPandocWith ropt =<< getResourceString
     fp <- getResourceFilePath
     unsafeCompiler . putStrLn $ "  ---------------- processing file at " ++ fromString fp
     unsafeCompiler $ writeFile (fp ++ ".metadata") (transformMeta . getMetaMap . itemBody $ itemPandoc)
     debugCompiler $ "  ==== " ++ fp
     _ <- load $ fromFilePath (fp ++ ".metadata") :: Compiler (Item String)
     reloadMetadata
     return . writePandocWith wopt $ itemPandoc
  where ropt = defaultHakyllReaderOptions
        wopt = defaultHakyllWriterOptions
        getMetaMap (Pandoc m _) = unMeta m

transformMeta :: IsString a => Map String MetaValue -> a
transformMeta = fromString . foldMapWithKey (\k -> toMeta k . stringify)
  where toMeta k v = k <> ": " <> v <> "\n"

pandocMetaCompiler :: (Pandoc -> Pandoc) -> Compiler (Item String)
pandocMetaCompiler t =
  do itemPandoc <- return . fmap t =<< readPandocWith ropt =<< getResourceString
     fp <- getResourceFilePath
     unsafeCompiler . putStrLn $ "  ---------------- processing file at " ++ fromString fp
     unsafeCompiler $ writeFile (fp ++ ".metadata") (transformMeta . getMetaMap . itemBody $ itemPandoc)
     debugCompiler $ "  ==== " ++ fp
     -- _ <- load $ fromFilePath (fp ++ ".metadata") :: Compiler (Item String)
     -- reloadMetadata
     return . writePandocWith wopt $ itemPandoc
  where ropt = defaultHakyllReaderOptions
        wopt = defaultHakyllWriterOptions
        getMetaMap (Pandoc m _) = unMeta m
