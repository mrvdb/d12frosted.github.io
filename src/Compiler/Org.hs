{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.Org (orgCompiler) where

import           BasicPrelude
import           Compiler.Pandoc
import           Data.Map.Lazy (mapWithKey)
import           Hakyll
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Shared
import           Text.Pandoc.Walk
import qualified Text.Parsec as P
import           Text.Parsec.String (Parser)
import           Text.Read (readEither)

orgCompiler :: Compiler (Item String)
orgCompiler = pandocMetadataCompilerWith transform
  where transform = tDateMeta . tTableOfContents . tHeaderIds

--------------------------------------------------------------------------------
-- Meta transformer
--------------------------------------------------------------------------------

tDateMeta :: Pandoc -> Pandoc
tDateMeta (Pandoc meta body) = Pandoc (walk modMetaDate meta) body
  where modMetaDate = Meta . mapWithKey convertDate . unMeta
        convertDate k r@(MetaInlines [(Str v)]) =
          if k == "date"
            then MetaInlines . toList . str . strip $ v
            else r
        convertDate _ v = v
        strip = stripLeft "<" . stripRight ">"

stripLeft :: String -> String -> String
stripLeft a b = if a `isPrefixOf` b then drop (length a) b else b

stripRight :: String -> String -> String
stripRight a b = if a `isSuffixOf` b then take (length b - length a) b else b

--------------------------------------------------------------------------------
-- Table of Contents transformer
--------------------------------------------------------------------------------

-- | replace all toc marks by table of contents
tTableOfContents :: Pandoc -> Pandoc
tTableOfContents p@(Pandoc m bs) = Pandoc m . concat . map modToc $ bs
  where modToc :: Block -> [Block]
        modToc b@(RawBlock _ s) = maybe [b] (generateToc p) . parseTOCConfig $ s
        modToc b = [b]

generateToc :: Pandoc -> TOC -> [Block]
generateToc p (TOCHeadLines level) = [tocHeader, tableOfContents level hs]
  where hs = filter (isMaxLevel level) $ getHeaders p
        isMaxLevel m (Header l _ _) = m >= l
        isMaxLevel _ _              = False
        tocHeader = Header 1 ("", [], []) . toList . str $ "Table of Contents"

tableOfContents :: Int -> [Block] -> Block
tableOfContents level headers = BulletList $ map (elementToListItem level) $ hierarchicalize headers

elementToListItem :: Int -> Element -> [Block]
elementToListItem level (Sec lev nums _ headerText subsecs)
  = Plain [Link headerText ("#" ++ headerId nums, "")] :
    [ BulletList (map (elementToListItem level) subsecs) |
      not (null subsecs) && lev <= level ]
elementToListItem _ (Blk _) = []

--------------------------------------------------------------------------------
-- Table of Contents config
--------------------------------------------------------------------------------

data TOC = TOCHeadLines Int deriving Show

parseTOCConfig :: String -> Maybe TOC
parseTOCConfig = either (const Nothing) Just . P.parse tocParser ""

tocParser :: Parser TOC
tocParser =
  do P.spaces >>
       P.string "#+TOC:" >>
       P.spaces >>
       P.string "headlines" >>
       P.spaces >>
       P.many1 P.digit >>=
       return . readEither >>=
       \case
         Right level -> return $ TOCHeadLines level
         Left e      -> P.parserFail e

--------------------------------------------------------------------------------
-- Headers
--------------------------------------------------------------------------------

-- | add unique id to all headers
tHeaderIds :: Pandoc -> Pandoc
tHeaderIds (Pandoc m bs) = Pandoc m . convert . hierarchicalize $ bs
  where mark :: Element -> [Block]
        mark (Blk b) = [b]
        mark (Sec lvl nums (_, cs, kvps) lbl ctns) =
          (Header lvl (headerId nums, cs, kvps) lbl) : convert ctns
        convert = concat . map mark

headerId :: [Int] -> String
headerId = ("sec" ++) . foldMap (("-" ++) . textToString . show) . reverse

getHeaders :: Pandoc -> [Block]
getHeaders = query selectHeader
  where selectHeader h@(Header _ _ _) = [h]
        selectHeader _ = []
