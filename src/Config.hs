{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config where

import BasicPrelude

-- Index page

indexPageDefaultTitle :: String
indexPageDefaultTitle = "The monkey among us"

indexPagePosts :: Int
indexPagePosts = 2

-- Archive page

archivePageTitle :: String
archivePageTitle = "Archives"
