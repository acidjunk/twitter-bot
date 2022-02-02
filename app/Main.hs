{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bot.Twitter as Twitter

main :: IO ()
main = do
  let keywords = [Twitter.Keyword "music", Twitter.Keyword "jazz"]
  authors <- Twitter.runTwitter ".cred.toml" $ Twitter.searchForKeywords keywords
  print authors

