{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bot.Twitter as Twitter
import Web.Tweet

configFilePath :: FilePath
configFilePath = "/Users/acidjunk/.cred.toml"

main :: IO ()
main = do
  let keywords = Twitter.SearchTerm "music%20jazz"
  (tweets, results) <- Twitter.runTwitter configFilePath $ do
    userTweets <- Twitter.searchTweetsForUser (Twitter.User "acidjunk")
    searchResults <- Twitter.searchForKeywords keywords
    pure (userTweets, searchResults)

  let parsedTweets = map (\t -> _screenName t <> ":" <> _text t) tweets
  print parsedTweets

