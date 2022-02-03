{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bot.Twitter as Twitter
import qualified Data.Text as T
import Web.Tweet

configFilePath :: FilePath
configFilePath = "/Users/acidjunk/.cred.toml"

twitterUser :: T.Text 
twitterUser = "acidjunk"

stringFromChar :: Char -> String
stringFromChar x = [x]

main :: IO ()
main = do
  -- Execute a search and display the account + bio
  let keywords = Twitter.SearchTerm "music%20jazz"
  searchResults <- Twitter.runTwitter configFilePath $ do
    searchResults <- Twitter.searchForKeywords keywords
    pure searchResults

  print searchResults

  -- FETCH ALL USERTWEETS and dump them in the console
  -- userTweets <- Twitter.runTwitter configFilePath $ do
  --   userTweets <- Twitter.searchTweetsForUser (Twitter.User twitterUser)
  --   pure userTweets

  -- let parsedTweets = map (\t -> _screenName t <> ":" <> _text t) userTweets
  -- mapM_ putStrLn parsedTweets
