{-# LANGUAGE DeriveFunctor, DerivingVia #-}

module Bot.Twitter
  ( Author(..)
  , Keyword(..)
  , TwitterM
  , runTwitter
  , searchForKeywords
  , getLastTweets
  ) where

import qualified Data.Text as T
import Control.Monad.Reader
import Web.Tweet
import Web.Tweet.API
import Web.Tweet.Utils

newtype TwitterM a
  = TwitterM (ReaderT FilePath IO a)
  deriving (Functor)
  deriving (Applicative, Monad, MonadReader FilePath, MonadIO)
  via ReaderT FilePath IO


runTwitter :: FilePath -> TwitterM a -> IO a
runTwitter credentialsPath (TwitterM action) =
  runReaderT action credentialsPath

newtype Keyword = Keyword T.Text
  deriving (Eq, Show)

newtype Author = Author T.Text
  deriving (Eq, Show)

searchForKeywords :: [Keyword] -> TwitterM [Author]
searchForKeywords keywords = do
  credentialsPath <- ask
  let userName = "acidjunk"
      tweetCount = 42
  tweets <- liftIO $ getAll userName (Just tweetCount) credentialsPath
  pure $ map (\t -> Author $ T.pack $ _screenName t) tweets

getLastTweets :: Author -> Int -> TwitterM [Tweet]
getLastTweets author count = undefined

-- TODO: function for searching if user has certain keywords in last X tweets (in other module?)
