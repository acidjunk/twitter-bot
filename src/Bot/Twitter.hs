{-# LANGUAGE OverloadedStrings, DeriveFunctor, DerivingVia #-}

module Bot.Twitter
  ( User(..)
  , SearchTerm(..)
  , TwitterM
  , runTwitter
  , searchForKeywords
  , searchTweetsForUser
  ) where

import GHC.Generics
import Data.Maybe (fromJust)
import Data.Vector
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson ((.:))
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


newtype SearchTerm = SearchTerm T.Text
  deriving (Eq, Show)

newtype User = User T.Text
  deriving (Eq, Show)



newtype SearchResults
  = SearchResults (Vector SearchResult)
  deriving Show

data SearchResult
  = SearchResult
  { _tweetText :: T.Text
  , _userName :: User
  , _description :: T.Text
  } deriving Show

instance A.FromJSON SearchResults where
  parseJSON = A.withArray "statuses" (fmap SearchResults . traverse A.parseJSON)

instance A.FromJSON SearchResult where
  parseJSON (A.Object value) = do
    tweetText <- value .: "text"
    userInfo <- value .: "user"
    screenName <- userInfo .: "screen_name"
    description <- userInfo .: "description"
    pure $ SearchResult tweetText (User screenName) description
  parseJSON _ = mzero

searchForKeywords :: SearchTerm -> TwitterM SearchResults
searchForKeywords (SearchTerm keywords) = do
  credentialsPath <- ask
  let query = "?q=" <> T.unpack keywords
  results <- liftIO $ searchRaw query credentialsPath
  -- TODO: use Either iso fromJust?
  pure $ fromJust $ A.decode results

searchTweetsForUser :: User -> TwitterM [TweetEntity]
searchTweetsForUser (User userName) = do
  credentialsPath <- ask
  liftIO $ getAll (T.unpack userName) Nothing credentialsPath

