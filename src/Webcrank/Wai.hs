{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Webcrank.Wai
  ( Wai
  , MonadWai(..)
  , dispatch
  , getRequest
  , getRequestHeader
  , module Webcrank
  , module Webcrank.Dispatch
  , module Network.Wai
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Network.Wai
import System.PosixCompat.Time

import Webcrank
import Webcrank.Dispatch hiding (dispatch)
import qualified Webcrank.Dispatch as W
import Webcrank.ServerAPI (ServerAPI(..))
import qualified Webcrank.ServerAPI as API

type WaiState = (Request, HTTPDate)

newtype Wai a =
  Wai { unWai :: ReaderT WaiState IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader WaiState
      , MonadThrow
      , MonadCatch
      )

class (Functor m, Applicative m, Monad m, MonadCatch m) => MonadWai m where
  liftWai :: Wai a -> m a

instance MonadWai Wai where
  liftWai = id

instance MonadWai m => MonadWai (StateT s m) where
  liftWai = lift . liftWai

instance MonadWai m => MonadWai (ReaderT s m) where
  liftWai = lift . liftWai

-- TODO
--   allow configuration of error handler and use it or sending the 404
dispatch
  :: MonadWai m
  => (forall a. m a -> Wai a)
  -> Dispatcher (Resource m)
  -> Application
dispatch f rt rq = maybe sendNotFound run $ W.dispatch rt (pathInfo rq) where
  run r = runWai f r rq
  sendNotFound respond = respond $ responseLBS notFound404 [] "404 Not Found"

getRequest :: MonadWai m => m Request
getRequest = liftWai $ asks fst

getRequestHeader :: MonadWai m => HeaderName -> m (Maybe ByteString)
getRequestHeader h = (snd <$>) . find ((h ==) . fst) . requestHeaders <$> getRequest

runWai
  :: MonadWai m
  => (forall a. m a -> Wai a)
  -> Resource m
  -> Application
runWai f r rq respond = do
  now <- fmap epochTimeToHTTPDate epochTime
  let w = f $ handleRequest r
  resp <- runReaderT (unWai w) (rq, now)
  respond resp

handleRequest
  :: MonadWai m
  => Resource m
  -> m Response
handleRequest r = send (API.handleRequest api r) where
  send go = resp <$> go
  resp (s, hs, b) = responseLBS s (hdrs hs) (fromMaybe LBS.empty b)
  hdrs = join . fmap sequenceA . Map.toList

api :: MonadWai m => ServerAPI m
api = ServerAPI
  { srvGetRequestMethod = requestMethod <$> getRequest
  , srvGetRequestHeader = getRequestHeader
  , srvGetRequestURI = undefined
  , srvGetRequestTime = liftWai $ asks snd
  }

