{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Webcrank.Wai
  ( WaiResource
  , WaiCrankT
  , WaiData
  , ReqData
  , dispatch
  , getRequest
  , getRequestHeader
  , module Webcrank
  , module Webcrank.Dispatch
  , module Network.Wai
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Control.Monad.RWS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (find)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Traversable
import Network.Wai
import System.PosixCompat.Time

import Webcrank
import Webcrank.Dispatch hiding (dispatch)
import qualified Webcrank.Dispatch as W
import Webcrank.ServerAPI hiding (handleRequest)
import qualified Webcrank.ServerAPI as API

data WaiData m = WaiData
  { _waiDataResourceData :: ResourceData m
  , _waiDataRequest :: Request
  , _waiDataRequestDate :: HTTPDate
  }

makeClassy ''WaiData

instance HasResourceData (WaiData m) m where
  resourceData = waiDataResourceData

newtype WaiCrankT m a =
  WaiCrankT { unWaiCrankT :: RWST (WaiData (WaiCrankT m)) LogData ReqData m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (WaiData (WaiCrankT m))
    , MonadState ReqData
    , MonadWriter LogData
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadTrans WaiCrankT where
  lift = WaiCrankT . lift

type WaiResource m = Resource (WaiCrankT m)
type WaiServerAPI m = ServerAPI (WaiCrankT m)

dispatch
  :: (Applicative m, MonadIO m, MonadCatch m)
  => (forall a. m a -> IO a)
  -> Dispatcher (WaiResource m)
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
dispatch f d rq = f . dispatch' d rq

dispatch'
  :: (Applicative m, MonadIO m, MonadCatch m)
  => Dispatcher (WaiResource m)
  -> Request
  -> (Response -> IO ResponseReceived)
  -> m ResponseReceived
dispatch' d rq respond = maybe (sendNotFound respond) run disp where
  run r = handleRequest r rq respond
  disp = W.dispatch d (pathInfo rq)

sendNotFound
  :: MonadIO m
  => (Response -> IO ResponseReceived)
  -> m ResponseReceived
sendNotFound respond = liftIO $ respond $ responseLBS notFound404 [] "404 Not Found"

getRequest :: (MonadReader r m, HasWaiData r n) => m Request
getRequest = view waiDataRequest

getRequestHeader
  :: (Functor m, MonadReader r m, HasWaiData r n)
  => HeaderName
  -> m (Maybe ByteString)
getRequestHeader h = (snd <$>) . find ((h ==) . fst) . requestHeaders <$> getRequest

runWaiCrankT
  :: (Applicative m, MonadCatch m, MonadIO m)
  => WaiCrankT m a
  -> WaiData (WaiCrankT m)
  -> m (a, ReqData, LogData)
runWaiCrankT w d = do
  runRWST (unWaiCrankT w) d newReqData

handleRequest
  :: (Applicative m, MonadCatch m, MonadIO m)
  => WaiResource m
  -> Request
  -> (Response -> IO ResponseReceived)
  -> m ResponseReceived
handleRequest r rq respond = do
  now <- liftIO epochTime
  let rd = WaiData (newResourceData api r) rq (epochTimeToHTTPDate now)
  res <- API.handleRequest (flip runWaiCrankT rd)
  liftIO $ respond $ toWaiRes res

toWaiRes :: (Status, HeadersMap, Maybe Body) -> Response
toWaiRes (s, hs, b) = responseLBS s (hdrs hs) (fromMaybe LBS.empty b) where
  hdrs = join . fmap sequenceA . HashMap.toList

api :: (Applicative m, Monad m) => WaiServerAPI m
api = ServerAPI
  { srvGetRequestMethod = requestMethod <$> view waiDataRequest
  , srvGetRequestHeader = getRequestHeader
  , srvGetRequestURI = undefined
  , srvGetRequestTime = view waiDataRequestDate
  }
