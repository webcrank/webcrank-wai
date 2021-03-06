{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Webcrank.Wai
  ( WaiResource
  , WaiCrankT
  , dispatch
  , HasRequest(..)
  , HasRequestDate(..)
  , WaiData
  , ReqData
  , getRequestBodyLBS
  , module Webcrank
  , module Webcrank.Dispatch
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Control.Monad.RWS
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Traversable
import Network.Wai hiding (rawPathInfo, pathInfo, requestHeaders)
import Network.Wai.Lens
import Prelude
import System.PosixCompat.Time

import Webcrank
import Webcrank.Dispatch hiding (dispatch)
import qualified Webcrank.Dispatch as W
import Webcrank.ServerAPI hiding (handleRequest)
import qualified Webcrank.ServerAPI as API

-- | Holds the request and resource state during request processing.
data WaiData m = WaiData
  { _resourceData :: ResourceData m
  , _waiDataRequest :: Request
  , _waiDataRequestDate :: HTTPDate
  }

makeFields ''WaiData

instance HasResourceData (WaiData m) m where
  resourceData f ~(WaiData rd rq d) = fmap (\rd' -> WaiData rd' rq d) (f rd)

-- | Monad transformer that all resource functions will run in. Provides
-- the ability to read the WAI @'Request'@ inside resource functions.
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

-- | Function for turning @'Dispatcher'@s into @'Application'@s.
-- A @run@ function must be provided for executing the application
-- logic.  In the simplest case, where @m@ is @IO@, this can just be
-- @'id'@.
dispatch
  :: (Applicative m, MonadIO m, MonadCatch m)
  => (forall a. m a -> IO a) -- ^ run
  -> ByteString -- ^ base uri
  -> Dispatcher (WaiResource m)
  -> Application
dispatch f u d rq = f . dispatch' d u rq

dispatch'
  :: (Applicative m, MonadIO m, MonadCatch m)
  => Dispatcher (WaiResource m)
  -> ByteString -- ^ base uri
  -> Request
  -> (Response -> IO ResponseReceived)
  -> m ResponseReceived
dispatch' d u rq respond = maybe (sendNotFound respond) run disp where
  run r = handleRequest r u rq respond
  disp = W.dispatch d (rq ^. pathInfo)

sendNotFound
  :: MonadIO m
  => (Response -> IO ResponseReceived)
  -> m ResponseReceived
sendNotFound respond = liftIO $ respond $ responseLBS notFound404 [] "404 Not Found"

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
  -> ByteString -- ^ base uri
  -> Request
  -> (Response -> IO ResponseReceived)
  -> m ResponseReceived
handleRequest r u rq respond = do
  now <- liftIO epochTime
  let rd = WaiData (newResourceData (api u) r) rq (epochTimeToHTTPDate now)
  res <- API.handleRequest (flip runWaiCrankT rd)
  liftIO $ respond $ waiRes res

waiRes :: (Status, HeadersMap, Maybe Body) -> Response
waiRes (s, hs, b) = responseLBS s (hdrs hs) (fromMaybe LB.empty b) where
  hdrs = join . fmap sequenceA . HashMap.toList

api :: (Applicative m, Monad m) => ByteString -> WaiServerAPI m
api baseUri = ServerAPI
  { srvGetRequestMethod = requestMethod <$> view request
  , srvGetRequestHeader = \h ->
      preview (request . headers . value h)
  , srvGetRequestURI = view $ request . rawPathInfo . to (B.takeWhile (/= 63) . (baseUri <>))
  , srvGetRequestTime = view requestDate
  }

getRequestBodyLBS :: (MonadIO m, MonadReader s m, HasRequest s Request) => m LB.ByteString
getRequestBodyLBS = liftIO . lazyRequestBody =<< view request

