{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Catch
import Data.Monoid
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp

import Webcrank.Wai

data MyData = MyData String

newtype MyApp a = MyApp { unMyApp :: StateT MyData Wai a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadWai
    )

runMyApp :: MyApp a -> MyData -> Wai a
runMyApp = evalStateT . unMyApp

runMyAppF :: MyData -> MyApp a -> Wai a
runMyAppF = flip runMyApp

index :: Path '[]
index = ""

indexResource :: MonadIO m => Resource m
indexResource = resourceWithHtml $ return $ textBody $ mconcat
  [ "<h1>Welcome to Webcrank!</h1>"
  , "<a href=\""
  , renderRoute echoPath $ "Hello" .*. HNil
  , "\">Allow me to greet you properly</a>"
  ]

echoPath :: Path '[Text]
echoPath = "echo" </> var

echoResource :: Monad m => Text -> Resource m
echoResource t = resourceWithHtml $ return $ textBody $  mconcat ["<h1>", t, "</h1>"]

myApp
  :: Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
myApp = dispatch (runMyAppF (MyData "a")) $ mconcat
  [ index ==> indexResource
  , echoPath ==> echoResource
  ]

main :: IO ()
main = Warp.run 3000 myApp

