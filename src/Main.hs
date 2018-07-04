{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.IORef (atomicModifyIORef', IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

type State = [Int]
data ResponseBody = ResponseBody { result :: Int, operands :: [Int] }
  deriving (Eq, Show, Generic)
instance ToJSON ResponseBody

toResponseBody :: State -> ResponseBody
toResponseBody ops = ResponseBody (sum ops) ops

type Calculator
  = "add" :> Capture "x" Int :> Get '[JSON] ResponseBody
  :<|> "sub" :> Capture "y" Int :> Get '[JSON] ResponseBody
  :<|> "reset" :> Get '[JSON] ResponseBody

add :: IORef State -> Int -> IO ResponseBody
add db value =
  atomicModifyIORef' db (\ops -> let n = (value : ops) in (n, toResponseBody n))

sub :: IORef State -> Int -> IO ResponseBody
sub db value =
  atomicModifyIORef' db (\ops -> let n = ((-1) * value : ops) in (n, toResponseBody n))

reset :: IORef State -> IO ResponseBody
reset db =
  atomicModifyIORef' db (const $ ([], toResponseBody []))

calculatorAPI :: Proxy Calculator
calculatorAPI = Proxy

main :: IO ()
main = do
  db <- newIORef []
  let
    server db = (liftIO . add db) :<|> (liftIO . sub db) :<|> (liftIO $ reset db)
  run 8081 (serve calculatorAPI (server db))

