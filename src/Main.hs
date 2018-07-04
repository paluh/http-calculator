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
import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
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

data State = State
  { sum :: Int
  , operands :: [Int]
  } deriving (Eq, Show, Generic)
instance ToJSON State

type Calculator
  = "add" :> Capture "x" Int :> Get '[JSON] State
  :<|> "sub" :> Capture "y" Int :> Get '[JSON] State
  :<|> "reset" :> Get '[JSON] State

add :: IORef State -> Int -> IO State
add db value = do
  modifyIORef db (\(State s ops) -> State (s + value) (value : ops))
  readIORef db

sub :: IORef State -> Int -> IO State
sub db value = do
  modifyIORef db (\(State s ops) -> State (s - value) ((-1) * value : ops))
  readIORef db

reset :: IORef State -> IO State
reset db = do
  modifyIORef db (const $ empty)
  readIORef db

calculatorAPI :: Proxy Calculator
calculatorAPI = Proxy

-- | Do we want Monoid for this?
empty = State 0 []


main :: IO ()
main = do
  db <- newIORef empty
  let
    server db = (liftIO . add db) :<|> (liftIO . sub db) :<|> (liftIO $ reset db)
  run 8081 (serve calculatorAPI (server db))

