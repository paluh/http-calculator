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
import Data.Aeson.Types
import Data.IORef (atomicModifyIORef', IORef, newIORef, modifyIORef, readIORef, writeIORef)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type Db = [Int]
data ResponseBody = ResponseBody { result :: Int, operands :: [Int] }
  deriving (Eq, Show, Generic)
instance ToJSON ResponseBody

type AppM = ReaderT (IORef Db) Handler

toResponseBody :: Db -> ResponseBody
toResponseBody ops = ResponseBody (sum ops) ops

type Calculator
  = "add" :> Capture "x" Int :> Get '[JSON] ResponseBody
  :<|> "sub" :> Capture "y" Int :> Get '[JSON] ResponseBody
  :<|> "reset" :> Get '[JSON] ResponseBody

add :: Int -> AppM ResponseBody
add value = do
  db <- ask
  liftIO $ atomicModifyIORef' db (\ops -> let n = (value : ops) in (n, toResponseBody n))

sub :: Int -> AppM ResponseBody
sub value = do
  db <- ask
  liftIO $ atomicModifyIORef' db (\ops -> let n = ((-1) * value : ops) in (n, toResponseBody n))

reset :: AppM ResponseBody
reset = do
  db <- ask
  liftIO $ atomicModifyIORef' db (const $ ([], toResponseBody []))

calculatorAPI :: Proxy Calculator
calculatorAPI = Proxy

main :: IO ()
main = do
  db <- newIORef []
  let
    server = hoistServer calculatorAPI (flip runReaderT db) (add :<|> sub :<|> reset)
  run 8081 (serve calculatorAPI server)

