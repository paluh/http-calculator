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

toResponseBody :: Db -> ResponseBody
toResponseBody ops = ResponseBody (sum ops) ops

type Calculator
  = "add" :> Capture "x" Int :> Get '[JSON] ResponseBody
  :<|> "sub" :> Capture "y" Int :> Get '[JSON] ResponseBody
  :<|> "reset" :> Get '[JSON] ResponseBody

add :: IORef Db -> Int -> IO ResponseBody
add db value =
  atomicModifyIORef' db (\ops -> let n = (value : ops) in (n, toResponseBody n))

sub :: IORef Db -> Int -> IO ResponseBody
sub db value =
  atomicModifyIORef' db (\ops -> let n = ((-1) * value : ops) in (n, toResponseBody n))

reset :: IORef Db -> IO ResponseBody
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

