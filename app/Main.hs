{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main
where

import Data.Aeson
import Data.Time
import Data.List
import Servant
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Either

type DocAPI = "docs" :> Capture "tag" String :> Get '[JSON] [Doc]

data Doc = Doc
  { url :: String
  , tags :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON Doc

docAPI :: Proxy DocAPI
docAPI = Proxy

server :: Server DocAPI
server = docs
  where docs :: String -> EitherT ServantErr IO [Doc]
        docs t = return $ queryDocs t

queryDocs t = filter (match t . tags) allDocs

match :: String -> [String] -> Bool
match t = any (isInfixOf t)

allDocs :: [Doc]
allDocs = [ Doc "http://i.imgur.com/dvDHMQV.gif" ["tag1", "tag2"] ]

app :: Application
app = serve docAPI server

main :: IO ()
main = run 8081 app
