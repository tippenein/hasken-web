{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Time
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad
import Control.Monad.IO.Class (liftIO)

type DocAPI = "users" :> Get '[JSON] [Doc]

data Doc = Doc
  { title :: String
  , content :: String
  , tags :: [String]
  } deriving (Eq, Show, Generic)

-- orphan ToJSON instance for Day. necessary to derive one for Doc
instance ToJSON Day where
  -- display a day in YYYY-mm-dd format
  toJSON d = toJSON (showGregorian d)

instance ToJSON Doc

docAPI :: Proxy DocAPI
docAPI = Proxy

server :: Server DocAPI
server = return docs

docs :: [Doc]
docs = [ Doc "Isaac Newton" "derp" ["tag1", "tag2"] ]

app :: Application
app = serve docAPI server

getCurrentDate = liftM utctDay getCurrentTime

main :: IO ()
main = run 8000 app
