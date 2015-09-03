{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Time
import Data.List
import Servant
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Either

type DocAPI =
    -- GET /docs/:tag
         "docs" :> QueryParam "tag" String :> Get '[JSON] [Doc]
    -- POST /docs
    :<|> "docs" :> ReqBody '[JSON] Doc :> Post '[JSON] Doc
    -- -- DELETE /docs/:id
    -- :<|> "docs" :> Capture "docId" String :> Delete

data Doc = Doc
  { url :: String
  , tags :: [String]
  } deriving (Eq, Generic)

instance ToJSON Doc
instance FromJSON Doc

instance Show Doc where
  show (Doc c ts) = show c

docAPI :: Proxy DocAPI
docAPI = Proxy

server :: Server DocAPI
server = docsByTag :<|> postDocH -- :<|> deleteDocH
  where docsByTag :: Maybe String -> EitherT ServantErr IO [Doc]
        docsByTag Nothing = return allDocs
        docsByTag (Just t) = return $ queryDocs t

        postDocH :: Doc -> EitherT ServantErr IO Doc
        postDocH = return

        -- deleteDocH _ = return ()

queryDocs :: String -> [Doc]
queryDocs t = filter (match t . tags) allDocs

match :: String -> [String] -> Bool
match t = any (isInfixOf t)

allDocs :: [Doc]
allDocs = [
    Doc "http://i.imgur.com/dvDHMQV.gif" ["tag1", "tag2"],
    Doc "http://i.imgur.com/dvDHMQV.gif" ["Shturf", "gif"],
    Doc "http://i.imgur.com/dvDHMQV.gif" ["polar", "winning"],
    Doc "http://i.imgur.com/dvDHMQV.gif" ["bear-polar", "penguin"]
    ]

app :: Application
app = serve docAPI server

runServer :: Port -> IO ()
runServer port = run port app

-- Put this all to work!
main :: IO ()
main = do
    putStrLn "starting app on port 8081"
    runServer 8081
