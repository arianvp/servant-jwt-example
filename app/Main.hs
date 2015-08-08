{-#LANGUAGE TemplateHaskell, DataKinds, TypeOperators, OverloadedStrings #-}
module Main where
import Data.Aeson.TH
import Data.Text
import Data.Default
import Servant.API
import Servant.API.Authentication
import Servant.Server
import Servant.Server.Internal.Authentication
import Web.JWT hiding (JSON)
import ServantJWT
import Data.Proxy
import Network.Wai.Handler.Warp

import System.Environment
import Control.Monad.Trans.Either


data Grant = Grant
  { token :: Text
  }
$(deriveJSON defaultOptions ''Grant)

type User = Text


type GrantAPI = "grant" :> Get '[JSON] Grant
grantToken :: Secret -> Maybe User -> EitherT ServantErr IO Grant
grantToken secret Nothing = left err401

-- A stub that always grants a token that's forever valid.
-- TODO:  Check if token expired
grantToken s (Just x) = return . Grant $ encodeSigned HS256 s def

grantServer s = laxProtect auth (grantToken s)
  where auth ba =
          if baUser ba == "arian" && baPass ba == "test"
            then return . Just $ "arian"
            else return Nothing


data Message = Message
  { message :: Text
  }

$(deriveJSON defaultOptions ''Message)

type API = (JWTProtect Lax :> MessageAPI)
       :<|> (AuthProtect (BasicAuth "grant") User Lax :> GrantAPI)

type MessageAPI = "message" :> Get '[JSON] Message

messageServer s = laxProtect (verify' s) getMessage

server :: Secret -> Server API
server s = messageServer s :<|> grantServer s

getMessage :: Maybe (JWT VerifiedJWT) -> EitherT ServantErr IO Message
getMessage (Just jwt) = return . Message $ "You've been authorized"
getMessage _          = left err401


main :: IO ()
main = do
  secretKey <- secret . maybe "secret" pack  <$> lookupEnv "JWT_SECRET"
  run 8080 . serve  (Proxy :: Proxy API) $  server secretKey 
  return ()
