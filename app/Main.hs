{-#LANGUAGE TemplateHaskell, DataKinds, TypeOperators, OverloadedStrings #-}
module Main where
import Data.Aeson.TH
import Data.Text
import Servant.API
import Servant.API.Authentication (AuthPolicy (Lax))
import Servant.Server
import Servant.Server.Internal.Authentication
import Web.JWT hiding (JSON)
import ServantJWT
import Data.Proxy
import Network.Wai.Handler.Warp

import System.Environment

data Message = Message
  { message :: Text
  }

$(deriveJSON defaultOptions ''Message)

-- type GrantAPI = "grant" :> ReqBody '[JSON] Credentials :> Post '[JSON] Grant


type API = JWTProtect Lax :> SecretAPI

type SecretAPI = "message" :> Get '[JSON] Message

server :: Secret -> Server API
server s = laxProtect (verify' s) getMessage

getMessage :: Monad m => Maybe (JWT VerifiedJWT) -> m Message
getMessage (Just jwt) = return . Message $ "You've been authorized"
getMessage _          = return . Message $ "access denied"


main :: IO ()
main = do
  secretKey <- secret . maybe "secret" pack  <$> lookupEnv "JWT_SECRET"
  run 8080 . serve  (Proxy :: Proxy API) $  server secretKey 
  return ()
