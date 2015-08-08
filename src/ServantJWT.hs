{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleInstances #-}
module ServantJWT where

import Servant.API
import Servant.API.Authentication
import Servant.Server.Internal.Authentication
import Web.JWT
import Data.Text
import Network.Wai
import Data.Text.Encoding
import Control.Monad

type JWTProtect a = AuthProtect (JWT UnverifiedJWT) (JWT VerifiedJWT) a

instance AuthData (JWT UnverifiedJWT) where
  authData = Web.JWT.decode . decodeUtf8 <=< lookup "Authorization" . requestHeaders

-- | verify but lifted into IO.
verify' :: Secret -> JWT UnverifiedJWT -> IO (Maybe (JWT VerifiedJWT))
verify' secret = return . Web.JWT.verify secret






