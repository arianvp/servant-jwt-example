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

import Data.Map (Map)
import Data.Set (Set)
import Data.ByteString
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Aeson
import Control.Monad


fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' json =
  case fromJSON json of
    Success a -> Just a
    _         -> Nothing

data Grant = Grant
  { token :: Text
  }
$(deriveJSON defaultOptions ''Grant)

data Creds = Creds
  { password :: ByteString
  , roles :: Set Role
  }
-- roles for resources 
--
data Role = Homepage
          | Msg 
          deriving (Ord, Eq)
$(deriveJSON defaultOptions ''Role)

type User = ByteString

creds :: Map User Creds
creds = Map.fromList
  [ ("arian", Creds "test" (Set.fromList [Msg, Homepage]))
  , ("aaron", Creds "test" (Set.fromList [Homepage]))
  , ("peter", Creds "test" (Set.empty))
  ]

type GrantAPI = "grant" :> Get '[JSON] Grant
grantToken :: Secret -> Maybe User -> EitherT ServantErr IO Grant
grantToken secret Nothing = left err401

-- A stub that always grants a token that's forever valid.
-- TODO:  Check if token expired
grantToken s (Just user) = do
  let (Just (Creds _ roles)) =  Map.lookup user creds
  let stuff = def { unregisteredClaims = Map.singleton "roles" . toJSON . Set.toList $ roles }
  return $ Grant $ encodeSigned HS256 s stuff

grantServer s = laxProtect auth (grantToken s)
  where
    auth ba = return $ do
      Creds pass roles <- Map.lookup (baUser ba) creds
      guard $ baPass ba == pass
      return $ baUser ba

data Message = Message
  { message :: Text
  }

$(deriveJSON defaultOptions ''Message)


verifyToken :: Secret -> JWT UnverifiedJWT -> IO (Maybe (Set Role))
verifyToken s jwt = return $ do
  fromJSON' =<< Map.lookup "roles" =<< unregisteredClaims . claims <$> Web.JWT.verify s jwt

type API =  (AuthProtect (JWT UnverifiedJWT) (Set Role) Lax :> MessageAPI)
       :<|> (AuthProtect (BasicAuth "grant") User Lax :> GrantAPI)
       :<|> (AuthProtect (JWT UnverifiedJWT)(Set Role) Lax :> HomepageAPI) 

type MessageAPI = "message" :> Get '[JSON] Message
messageServer s = laxProtect (verifyToken s) getMessage
getMessage (Just roles) =
  if Msg `Set.member` roles
      then return . Message $ "Welcome. you're authorized"
      else left err403
getMessage _          = left err401


type HomepageAPI = "home" :> Get '[JSON] Text
homepageServer s = laxProtect (verifyToken s) getHomepage
getHomepage (Just roles) =
  if Homepage `Set.member` roles
      then return "Homepage"
      else left err403

server :: Secret -> Server API
server s = messageServer s :<|> grantServer s :<|> homepageServer s


main :: IO ()
main = do
  secretKey <- secret . maybe "secret" Data.Text.pack  <$> lookupEnv "JWT_SECRET"
  run 8080 . serve  (Proxy :: Proxy API) $  server secretKey 
  return ()
