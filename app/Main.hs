module Main where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.OpenApi hiding (Server, delete, server, title, url)
import GHC.Generics
import Network.HTTP.Types
import Network.URI
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.OpenApi
import Prelude hiding (id)

main :: IO ()
main = do
  mState <- newMVar $ AppState (0, 1) []
  let frontCors =
        simpleCorsResourcePolicy
          { corsOrigins = Just (["http://localhost:8080", "http://localhost:4200"], True),
            corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"],
            corsRequestHeaders = ["Authorization", "Content-Type", "Navigation"]
          }
  putStrLn "Serving on 8080"
  run 8080 $
    cors (const $ Just frontCors) $ serve (Proxy @API) $ server mState

server :: MVar AppState -> Server API
server mvar =
  create
    :<|> update
    :<|> delete
    :<|> list
    :<|> spec
  where
    create :: LinkContent -> Handler Link
    create LinkContent {..} = do
      checkUrl url
      newQl <- liftIO $
        modifyMVar mvar $ \state ->
          let nextId = uncurry (+) $ nextIdMem state
              ql = Link {id = nextId, ..}
           in return (AppState (snd $ nextIdMem state, nextId) (ql : links state), ql)
      logState "Link added"
      return newQl
    update :: Int -> LinkContent -> Handler NoContent
    update qlId LinkContent {..} = do
      checkUrl url
      updated <- liftIO $
        modifyMVar mvar $ \state ->
          let up q =
                if id q == qlId
                  then Link {id = qlId, ..}
                  else q
              updatedQL = map up $ links state
           in return (state {links = updatedQL}, any ((== qlId) . id) updatedQL)
      if updated
        then do
          logState "Link updated"
          return NoContent
        else throwError $ err404 {errBody = "Unknown Link"}
    delete :: Int -> Handler NoContent
    delete qlId = do
      deleted <- liftIO $
        modifyMVar mvar $ \state ->
          let updatedQL = filter ((/= qlId) . id) $ links state
           in return (state {links = updatedQL}, updatedQL /= links state)
      if deleted
        then do
          logState "Link deleted"
          return NoContent
        else throwError $ err404 {errBody = "Unknown Link"}
    list :: Handler [Link]
    list = links <$> liftIO (readMVar mvar)
    spec :: Handler Spec
    spec = return $ Spec $ toJSON $ toOpenApi (Proxy :: Proxy API)
    checkUrl :: String -> Handler ()
    checkUrl url =
      when (isNothing $ parseURI url) $
        throwError $
          err400 {errBody = "invalid Link URL"}
    logState :: String -> Handler ()
    logState op =
      liftIO $ do
        putStrLn "---"
        putStrLn $ "Action '" <> op <> "' performed"
        putStrLn "New state:"
        readMVar mvar >>= mapM_ print . links

type QL a = "api" :> "links" :> a

type API =
  Summary "Create a new Link"
    :> QL (ReqBody '[JSON] LinkContent :> Post '[JSON] Link)
    :<|> Summary "Edit a Link"
      :> QL (Capture "id" Int :> ReqBody '[JSON] LinkContent :> Verb 'PUT 204 '[JSON] NoContent)
    :<|> Summary "Delete a Link"
      :> QL (Capture "id" Int :> Verb 'DELETE 204 '[JSON] NoContent)
    :<|> Summary "List Links"
      :> QL (Get '[JSON] [Link])
    :<|> Summary "API Specifications (OpenAPI)"
      :> "api"
      :> Get '[JSON] Spec

data LinkContent = LinkContent
  { title :: String,
    url :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

data Link = Link
  { id :: Int,
    title :: String,
    url :: String
  }
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance Show Link where
  show Link {..} = show id <> " | " <> title <> " | " <> url

newtype Spec = Spec Value
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance ToSchema Spec where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Spec") mempty

data AppState = AppState
  { nextIdMem :: (Int, Int),
    links :: [Link]
  }
