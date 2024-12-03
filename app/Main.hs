{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (Exception, SomeException)
import Control.Monad.IO.Class ( MonadIO )
import Crypto.KDF.BCrypt qualified as BCrypt
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple qualified as DB
import Data.Aeson qualified as A
import Data.Foldable (for_)
import Data.Function ((&))
import Data.String (fromString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.ByteString qualified as ByteString
import Data.Text.Internal.Builder qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time.Clock (UTCTime)
import Data.Typeable ( Typeable )
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types.Status qualified as Status
import Network.HTTP.Types.URI qualified as URI
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.HttpAuth qualified as HttpAuth
import Network.Wai.Middleware.RequestLogger qualified as MW
import Network.Wai.Middleware.Static
import Network.Wai qualified as Wai
import Options (Options(..))
import Options qualified
import Prelude hiding (id)
import Text.Blaze.Html5 ((!), (!?))
import Text.Blaze.Html5.Attributes qualified as Attributes
import Text.Blaze.Html5 qualified as HTML
import Text.Blaze.Html qualified
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html.Renderer.Utf8 qualified as HTMLBS
import UnliftIO.Exception qualified as Exception
import Web.Scotty qualified as Scotty

import Migration qualified 

data ToDo = ToDo { id :: Int
                 , todo :: Text.Text
                 , done_date :: Maybe UTCTime
                 --      ... Nothing == "not done"
                 --      ... use FromField UTCTime
                 --      ... <https://hackage.haskell.org
                 --      ...               /package/sqlite-simple-0.4.18.2
                 --      ...               /docs/Database-SQLite-Simple-FromField.html
                 --      ...               #t:FromField>
                 }
                 deriving (Generic, FromRow, Show)

data User = User { userId :: Int
                 , userName :: Text.Text
                 , userPassword_hash :: Text.Text
                 -- , userPassword_hash :: ByteString.ByteString
                 }
                 deriving (Generic, FromRow, Show)

settings :: Warp.Port -> Warp.Settings
settings port = Warp.defaultSettings
  & Warp.setPort port
  & Warp.setOnException (\req e -> print e)
  & Warp.setOnExceptionResponse (\e -> Wai.responseLBS Status.status500 mempty $ HTMLBS.renderHtml $ do
        HTML.docTypeHtml $ do
            headTag "Error! Error!"

            HTML.body $ do
                HTML.h1 "Danger! Danger!"

  )

doneCheckboxID :: HTML.ToValue a => a -> HTML.AttributeValue
doneCheckboxID id = "doneCheckbox-" <> HTML.toValue id

doneCheckbox :: HTML.ToValue a => a -> Bool -> HTML.Html
doneCheckbox id val =
    HTML.input
        ! Attributes.type_ "checkbox"
        ! Attributes.id (doneCheckboxID id)
        ! Attributes.name "doneCheckbox"
        !? (val, Attributes.checked mempty)

updateForm1 :: ToDo -> HTML.Html
updateForm1 ToDo {id, todo, done_date} =
    let next = URI.urlEncode False ("/" <> Text.Encoding.encodeUtf8 (Text.pack (show id))) in
    HTML.form ! Attributes.action ("/" <> HTML.toValue id <> "?next=" <> HTML.toValue (Text.Encoding.decodeUtf8 next))
              ! Attributes.method "post" $ do
        HTML.p $ HTML.toMarkup $ "Current: " <> todo
        HTML.label ! Attributes.for "todo"
                    $ HTML.toMarkup (Text.pack "Updated:")
        HTML.input ! Attributes.type_ "text" ! Attributes.name "todo"
        HTML.input ! Attributes.type_ "submit" -- calls post on "/:id"

noFavIcon :: HTML.Html
noFavIcon =
    HTML.link ! Attributes.rel "icon"
              ! Attributes.href "data:,"


main :: IO ()
main = do
    argv@Options.Options { port, db } <- Options.getOptions

    DB.withConnection db $ \conn -> do

        Migration.run conn

        let opts = Scotty.defaultOptions { Scotty.settings = settings port }

        Scotty.scottyOpts opts (server conn argv)

authSettings :: HttpAuth.AuthSettings
authSettings = "Default"
  { HttpAuth.authIsProtected = \req -> return (Wai.pathInfo req /= ["signup"])
  , HttpAuth.authOnNoAuth = \realm req respond -> 
      if Wai.pathInfo req /= ["login"]
      then
        respond $ Wai.responseLBS
          Status.status301
          [ ("content-type", "text/plain")
          , ("Location", "/signup")
          ]
          "Please sign up" 
      else
        respond $ Wai.responseLBS
          Status.status401
          [ ("content-type", "text/plain")
          , ("WWW-Authenticate", "Basic realm=\"" <> realm <> "\"")
          ]
          "Basic authentication is required"
  }

server :: (HasCallStack) => DB.Connection -> Options.Options -> Scotty.ScottyM ()
server conn Options.Options { staticDir, reqLogger } = do
    Scotty.middleware reqLogger
    Scotty.middleware $ staticPolicy (noDots >-> addBase staticDir)
    Scotty.middleware $ flip HttpAuth.basicAuth authSettings $ \u p -> do
      users <- DB.queryNamed conn [sql|select user_id, name, password_hash
                     from users where name=:username ;|]
                     [ ":username" := Text.Encoding.decodeUtf8 u ] :: IO [User]

      case users of
        [] -> return False

        (User {userPassword_hash}:_) ->
          return $ BCrypt.validatePassword p (Text.Encoding.encodeUtf8 userPassword_hash)

    get "/login" $ do
        Scotty.redirect "/"

    get "/" $ do

        todos <- Scotty.liftIO $
            DB.query_ conn [sql|select id, todo, done_date
                                from todos
                                where done_date is null;|] :: Scotty.ActionM [ToDo]

        Scotty.addHeader "cache-control" "no-store"

        Scotty.html $ renderHtml $ HTML.docTypeHtml $ do
            headTag "To-Do's"

            HTML.body $ do

                HTML.div ! Attributes.class_ "heading-nav" $ do

                   HTML.h1 "To-Do's"

                   HTML.form ! Attributes.action "/" ! Attributes.method "post" $ do
                       HTML.input ! Attributes.type_ "text" ! Attributes.name "todo"
                       HTML.input ! Attributes.type_ "submit" ! Attributes.value "new" -- calls post on "/"
                   HTML.a ! Attributes.style "text-decoration: underline; color: cornflowerblue;"
                          ! Attributes.href "/logout" $ "logout"

                HTML.ul $ do
                    for_ todos $ \ToDo {id, todo, done_date} -> do
                        HTML.li ! Attributes.id ("todo-" <> HTML.toValue id)
                                ! Attributes.class_ "flex-container" $ do

                                    HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                           ! Attributes.href ("/" <> HTML.toValue id)
                                           $ HTML.toMarkup todo

                                    HTML.form
                                      ! Attributes.action ("/" <> HTML.toValue id <> "?next=%2Fadmin")
                                      ! Attributes.method "post" $ do
                                         HTML.input  ! Attributes.type_ "hidden"
                                                     ! Attributes.name "done"
                                                     ! Attributes.value (HTML.toValue True)
                                         HTML.button ! Attributes.type_ "submit" $ do
                                           "done"

    get "/admin" $ do
        todos <- Scotty.liftIO $
                    DB.query_ conn [sql|select id, todo, done_date from todos;|]
                        :: Scotty.ActionM [ToDo]

        Scotty.html $ renderHtml $ HTML.docTypeHtml $ do
            headTag "<< Admin Console >>"

            HTML.body $ do
                HTML.h1 "Admin Console: To-Do's"

                HTML.ul $ do
                    for_ todos $ \ToDo {id, todo, done_date} -> do
                        HTML.li ! Attributes.id ("todo-" <> HTML.toValue id)
                                ! Attributes.class_ "flex-container" $ do

                                   HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                          ! Attributes.href ("/" <> HTML.toValue id)
                                          $ case done_date of
                                              Just _  -> (HTML.s (HTML.toMarkup todo))
                                              Nothing -> HTML.toMarkup todo

                                   HTML.button ! Attributes.value (HTML.toValue id)
                                               ! Attributes.onclick "deleteToDo(this)"
                                               $ "delete"

                                   HTML.form
                                     ! Attributes.action ("/" <> HTML.toValue id <> "?next=%2F")
                                     ! Attributes.method "post" $ do
                                        HTML.input  ! Attributes.type_ "hidden"
                                                    ! Attributes.name "done"
                                                    ! Attributes.value (HTML.toValue False)
                                        HTML.button ! Attributes.type_ "submit" $ do
                                          "resurrect"

                                   HTML.p $ case done_date of
                                     Just datetime -> HTML.toMarkup $ "done on " <> show datetime
                                     Nothing       -> "not done"


                HTML.form ! Attributes.action "/" ! Attributes.method "post" $ do
                    HTML.input ! Attributes.type_ "text" ! Attributes.name "todo"
                    HTML.input ! Attributes.type_ "submit" -- calls post on "/"

    post "/" $ do
        todo <- Scotty.formParam "todo"
        Scotty.liftIO $
            DB.execute conn [sql|insert into todos (todo) values (?);|] (DB.Only todo :: DB.Only Text.Text)

        Scotty.redirect "/"

    post "/:id" $ do
        id <- Scotty.captureParam "id"        -- capture is path part of the URL
        todo <- Scotty.formParamMaybe "todo"  -- form is request body
        done <- Scotty.formParamMaybe "done"  --
        next <- Scotty.queryParamMaybe "next" -- query param part of the URL
        Scotty.liftIO $
            DB.executeNamed conn [sql|update todos
                                      set todo=case when :todo is null then todo else :todo end,
                                          done_date=case when :done is null
                                                      then done_date
                                                      else case :done when true then datetime()
                                                                      when false then null
                                                           end
                                                    end
                                      where id=:id;|]
                [ ":id" := (id :: Int),
                  ":todo" := (todo :: Maybe Text.Text),
                  ":done" := (done :: Maybe Bool) ]

        Scotty.redirect $ case next of
            Nothing -> ("/" <> Text.Lazy.pack (show id))
            Just p  -> Text.Lazy.Encoding.decodeUtf8 $ ByteString.Lazy.fromStrict $ URI.urlDecode False p

    get "/logout" $ do
        Scotty.status Status.status401

        Scotty.setHeader "WWW-Authenticate" "Basic realm=\"Default\""

    post "/signup" $ do
        username :: Text.Text <- Scotty.formParam "username"
        password :: ByteString.ByteString <- Scotty.formParam "password"
        hashed_password <- Scotty.liftIO $ BCrypt.hashPassword 12 password 
        Scotty.liftIO $
            DB.executeNamed conn [sql|insert into users (name, password_hash)
                                      values (:username, :hashed_password);|]
                [ ":username" := username 
                , ":hashed_password" := Text.Encoding.decodeUtf8 hashed_password
                ]

        Scotty.redirect "/"

    get "/signup" $ do
        Scotty.html $ renderHtml $ HTML.docTypeHtml $ do
            headTag "<< Signup >>"
 
            HTML.body $ do
                HTML.h1 "Signup"
 
                HTML.span $ do
                    HTML.p "or"
                    HTML.a ! Attributes.href "/login" $ "Login"

                HTML.form ! Attributes.class_ "signup-form"
                          ! Attributes.action "/signup"
                          ! Attributes.method "post" $ do
                    HTML.label ! Attributes.for "username" $ "username:"
                    HTML.input ! Attributes.type_ "text" ! Attributes.name "username"
                    HTML.label ! Attributes.for "password" $ "password:"
                    HTML.input ! Attributes.type_ "password" ! Attributes.name "password"
                    HTML.input ! Attributes.type_ "submit" ! Attributes.value "signup"

    delete "/:id" $ do
        id <- Scotty.captureParam "id"
        Scotty.liftIO $
            DB.executeNamed conn [sql|delete from todos where id=:id ;|]
                [ ":id" := (id :: Int) ]

    get "/:id" $ do
        id <- Scotty.captureParam "id"

        todos <- Scotty.liftIO $
            DB.queryNamed conn [sql|select id, todo, done_date from todos where id=:id ;|]
                [ ":id" := (id :: Int) ] :: Scotty.ActionM [ToDo]
        if null todos
            then
                Scotty.status Status.status404
            else
                Scotty.html $ renderHtml $ HTML.docTypeHtml $ do
                    headTag $ mapM_ (HTML.toMarkup . todo) todos

                    HTML.body $ do
                        HTML.h1 $ HTML.toMarkup (Text.pack "Editing: ")
                                <> mapM_ (HTML.toMarkup . todo) todos

                        mapM_ updateForm1 todos


headTag title =
  HTML.head $ do
    HTML.link ! Attributes.rel "stylesheet" ! Attributes.href "main.css"
    HTML.title $ "Talk to a Database | " <> title

    noFavIcon

    HTML.script ! Attributes.type_ "text/javascript" ! Attributes.src "utils.js" $ mempty

data Exc = Exc Text
  deriving (Show, Exception)

get :: Scotty.RoutePattern -> Scotty.ActionM () -> Scotty.ScottyM ()
get p act =
  Scotty.get p
    (act `Exception.catch` (\(e :: SomeException) -> do
                             Scotty.liftIO (print e)
                             Exception.throwIO e))

post :: Scotty.RoutePattern -> Scotty.ActionM () -> Scotty.ScottyM ()
post p act =
  Scotty.post p
    (act `Exception.catch` (\(e :: SomeException) -> do
                             Scotty.liftIO (print e)
                             Exception.throwIO e))

delete :: Scotty.RoutePattern -> Scotty.ActionM () -> Scotty.ScottyM ()
delete p act =
  Scotty.delete p
    (act `Exception.catch` (\(e :: SomeException) -> do
                             Scotty.liftIO (print e)
                             Exception.throwIO e))

