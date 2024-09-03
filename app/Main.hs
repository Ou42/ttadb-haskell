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
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple qualified as DB
import Data.Foldable (for_)
import Data.Function ((&))
import Data.String (fromString)
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
import Network.Wai.Middleware.RequestLogger qualified as MW
import Network.Wai.Middleware.Static
import Network.Wai qualified as Wai
import Options (Options(..))
import Options qualified 
import Prelude hiding (id)
import Prelude qualified
import Text.Blaze.Html5 ((!), (!?))
import Text.Blaze.Html5.Attributes qualified as Attributes
import Text.Blaze.Html5 qualified as HTML
import Text.Blaze.Html qualified 
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html.Renderer.Utf8 qualified as HTMLBS
import UnliftIO.Exception qualified as Exception
import Web.Scotty qualified as Scotty

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

data Hello = Hello
  deriving (Show, Exception)

main :: IO ()
main = do
    argv@Options.Options { port, db } <- Options.getOptions

    DB.withConnection db $ \conn -> do

        DB.execute_ conn [sql|create table if not exists todos
                            ( id INTEGER primary key autoincrement
                            , todo TEXT
                            , done_date TEXT -- can add a check constraint: date time fmt
                            );|] -- between [sql| ... |] is a quasi-quoter, this is SQL not Haskell

        let opts = Scotty.defaultOptions { Scotty.settings = settings port }

        Scotty.scottyOpts opts (server conn argv)

server :: (HasCallStack) => DB.Connection -> Options.Options -> Scotty.ScottyM ()
server conn Options.Options { staticDir, reqLogger } = do
    Scotty.middleware reqLogger
    Scotty.middleware $ staticPolicy (noDots >-> addBase staticDir)

    get "/" $ do

        Scotty.liftIO $ print "I'm processing a GET!" 

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

                HTML.ul $ do
                    for_ todos $ \ToDo {id, todo, done_date} -> do
                        HTML.li ! Attributes.id ("todo-" <> HTML.toValue id) $ do
                            HTML.div ! Attributes.class_ "flex-container" $ do

                                HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                       ! Attributes.href ("/" <> HTML.toValue id)
                                       $ HTML.toMarkup todo

                                HTML.form
                                  ! Attributes.action ("/" <> HTML.toValue id <> "?next=%2F")
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
                        HTML.li ! Attributes.id ("todo-" <> HTML.toValue id) $ do
                            HTML.div ! Attributes.class_ "flex-container" $ do

                                HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                       ! Attributes.href ("/" <> HTML.toValue id)
                                       $ case done_date of
                                           Just _  -> (HTML.s (HTML.toMarkup todo))
                                           Nothing -> HTML.toMarkup todo

                                HTML.button ! Attributes.value (HTML.toValue id)
                                            ! Attributes.onclick "deleteToDo(this)"
                                            $ "delete"

                                HTML.form
                                  ! Attributes.action "/resurrect"
                                  ! Attributes.method "post" $ do
                                     HTML.input  ! Attributes.type_ "hidden"
                                                 ! Attributes.name "id"
                                                 ! Attributes.value (HTML.toValue id)
                                     HTML.button ! Attributes.style "background-color: green;"
                                                 ! Attributes.type_ "submit" $ "toggle done"

                                     doneCheckbox id (done_date /= Nothing)

                                     HTML.label  ! Attributes.for (doneCheckboxID id)
                                       $ case done_date of
                                           Just datetime -> HTML.toMarkup
                                                            $ "done on " <> show datetime
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

