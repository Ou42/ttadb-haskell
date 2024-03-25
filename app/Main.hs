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
import Data.Text.Internal.Builder qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Typeable ( Typeable )
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types.Status qualified as Status
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger qualified as MW
import Network.Wai.Middleware.Static
import Network.Wai qualified as Wai
import Options (Options(..))
import Options qualified 
import Prelude hiding (id)
import Prelude qualified
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes qualified as Attributes
import Text.Blaze.Html5 qualified as HTML
import Text.Blaze.Html qualified 
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html.Renderer.Utf8 qualified as HTMLBS
import UnliftIO.Exception qualified as Exception
import Web.Scotty qualified as Scotty
import Web.Scotty.Trans qualified as ScottyT


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

checkbox :: HTML.ToValue a => Bool -> a -> HTML.Html
checkbox True id =
    HTML.input
        ! Attributes.type_ "checkbox"
        ! Attributes.id ("doneCkbx-" <> HTML.toValue id)
        ! Attributes.name ("doneCkbx-" <> HTML.toValue id)
        ! Attributes.disabled (HTML.toValue True)
        ! Attributes.checked "checked"

checkbox False id =
    HTML.input
        ! Attributes.type_ "checkbox"
        ! Attributes.id ("doneCkbx-" <> HTML.toValue id)
        ! Attributes.name ("doneCkbx-" <> HTML.toValue id)

updateForm1 :: ToDo -> HTML.Html
updateForm1 ToDo {id, todo, done_date} =
-- updateForm1 ToDo {id, todo} =
-- updateForm1 ToDo {id, todo, done, done_date} =
    HTML.form ! Attributes.action ("/" <> HTML.toValue id)
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
    Options.Options { port, db } <- Options.getOptions

    -- jsFile <- readFile "app/utils.js"

    DB.withConnection db $ \conn -> do

        DB.execute_ conn [sql|create table if not exists todos
                            ( id INTEGER primary key autoincrement
                            , todo TEXT
                            -- , done BOOLEAN default FALSE
                            , done_date TEXT -- can add a check constraint: date time fmt
                            );|] -- between [sql| ... |] is a quasi-quoter, this is SQL not Haskell

        let opts = ScottyT.defaultOptions { ScottyT.settings = settings port }

        let run = Prelude.id
  -- note: we use 'id' since we don't have to run any effects at each action
        ScottyT.scottyOptsT opts run (server conn)
        -- ScottyT.scottyOptsT opts run (server conn jsFile cssFile)

-- Any custom monad stack will need to implement 'MonadUnliftIO'
-- server :: MonadUnliftIO m => ScottyT.ScottyT m ()
-- server :: (HTML.ToMarkup a1, HTML.ToMarkup a2, HasCallStack)
--            => DB.Connection -> a2 -> a1 -> ScottyT.ScottyT IO ()
-- server conn jsFile cssFile = do

server :: (HasCallStack) => DB.Connection -> ScottyT.ScottyT IO ()
server conn = do
    Scotty.middleware MW.logStdoutDev
    Scotty.middleware $ staticPolicy (noDots >-> addBase "static")

    -- get :: _ 
    get "/" $ do

        -- Scotty.liftIO $ Control.Exception.throwIO Hello
        -- Scotty.throw Hello
        Scotty.liftIO $ print "I'm processing a GET!" 

        todos <- Scotty.liftIO $
            DB.query_ conn [sql|select id, todo, done_date from todos;|] :: Scotty.ActionM [ToDo]

        Scotty.html $ renderHtml $ HTML.docTypeHtml $ do
            headTag "To-Do's"

            HTML.body $ do
                HTML.h1 "To-Do's"

                HTML.ul $ do
                    -- for_ todos $ \ToDo {id, todo} -> do
                    for_ todos $ \ToDo {id, todo, done_date} -> do
                        HTML.li ! Attributes.id ("todo-" <> HTML.toValue id) $ do
                            HTML.div ! Attributes.class_ "flex-container" $ do

                                HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                        ! Attributes.href ("/" <> HTML.toValue id)
                                        $ HTML.toMarkup todo

                                HTML.button ! Attributes.value (HTML.toValue id)
                                            ! Attributes.onclick "deleteToDo(this)"
                                            $ "delete"

                HTML.form ! Attributes.action "/" ! Attributes.method "post" $ do
                    HTML.input ! Attributes.type_ "text" ! Attributes.name "todo"
                    HTML.input ! Attributes.type_ "submit" -- calls post on "/"


    get "/csstest" $ do
      Scotty.html $ renderHtml $ HTML.docTypeHtml $ do
        headTag "CSS Test page"

        HTML.ul $ do
            let todos = [ ToDo { id = 0, todo = "zero", done_date = Nothing }
                        , ToDo { id = 1, todo = "one", done_date = Nothing }
                        ]

            for_ todos $ \ToDo {id, todo, done_date} -> do
                HTML.li ! Attributes.id ("todo-" <> HTML.toValue id) $ do
                    HTML.div ! Attributes.class_ "flex-container" $ do

                        HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                ! Attributes.href ("/" <> HTML.toValue id)
                                $ HTML.toMarkup todo


                        let toConsole = "console.log('*NOT* deleting: ' + this + ' btn: ' + this.value)"
                        HTML.button ! Attributes.value (HTML.toValue id)
                                    ! Attributes.onclick toConsole 
                                    -- ! Attributes.onclick "DONOTdeleteToDo(this)"
                                    $ "do nothing" -- "delete"

    get "/admin" $ do
        todos <- Scotty.liftIO $
                    DB.query_ conn [sql|select id, todo, done_date from todos;|]
                        :: Scotty.ActionM [ToDo]
                    -- DB.query_ conn [sql|select id, todo from todos;|] :: Scotty.ActionM [ToDo]

        Scotty.html $ renderHtml $ HTML.docTypeHtml $ do
            headTag "<< Admin Console >>"

            HTML.body $ do
                HTML.h1 "To-Do's"

                HTML.ul $ do
                    for_ todos $ \ToDo {id, todo, done_date} -> do
                        HTML.li ! Attributes.id ("todo-" <> HTML.toValue id) $ do
                            HTML.div ! Attributes.class_ "flex-container" $ do

                                HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                        ! Attributes.href ("/" <> HTML.toValue id)
                                        $ HTML.toMarkup todo

                                -- HTML.p $ HTML.span (if done then "checked" else "!checkd")

                                -- checkbox done id

                                HTML.button ! Attributes.value (HTML.toValue id)
                                            ! Attributes.onclick "deleteToDo(this)"
                                            $ "delete"

                HTML.form ! Attributes.action "/" ! Attributes.method "post" $ do
                    HTML.input ! Attributes.type_ "text" ! Attributes.name "todo"
                    HTML.input ! Attributes.type_ "submit" -- calls post on "/"

    Scotty.post "/" $ do
        todo <- Scotty.formParam "todo"
        -- Scotty.liftAndCatchIO $
        Scotty.liftIO $
            DB.execute conn [sql|insert into todos (todo) values (?);|] (DB.Only todo :: DB.Only Text.Text)

        Scotty.redirect "/"

    Scotty.post "/:id" $ do
        id <- Scotty.captureParam "id"      -- capture is URL
        todo <- Scotty.formParam "todo"  -- form is request body
        Scotty.liftIO $
            DB.executeNamed conn [sql|update todos set todo=:todo where id=:id;|]
                [ ":id" := (id :: Int), ":todo" := (todo :: Text.Text) ]

        Scotty.redirect ("/" <> Text.Lazy.pack (show id))


    Scotty.delete "/:id" $ do
        id <- Scotty.captureParam "id"
        Scotty.liftIO $
            DB.executeNamed conn [sql|delete from todos where id=:id ;|]
                [ ":id" := (id :: Int) ]

    Scotty.get "/:id" $ do
        id <- Scotty.captureParam "id"

        todos <- Scotty.liftIO $
            -- DB.queryNamed conn [sql|select * from todos where id=:id ;|]
            DB.queryNamed conn [sql|select id, todo from todos where id=:id ;|]
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

    -- jsFile <- readFile "app/utils.js"
    -- // JS callback funcs.  Called when a button is clicked.
    -- HTML.script $ HTML.toMarkup jsFile
    HTML.script ! Attributes.type_ "text/javascript" ! Attributes.src "utils.js" $ mempty

data Exc = Exc Text
  deriving (Show, Exception)

get :: Scotty.RoutePattern -> Scotty.ActionM () -> Scotty.ScottyM ()
get p act =
  Scotty.get p
    (act `Exception.catch` (\(e :: SomeException) -> do
                             Scotty.liftIO (print e)
                             Exception.throwIO e))

