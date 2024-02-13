{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (Exception(..))
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Data.String (fromString)
import Data.Typeable ( Typeable )
import qualified Network.Wai.Middleware.RequestLogger as MW
import GHC.Stack (HasCallStack)

import qualified Web.Scotty.Trans as ScottyT

import Prelude hiding (id)
import qualified Prelude

import qualified Data.Text as Text
import qualified Web.Scotty as Scotty
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple.QQ (sql)
import qualified Data.Text.Internal.Builder as Text
import qualified Data.Text.Lazy as Text.Lazy
import Database.SQLite.Simple.FromRow (FromRow)
import GHC.Generics (Generic)
import qualified Network.HTTP.Types.Status as Status
import qualified Text.Blaze.Html
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attributes
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Foldable (for_)
import qualified Options
import Options (Options(..))
import Data.Time.Clock (UTCTime)
-- import qualified Web.Scotty.Internal.Types as ScottyT

data ToDo = ToDo { id :: Int
                 , todo :: Text.Text
                --  , done :: Bool -- *will* need to remove the column. make a temp table (copy), remove col, save temp table back as the primary table
                 , done_date :: Maybe UTCTime -- Nothing == "not done", use FromField UTCTime <https://hackage.haskell.org/package/sqlite-simple-0.4.18.2/docs/Database-SQLite-Simple-FromField.html#t:FromField>
                 }
  deriving (Generic, FromRow, Show)

-- | A custom exception type.
data Except = Forbidden | NotFound Int | StringEx String
    deriving (Show, Eq, Typeable, Exception)

-- Also, convert to use Blaze-it (TM, Danny)
-- | User-defined exceptions should have an associated Handler:
handleEx :: MonadIO m => ScottyT.ErrorHandler m
handleEx = ScottyT.Handler $ \case
  Forbidden -> do
    ScottyT.status Status.status403
    ScottyT.html "<h1>Scotty Says No</h1>"
  NotFound i -> do
    ScottyT.status Status.status404
    ScottyT.html $ fromString $ "<h1>Can't find " ++ show i ++ ".</h1>"
  StringEx s -> do
    ScottyT.status Status.status500
    ScottyT.html $ fromString $ "<h1>" ++ s ++ "</h1>"

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
updateForm1 ToDo {id, todo, done} =
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

main :: IO ()
main = do
    Options.Options { port, db } <- Options.getOptions

    jsFile <- readFile "app/utils.js"
    cssFile <- readFile "app/main.css"

    DB.withConnection db $ \conn -> do

        DB.execute_ conn [sql|create table if not exists todos
                            ( id INTEGER primary key autoincrement
                            , todo TEXT
                            -- , done BOOLEAN default FALSE
                            , done_date TEXT -- can add a check constraint: date time fmt
                            );|] -- between [sql| ... |] is a quasi-quoter, this is SQL not Haskell

        ScottyT.scottyT port Prelude.id (server conn jsFile cssFile) -- note: we use 'id' since we don't have to run any effects at each action

-- Any custom monad stack will need to implement 'MonadUnliftIO'
-- server :: MonadUnliftIO m => ScottyT.ScottyT m ()
server :: (HTML.ToMarkup a1, HTML.ToMarkup a2, HasCallStack)
            => DB.Connection -> a2 -> a1 -> ScottyT.ScottyT IO ()
server conn jsFile cssFile = do
    ScottyT.middleware MW.logStdoutDev

    ScottyT.defaultHandler handleEx    -- define what to do with uncaught exceptions

    ScottyT.get "/" $ do

        todos <- ScottyT.liftIO $
                    DB.query_ conn [sql|select id, todo, done_date from todos;|] :: Scotty.ActionM [ToDo]

        -- deprecated: ScottyT.raise $ fromString $ "ummm something something + " <> show todos
        -- ScottyT.throw $ StringEx $ "ummm something something + " <> show todos

        ScottyT.html $ renderHtml $ HTML.docTypeHtml $ do
            HTML.head $ do
                HTML.title "Talk to a Database | To-Do's"

                noFavIcon

                HTML.style $ HTML.toMarkup cssFile

                HTML.script $ do
                    -- // JS funcs called when buttons clicked
                    HTML.toMarkup jsFile

            HTML.body $ do
                HTML.h1 "To-Do's"

                HTML.ul $ do
                    -- for_ todos $ \ToDo {id, todo} -> do
                    for_ todos $ \ToDo {id, todo, done} -> do
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



    ScottyT.get "/admin" $ do
        todos <- ScottyT.liftIO $
                    DB.query_ conn [sql|select id, todo, done from todos;|] :: Scotty.ActionM [ToDo]
                    -- DB.query_ conn [sql|select id, todo from todos;|] :: Scotty.ActionM [ToDo]

        ScottyT.html $ renderHtml $ HTML.docTypeHtml $ do
            HTML.head $ do
                HTML.title "<< Admin Console >> Talk to a Database | To-Do's << Admin Console >>"

                noFavIcon

                HTML.style $ HTML.toMarkup cssFile

                HTML.script $ do
                    -- // JS funcs called when buttons clicked
                    HTML.toMarkup jsFile

            HTML.body $ do
                HTML.h1 "To-Do's"

                HTML.ul $ do
                    for_ todos $ \ToDo {id, todo, done} -> do
                        HTML.li ! Attributes.id ("todo-" <> HTML.toValue id) $ do
                            HTML.div ! Attributes.class_ "flex-container" $ do

                                HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                        ! Attributes.href ("/" <> HTML.toValue id)
                                        $ HTML.toMarkup todo

                                HTML.p $ HTML.span (if done then "checked" else "!checkd")

                                checkbox done id

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
                    HTML.head $ do
                        HTML.title $ mapM_ (HTML.toMarkup . todo) todos

                        noFavIcon

                    HTML.body $ do
                        HTML.h1 $ HTML.toMarkup (Text.pack "Editing: ")
                                <> mapM_ (HTML.toMarkup . todo) todos

                        mapM_ updateForm1 todos
