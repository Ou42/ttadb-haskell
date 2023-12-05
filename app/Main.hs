{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude hiding (id)
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

data ToDo = ToDo { id :: Int, todo :: Text.Text }
  deriving (Generic, FromRow, Show)


updateForm1 :: ToDo -> HTML.Html
updateForm1 ToDo {id, todo} =
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
    jsFile <- readFile "app/utils.js"
    cssFile <- readFile "app/main.css"

    DB.withConnection "ttadb.db" $ \conn -> do

        DB.execute_ conn [sql|create table if not exists todos
                            ( id integer primary key autoincrement
                            , todo text
                            );|] -- between [sql| ... |] is a quasi-quoter, this is SQL not Haskell

        Scotty.scotty 4242 $ do

            Scotty.get "/" $ do
                todos <- Scotty.liftAndCatchIO $
                            DB.query_ conn [sql|select id, todo from todos;|] :: Scotty.ActionM [ToDo]

                Scotty.html $ renderHtml $ HTML.docTypeHtml $ do
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
                            for_ todos $ \ToDo {id, todo} -> do
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

            Scotty.post "/" $ do
                todo <- Scotty.param "todo"
                Scotty.liftAndCatchIO $
                    DB.execute conn [sql|insert into todos (todo) values (?);|] (DB.Only todo :: DB.Only Text.Text)

                Scotty.redirect "/"

            Scotty.post "/:id" $ do
                id <- Scotty.param "id"
                todo <- Scotty.param "todo"
                Scotty.liftAndCatchIO $
                    DB.executeNamed conn [sql|update todos set todo=:todo where id=:id;|]
                        [ ":id" := (id :: Int), ":todo" := (todo :: Text.Text) ]

                Scotty.redirect ("/" <> Text.Lazy.pack (show id))


            Scotty.delete "/:id" $ do
                id <- Scotty.param "id"
                Scotty.liftAndCatchIO $
                    DB.executeNamed conn [sql|delete from todos where id=:id ;|]
                        [ ":id" := (id :: Int) ]

            Scotty.get "/:id" $ do
                id <- Scotty.param "id"
                todos <- Scotty.liftAndCatchIO $
                    DB.queryNamed conn [sql|select * from todos where id=:id;|]
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
 