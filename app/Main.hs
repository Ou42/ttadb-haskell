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

updateChecklist :: HTML.Html
updateChecklist =
    HTML.ol $ do
        HTML.li $
            HTML.toMarkup (Text.pack "- ☑ Create Form")
        HTML.li $
            HTML.toMarkup (Text.pack "- ☑ update database via PUT")
        HTML.li $
            HTML.toMarkup (Text.pack "- ☑ reload page to show change")
        HTML.li $
            HTML.toMarkup (Text.pack "- [ ] Don't do PUT if no change made")
        HTML.li $
            HTML.toMarkup (Text.pack "- [ ] edit DOM instead of reload page?!")


updateForm :: HTML.Html
updateForm =
    HTML.div ! Attributes.class_ "container" $ do
        HTML.div ! Attributes.class_ "form_container" $ do
            HTML.form ! Attributes.class_ "update_form"
                    --   ! Attributes.action "/edit/:id"
                    --   ! Attributes.method "post"
                      $ do

                -- HTML.p ! Attributes.class_ "prev_todo " $ "Current: " <> "???"
                -- HTML.p ! Attributes.id "prev_todo" $ "Current: " <> "???"
                HTML.p ! Attributes.name "prev_todo" $ "Current: " <> "???"

                HTML.label ! Attributes.for "updatedtodo"
                           $ HTML.toMarkup (Text.pack "Updated:")

                HTML.input ! Attributes.type_ "text"
                           ! Attributes.name "update_id"
                        --    ! Attributes.id "update_id"
                        --    ! Attributes.class_ "update_id"
                           ! Attributes.hidden "true"

                HTML.input ! Attributes.type_ "text"
                           ! Attributes.name "updated_todo"
                           ! Attributes.placeholder "Enter Edited ToDo"

                HTML.button ! Attributes.type_ "button" -- REQUIRED! else it "submits" the form!
                            ! Attributes.onclick "update()"
                            $ "Update"

                -- HTML.input ! Attributes.type_ "submit" -- calls post on "/edit/:id"

updateForm1 :: ToDo -> HTML.Html
updateForm1 ToDo {id, todo} =
    HTML.form ! Attributes.action ("/" <> HTML.toValue id)
              ! Attributes.method "post" $ do
        HTML.p $ HTML.toMarkup $ "Current: " <> todo
        HTML.label ! Attributes.for "todo"
                    $ HTML.toMarkup (Text.pack "Updated:")
        HTML.input ! Attributes.type_ "text" ! Attributes.name "todo"
        HTML.input ! Attributes.type_ "submit" -- calls post on "/:id"

formName :: Show a => a -> String
formName id = "editform" <> show id

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

                        HTML.link ! Attributes.rel "icon"
                                  ! Attributes.href "data:,"

                        HTML.style $ HTML.toMarkup cssFile

                        HTML.script $ do
                            -- // JS funcs called when buttons clicked
                            HTML.toMarkup jsFile

                    HTML.body $ do
                        HTML.h1 "To-Do's"

                        updateForm -- can't send in a value?!

                        HTML.hr

                        HTML.ul $ do
                            for_ todos $ \ToDo {id, todo} -> do
                                -- HTML.li ! Attributes.name (HTML.toValue ("todo: " <> show id)) $ do
                                HTML.li $ do
                                    HTML.div ! Attributes.class_ "flex-container" $ do

                                        HTML.a ! Attributes.name (HTML.toValue ("todo: " <> show id))
                                               ! Attributes.href ("/" <> HTML.toValue id)
                                               $ HTML.toMarkup todo

                                        -- old idea: to show the form under the item in the list
                                        HTML.p ! Attributes.id (HTML.toValue (formName id))
                                               ! Attributes.style "display: none"
                                               $ "booyah!"

                                        HTML.button ! Attributes.value (HTML.toValue id)
                                                    ! Attributes.onclick "deleteToDo(this)"
                                                    $ "delete"
                                                    -- "delete" -- "delete id:" <> HTML.toMarkup id

                                        HTML.button ! Attributes.value (HTML.toValue id)
                                                    ! Attributes.onclick "updateToDo(this)"
                                                    $ "update"
                                                    -- "update id:" <> HTML.toMarkup id

                                        HTML.button ! Attributes.value (HTML.toValue id)
                                                    -- ! Attributes.onclick "toggleVisUpdateForm(this)" $ do
                                                    ! Attributes.onclick "edit(this)"
                                                    $ "update on page (id: " <> HTML.toMarkup id <> ")"

                        HTML.form ! Attributes.action "/" ! Attributes.method "post" $ do
                            HTML.input ! Attributes.type_ "text" ! Attributes.name "todo"
                            HTML.input ! Attributes.type_ "submit" -- calls post on "/"

                        updateChecklist

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
                        -- Scotty.html $ "<h1>" <> Text.Lazy.pack (show todo) <> "</h1>"
                        Scotty.html $ renderHtml $ HTML.html $ do
                                        HTML.head $ do
                                            HTML.title $ mapM_ (HTML.toMarkup . todo) todos
                                        HTML.body $ do
                                            HTML.h1 $ mapM_ (HTML.toMarkup . todo) todos

            Scotty.put "/:id" $ do
                id <- Scotty.param "id"
                todo <- Scotty.param "todo"
                Scotty.liftAndCatchIO $
                    DB.executeNamed conn [sql|update todos set todo=:todo where id=:id;|]
                        [ ":id" := (id :: Int), ":todo" := (todo :: Text.Text) ]

            Scotty.get "/edit/:id" $ do
                id <- Scotty.param "id"
                todos <- Scotty.liftAndCatchIO $
                    DB.queryNamed conn [sql|select * from todos where id=:id;|]
                        [ ":id" := (id :: Int) ] :: Scotty.ActionM [ToDo]
                if null todos
                    then
                        Scotty.status Status.status404
                    else
                        Scotty.html $ renderHtml $ HTML.html $ do
                            HTML.head $ do
                                HTML.title $ mapM_ (HTML.toMarkup . todo) todos
                            HTML.body $ do
                                HTML.h1 $ HTML.toMarkup (Text.pack "Editing: ")
                                        <> mapM_ (HTML.toMarkup . todo) todos

                                updateChecklist

                                HTML.hr

                                mapM_ updateForm1 todos
