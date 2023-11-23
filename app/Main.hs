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

main :: IO ()
main = DB.withConnection "ttadb.db" $ \conn -> do

    DB.execute_ conn [sql|create table if not exists todos
                          ( id integer primary key autoincrement
                          , todo text
                          );|] -- between [sql| ... |] is a quasi-quoter, this is SQL not Haskell

    Scotty.scotty 4242 $ do

        Scotty.get "/" $ do
            todos <- Scotty.liftAndCatchIO $
                         DB.query_ conn [sql|select id, todo from todos;|] :: Scotty.ActionM [ToDo]

            Scotty.html $ renderHtml $ HTML.html $ do
                HTML.head $ do
                    HTML.title "Talk to a Database | To-Do's"

                    HTML.style $ do
                        "a { text-decoration: none; color: white; }\
                        \li { background-color: cornflowerblue; }\
                        \.flex-container { display: flex; }\
                        \.flex-container a { flex: 0 0 33%; background-color: lightslategray; }\
                        \.flex-container button { align-self: flex-start; }"

                    HTML.script $ do
                        "const deleteToDo = b => { fetch(`/${b.value}`, { method: 'DELETE' })\
                        \ .then(r => b.parentElement.remove()); };\
                        \const updateToDo = b => { window.location.href = `/edit/${b.value}`; } // Simulate a mouse click"
                        -- \const updateToDo = b => { fetch(`/edit/${b.value}`, { method: 'GET' }) }" -- \
                        -- \ .then( console.log('Hello!'); ); }"

                HTML.body $ do
                    HTML.h1 "To-Do's"
                    HTML.ul $ do
                      for_ todos $ \ToDo {id, todo} -> do
                        HTML.li $ do
                            HTML.div ! Attributes.class_ "flex-container" $ do
                                HTML.a ! Attributes.href ("/" <> HTML.toValue id) $ do
                                        HTML.toMarkup todo
                                HTML.button ! Attributes.type_ "button"
                                            ! Attributes.value (HTML.toValue id)
                                            ! Attributes.onclick "deleteToDo(this)" $ do
                                    "delete id:" <> HTML.toMarkup id
                                HTML.button ! Attributes.type_ "button"
                                            ! Attributes.value (HTML.toValue id)
                                            ! Attributes.onclick "updateToDo(this)" $ do
                                    "update id:" <> HTML.toMarkup id

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

        Scotty.get "/edit/:id" $ do
            id <- Scotty.param "id"
            todos <- Scotty.liftAndCatchIO $
                DB.queryNamed conn [sql|select * from todos where id=:id;|]
                    [ ":id" := (id :: Int) ] :: Scotty.ActionM [ToDo]
            let currenttodo = concatMap (show . todo) todos
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
                                        HTML.ol $ do
                                            HTML.li $ do
                                                HTML.toMarkup (Text.pack "- ☑ Create Form")
                                            HTML.li $ do
                                                HTML.toMarkup (Text.pack "- [ ] 'on submit', Check if change made to item")
                                            HTML.li $ do
                                                HTML.toMarkup (Text.pack "- [ ] If Change made, update database")

                                        HTML.hr

                                        HTML.form ! Attributes.action "/edit/:id" ! Attributes.method "post" $ do
                                            HTML.label ! Attributes.for "currenttodo" $ HTML.toMarkup (Text.pack "Current:")
                                            HTML.input ! Attributes.type_ "text"
                                                       ! Attributes.disabled "disabled"
                                                       ! Attributes.value (HTML.toValue currenttodo)
                                                       ! Attributes.name "currenttodo"
                                            HTML.label ! Attributes.for "updatedtodo" $ HTML.toMarkup (Text.pack "Updated:")
                                            HTML.input ! Attributes.type_ "text" ! Attributes.name "updatedtodo"
                                            HTML.input ! Attributes.type_ "submit" -- calls post on "/edit/:id"

    -- DB.close conn