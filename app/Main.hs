{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Text.Blaze.Html.Renderer.Text (renderHtml)

data ToDo = ToDo { id :: Int, todo :: Text.Text }
  deriving (Generic, FromRow, Show)

main :: IO ()
main = DB.withConnection "ttadb.db" $ \conn -> do
    -- conn <- DB.open "ttadb.db" -- with conn ( later )

    DB.execute_ conn [sql|create table if not exists todos
                          ( id integer primary key autoincrement
                          , todo text
                          );|] -- between [sql| ... |] is a quasi-quoter, this is SQL not Haskell

    Scotty.scotty 4242 $ do

        Scotty.get "/" $ do
            -- Scotty.html "<h1>hello!</h1>"
            todos <- Scotty.liftAndCatchIO $
                         DB.query_ conn [sql|select id, todo from todos;|] :: Scotty.ActionM [ToDo]
            -- Scotty.html $ "<h1>" <> Text.Lazy.pack (show todos) <> "</h1>"
            Scotty.html $ renderHtml $ HTML.html $ do
                HTML.head $ do
                    HTML.title "Talk to a Database | To-Do's"
                HTML.body $ do
                    HTML.h1 $ HTML.toMarkup $ show todos

        Scotty.post "/" $ do
            todo <- Scotty.param "todo"
            Scotty.liftAndCatchIO $
                DB.execute conn [sql|insert into todos (todo) values (?);|] (DB.Only todo :: DB.Only Text.Text)

            -- Scotty.status Status.status200

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
            todo <- Scotty.liftAndCatchIO $
                DB.queryNamed conn [sql|select * from todos where id=:id;|]
                    [ ":id" := (id :: Int) ] :: Scotty.ActionM [ToDo]
            if null todo
                then
                    Scotty.status Status.status404
                else
                    Scotty.html $ "<h1>" <> Text.Lazy.pack (show todo) <> "</h1>"


    -- DB.close conn