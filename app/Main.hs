{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Text as Text
import qualified Web.Scotty as Scotty
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import qualified Data.Text.Internal.Builder as Text
import qualified Data.Text.Lazy as Text.Lazy
import Database.SQLite.Simple.FromRow (FromRow)
import GHC.Generics (Generic)
import qualified Network.HTTP.Types.Status as Status

data Greeting = Greeting { id :: Int, greeting :: Text.Text }
  deriving (Generic, FromRow, Show)

main :: IO ()
main = DB.withConnection "ttadb.db" $ \conn -> do
    -- conn <- DB.open "ttadb.db" -- with conn ( later )

    DB.execute_ conn [sql|create table if not exists greetings
                          ( id integer primary key autoincrement
                          , greeting text
                          );|]

    Scotty.scotty 4242 $ do

        Scotty.get "/" $ do
            -- Scotty.html "<h1>hello!</h1>"
            greetings <- Scotty.liftAndCatchIO $
                         DB.query_ conn [sql|select id, greeting from greetings;|] :: Scotty.ActionM [Greeting]
            Scotty.html $ "<h1>" <> Text.Lazy.pack (show greetings) <> "</h1>"

        Scotty.post "/" $ do
            greeting <- Scotty.param "greeting"
            Scotty.liftAndCatchIO $
                        DB.execute conn [sql|insert into greetings (greeting) values (?);|] (DB.Only greeting :: DB.Only Text.Text)

            Scotty.status Status.status200

    -- DB.close conn