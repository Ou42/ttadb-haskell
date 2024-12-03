{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Migration where

import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple qualified as DB
import Database.SQLite.Simple (NamedParam(..))
import Data.Foldable (for_, find)

run :: DB.Connection -> IO ()
run conn = do
  DB.execute_ conn [sql|create table if not exists migration_checkpoint
                        ( checkpoint_number INTEGER not null default (0)
                        );|]
                      
  let target = maximum $ fmap fst migrations
  c <- DB.query_ conn [sql|select checkpoint_number from migration_checkpoint;|]

  current <-
    case c of
      [DB.Only n] -> pure n
      [] -> do
        DB.execute_ conn [sql|insert into migration_checkpoint default values|]
        pure 0

  for_ [current+1..target] $ \n -> do
    let m = find (\(i, _) -> n == i) migrations
    for_ m $ \(_, migration) -> do
      putStrLn $ "running migration " <> show n

      migration conn
      DB.executeNamed conn [sql|update migration_checkpoint 
                                set checkpoint_number=:n;|]
                           [":n" := n]

migrations :: [(Int, DB.Connection -> IO ())]
migrations = [ (1, createToDos)
             , (2, createUsers)
             , (3, dropManualTables)
             , (4, dropManualTables2)
             , (5, dropExtraUsersCols)
             ]

createToDos :: DB.Connection -> IO ()
createToDos conn = do
  DB.execute_ conn [sql|create table if not exists todos
                        ( id INTEGER primary key autoincrement
                        , todo TEXT
                        , done_date TEXT -- can add a check constraint: date time fmt
                        );|] -- between [sql| ... |] is a quasi-quoter, this is SQL not Haskell

createUsers :: DB.Connection -> IO ()
createUsers conn = do
  DB.execute_ conn [sql|create table if not exists users
                        ( user_id INTEGER PRIMARY KEY AUTOINCREMENT,
                          name TEXT NOT NULL,
                          password_hash TEXT NOT NULL,
                          authorized BOOLEAN,
                          authorized_date DATETIME,
                          last_login DATETIME );|]

dropManualTables :: DB.Connection -> IO ()
dropManualTables conn = do
  DB.execute_ conn [sql|drop table if exists old_users;|]

dropManualTables2 :: DB.Connection -> IO ()
dropManualTables2 conn = DB.withTransaction conn $ do
  DB.execute_ conn [sql|drop table if exists new_todos;|]
  DB.execute_ conn [sql|drop table if exists greetings;|]

dropExtraUsersCols :: DB.Connection -> IO ()
dropExtraUsersCols conn = DB.withTransaction conn $ do
  DB.execute_ conn [sql|alter table users drop column authorized;|]
  DB.execute_ conn [sql|alter table users drop column last_login;|]


