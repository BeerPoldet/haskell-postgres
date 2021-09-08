{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.Migration 
  (migrate)
  where

import           Control.Monad                        (void)
import           Database.PostgreSQL.Simple           (ConnectInfo (..),
                                                       connect,
                                                       defaultConnectInfo,
                                                       withTransaction, Connection)
import Database.PostgreSQL.Simple.Types (fromQuery)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationCommands, MigrationInitialization, MigrationScript),
                                                       MigrationContext (MigrationContext),
                                                       runMigration)

migrate :: Connection -> IO ()
migrate connection =
  withTransaction connection $ void $ runMigration $
    MigrationContext migrations True connection
  where migrations :: MigrationCommand
        migrations = MigrationCommands
          [ MigrationInitialization
          , createMovieTable
          , addCreatedAtOnMovieTable
          , useSerialAsPrimaryKeyOnMovieTable
          , bootstrapMoviesTable
          ]

createMovieTable :: MigrationCommand
createMovieTable = MigrationScript 
  "create movie table"
  $ fromQuery 
  [sql|
    create table movies(
      id int primary key not null,
      name text not null,
      description text not null
    )
  |]

addCreatedAtOnMovieTable :: MigrationCommand
addCreatedAtOnMovieTable = MigrationScript 
  "add created_at on movie table"
  $ fromQuery 
  [sql|
    alter table movies
      add column created_at timestamp without time zone not null default (now() at time zone 'utc')
  |]

useSerialAsPrimaryKeyOnMovieTable :: MigrationCommand
useSerialAsPrimaryKeyOnMovieTable = MigrationScript
  "alter serial as type for movie table primary key"
  $ fromQuery
  [sql|
    create sequence movies_serial as int start 1 owned by movies.id;
    alter table movies
      alter column id set default nextval('movies_serial');
  |]

bootstrapMoviesTable :: MigrationCommand
bootstrapMoviesTable = MigrationScript
  "bootstrap initial movies data to movie table"
  $ fromQuery
  [sql|
    insert into movies (name, description)
    values
      ('My Hero Academia', 'My Hero description'),
      ('The Witcher', 'The Witcher description');
  |]

