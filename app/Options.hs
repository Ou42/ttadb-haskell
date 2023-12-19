module Options where

import qualified Options.Applicative as Opts

data Options = Options { port :: Int
                       , db   :: FilePath
                       }

optionsParser :: Opts.Parser Options
optionsParser =
    Options <$> portParser <*> dbParser

getOptions :: IO Options
getOptions = do
    let info  = Opts.info ( Opts.helper <*> optionsParser ) ( Opts.progDesc "To-Do List that talks to a Database")
    let prefs = Opts.prefs ( Opts.showHelpOnError <> Opts.showHelpOnEmpty )

    Opts.customExecParser prefs info

portParser :: Opts.Parser Int
portParser = Opts.option Opts.auto
  (  Opts.long "port" 
  <> Opts.help "Port to listen on"
  <> Opts.metavar "INT"
  <> Opts.value 4242
  )

dbParser :: Opts.Parser FilePath
dbParser = Opts.strOption
  (  Opts.long "db" 
  <> Opts.help "Path to database (ex: ttadb.db)"
  <> Opts.metavar "PATH"
  )
