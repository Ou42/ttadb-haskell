module Options where

import qualified Options.Applicative as Opts
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

data Options = Options { port :: Int
                       , db   :: FilePath
                       , staticDir :: FilePath
                       , reqLogger :: Wai.Middleware
                       }

optionsParser :: Opts.Parser Options
optionsParser =
    Options <$> portParser <*> dbParser <*> staticDirParser <*> reqLoggerParser

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

staticDirParser :: Opts.Parser FilePath
staticDirParser = Opts.strOption
  (  Opts.long "static-dir" 
  <> Opts.help "Path to static directory (js,css,etc)"
  <> Opts.metavar "PATH"
  )

reqLoggerParser :: Opts.Parser Wai.Middleware
reqLoggerParser = Opts.option ( Opts.eitherReader readReqLogger )
  (  Opts.long "req-logger" 
  <> Opts.help "Logging style: [dev|apache]"
  <> Opts.metavar "LOGGER"
  <> Opts.value RequestLogger.logStdout -- def should always be production!
  )

readReqLogger :: String -> Either String Wai.Middleware
readReqLogger logStyleStr =
  case logStyleStr of
    "dev" -> Right RequestLogger.logStdoutDev
    "apache" -> Right RequestLogger.logStdout
    _ -> Left ("Unexpected log style. Expected [dev|apache], got: " <> logStyleStr)

