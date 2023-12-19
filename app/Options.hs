module Options where

import qualified Options.Applicative as Opts

data Options = Options { port :: Int }

optionsParser :: Opts.Parser Options
optionsParser =
    Options <$> portParser


getOptions :: IO Options
getOptions = do
    let info = Opts.info ( Opts.helper <*> optionsParser ) mempty
    Opts.execParser info

portParser :: Opts.Parser Int
portParser = Opts.option Opts.auto
  (  Opts.long "port" 
  <> Opts.help "Port to listen on"
  <> Opts.metavar "INT"
  <> Opts.value 4242
  )