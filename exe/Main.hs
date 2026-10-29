module Main (main) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Tap
import Data.Version
import Options.Applicative
import Options.Applicative.NonEmpty
import Paths_tap

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ReadTap fM -> print . runGet (get :: Get Hdr) =<< maybe BS.getContents BS.readFile fM
    WriteTap hdr fM -> maybe BS.putStr BS.writeFile fM $ runPut $ put hdr

data CLI = ReadTap (Maybe FilePath) | WriteTap Hdr (Maybe FilePath)

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "radiotap"

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (fmap ReadTap $
    optional $ fileArg "binary intput file or stream STDIN") $
    progDesc "read radiotap data"
  , command "w" $ info (WriteTap <$> parseHdr <*>
    optional (fileArg "binary output file or stream STDOUT")) $
    progDesc "write radiotap data"]

fileArg :: String -> Parser FilePath
fileArg h = strArgument $ metavar "FILE" <> completer (bashCompleter "file") <> help h

parseHdr :: Parser Hdr
parseHdr = Hdr <$> option auto (long "ver" <> metavar "WORD8" <> help "header version")
  <*> option auto (long "hpd" <> metavar "WORD8" <> help "header pad")
  <*> option auto (long "len" <> metavar "WORD16" <> help "entire length")
  <*> some1 (option auto $ long "prsnt" <> metavar "WORD32" <> help "header present fields")
