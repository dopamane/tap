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
    WriteTap _ h fM -> maybe BS.putStr BS.writeFile fM $ runPut $ put h

data CLI = ReadTap (Maybe FilePath) | WriteTap [Field] Tap (Maybe FilePath)

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "radiotap"

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (fmap ReadTap $
    optional $ fileArg "binary intput file or stream STDIN") $
    progDesc "read radiotap data"
  , command "w" $ info (WriteTap <$> parseDefn <*> parseTap <*>
    optional (fileArg "binary output file or stream STDOUT")) $
    progDesc "write radiotap data"]

fileArg :: String -> Parser FilePath
fileArg h = strArgument $ metavar "FILE" <> completer (bashCompleter "file") <> help h

parseTap :: Parser Tap
parseTap = Tap <$> parseHdr <*> many parseField

parseHdr :: Parser Hdr
parseHdr = parserOptionGroup "Header" $
  Hdr <$> option auto (long "ver" <> metavar "WORD8" <> help "header version")
  <*> option auto (long "hpd" <> metavar "WORD8" <> help "header pad")
  <*> option auto (long "len" <> metavar "WORD16" <> help "entire length")
  <*> some1 (option auto $ long "prsnt" <> metavar "WORD32" <> help "header present fields")

parseField :: Parser Field
parseField = parserOptionGroup "Fields" $ asum
  [ fmap (Field "TSFT" 0 . pure . U64) $ option auto $ long "tsft" <> metavar "WORD64" <>
    help "time sync function timer usec"
  , fmap (Field "Flags" 1 . pure . U8) $ option auto $ long "flags" <> metavar "WORD8" <>
    help "properties of tx and rx frames"
  , Field "" <$> option auto (long "field" <> metavar "INT" <> help "field num")
    <*> many parseStructVal

  ]

parseDefn :: Parser [Field]
parseDefn = parserOptionGroup "Definition" $ asum
  [ flag' definedFields $ long "def-fields" <> help "radiotap defined fields"
  , some parseFieldDef
  ]

parseFieldDef :: Parser Field
parseFieldDef = Field
  <$> strOption (long "def-name" <> metavar "STR" <> help "field definition name")
  <*> option auto (long "def-num" <> metavar "INT" <> help "field definition num")
  <*> many parseStruct

parseStruct :: Parser Struct
parseStruct = asum
  [ flag' (U8  0) $ long "def-u8"  <> help "u8 field definition"
  , flag' (S8  0) $ long "def-s8"  <> help "s8 field definition"
  , flag' (U16 0) $ long "def-u16" <> help "u16 field definition"
  , flag' (S16 0) $ long "def-s16" <> help "s16 field definition"
  , flag' (U32 0) $ long "def-u32" <> help "u32 field definition"
  , flag' (S32 0) $ long "def-s32" <> help "s32 field definition"
  , flag' (U64 0) $ long "def-u64" <> help "u64 field definition"
  , flag' (S64 0) $ long "def-s64" <> help "s64 field definition"
  ]

parseStructVal :: Parser Struct
parseStructVal = asum
  [ option auto $ long "u8"  <> metavar "WORD8"  <> help "u8 value"
  , option auto $ long "s8"  <> metavar "INT8"   <> help "s8 value"
  , option auto $ long "u16" <> metavar "WORD16" <> help "u16 value"
  , option auto $ long "s16" <> metavar "INT16"  <> help "s16 value"
  , option auto $ long "u32" <> metavar "WORD32" <> help "u32 value"
  , option auto $ long "s32" <> metavar "INT32"  <> help "s32 value"
  , option auto $ long "u64" <> metavar "WORD64" <> help "u64 value"
  , option auto $ long "s64" <> metavar "INT64"  <> help "s64 value"
  ]
