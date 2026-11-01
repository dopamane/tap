module Main (main) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Tap
import Data.Version
import Options.Applicative
import Options.Applicative.NonEmpty
import Paths_tap

main :: IO ()
main = do
  CLI dfn cmd <- customExecParser prefs' $ pinfo $ showVersion version
  d <- either (fail . mappend "definition error: ") return $ fmt dfn
  case cmd of
    ReadTap fM -> print . runGet (getTap d) =<< maybe BS.getContents BS.readFile fM
    WriteTap t fM -> do
      f <- either (fail . mappend "field error: ") return $ unk (map num d) =<< fmt (fields t)
      maybe BS.putStr BS.writeFile fM $ runPut $ putTap t{fields=f}

fmt :: [Fld] -> Either String [Fld]
fmt = go . NE.groupWith num . sortOn num
  where
    go [] = Right []
    go ((f:|[]):gs) = (f:) <$> go gs
    go ((f:|fs):_) = Left $ "duplicate numbered fields: " ++ show (f:fs)

unk :: [Int] -> [Fld] -> Either String [Fld]
unk ds = go
  where
    go [] = Right []
    go (f:fs)
      | num f `elem` ds = (f:) <$> go fs
      | otherwise = Left $ "undefined field: " ++ show f

data CLI = CLI [Fld] Cmd

data Cmd = ReadTap (Maybe FilePath) | WriteTap Tap (Maybe FilePath)

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "radiotap"

parser :: Parser CLI
parser = liftA2 CLI parseDefn $ hsubparser $ mconcat
  [ command "r" $ info (ReadTap <$>
    optional (fileArg "binary intput file or stream STDIN")) $
    progDesc "read radiotap data"
  , command "w" $ info (WriteTap <$> parseTap <*>
    optional (fileArg "binary output file or stream STDOUT")) $
    progDesc "write radiotap data"]

fileArg :: String -> Parser FilePath
fileArg h = strArgument $ metavar "FILE" <> completer (bashCompleter "file") <> help h

parseTap :: Parser Tap
parseTap = Tap <$> parseHdr <*> many parseFld

parseHdr :: Parser Hdr
parseHdr = parserOptionGroup "Header" $
  Hdr <$> option auto (long "ver" <> metavar "WORD8" <> help "header version")
  <*> option auto (long "hpd" <> metavar "WORD8" <> help "header pad")
  <*> option auto (long "len" <> metavar "WORD16" <> help "entire length")
  <*> some1 (option auto $ long "prsnt" <> metavar "WORD32" <> help "header present fields")

parseFld :: Parser Fld
parseFld = parserOptionGroup "Fields" $ asum
  [ fmap (Fld "TSFT" 0 . pure . U64) $ option auto $ long "tsft" <> metavar "WORD64" <>
    help "time sync function timer usec"
  , fmap (Fld "Flags" 1 . pure . U8) $ option auto $ long "flags" <> metavar "WORD8" <>
    help "properties of tx and rx frames"
  , Fld "" <$> option auto (long "fld" <> metavar "INT" <> help "field num")
    <*> many parseStructVal
  ]

parseDefn :: Parser [Fld]
parseDefn = parserOptionGroup "Definition" $ asum
  [ flag [] [] $ long "empty" <> help "empty field definition default"
  , flag' defFlds $ long "dfs" <> help "radiotap defined fields"
  , some parseFldDef
  ]

parseFldDef :: Parser Fld
parseFldDef = Fld
  <$> strOption (long "name" <> metavar "STR" <> help "field definition name")
  <*> option auto (long "num" <> metavar "INT" <> help "field definition num")
  <*> many parseStruct

parseStruct :: Parser Struct
parseStruct = asum
  [ flag' (U8  0) $ long "du8"  <> help "u8 field definition"
  , flag' (S8  0) $ long "ds8"  <> help "s8 field definition"
  , flag' (U16 0) $ long "du16" <> help "u16 field definition"
  , flag' (S16 0) $ long "ds16" <> help "s16 field definition"
  , flag' (U32 0) $ long "du32" <> help "u32 field definition"
  , flag' (S32 0) $ long "ds32" <> help "s32 field definition"
  , flag' (U64 0) $ long "du64" <> help "u64 field definition"
  , flag' (S64 0) $ long "ds64" <> help "s64 field definition"
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
