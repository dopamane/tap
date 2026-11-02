module Main (main) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.NonEmpty as NE
import Data.Tap
import Data.Version
import Data.Word
import Options.Applicative
import Paths_tap

main :: IO ()
main = do
  CLI dfn cmd <- customExecParser prefs' $ pinfo $ showVersion version
  d <- either (fail . mappend "definition error: ") return $ fmt dfn
  case cmd of
    ReadTap fM -> print . runGet (getTap d) =<< maybe BS.getContents BS.readFile fM
    WriteTap (v,p) fs fM -> do
      fs' <- either (fail . mappend "field error: ") return $ unk (map num d) =<< fmt fs
      let ps = mkPrsnts $ map num fs'
          hlen = 4 + fromIntegral (NE.length ps) * 4
          slen = sum $ map structLen $ padStructs $ structs =<< fs'
          tlen = hlen + slen
      when (tlen >= fromIntegral (maxBound :: Word16)) $
        fail $ "struct length too long: " <> show tlen
      maybe BS.putStr BS.writeFile fM $ runPut $ putTap $
        Tap (Hdr v p (fromIntegral tlen) ps) fs'

data CLI = CLI [Fld] Cmd

data Cmd = ReadTap (Maybe FilePath)
  | WriteTap (Word8, Word8) [Fld] (Maybe FilePath)

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "radiotap"

parser :: Parser CLI
parser = liftA2 CLI (concat <$> many parseDefn) $ hsubparser $ mconcat
  [ command "r" $ info (ReadTap <$>
    optional (fileArg "binary intput file or stream STDIN")) $
    progDesc "read radiotap data"
  , command "w" $ info (WriteTap <$> parseHdr <*> many parseFld <*>
    optional (fileArg "binary output file or stream STDOUT")) $
    progDesc "write radiotap data"]

fileArg :: String -> Parser FilePath
fileArg h = strArgument $ metavar "FILE" <> completer (bashCompleter "file") <> help h

parseHdr :: Parser (Word8, Word8)
parseHdr = parserOptionGroup "Header" $ (,)
  <$> option auto (long "ver" <> metavar "WORD8" <> value 0 <> showDefault <> help "header version")
  <*> option auto (long "hpd" <> metavar "WORD8" <> value 0 <> showDefault <> help "header pad")

parseFld :: Parser Fld
parseFld = parserOptionGroup "Fields" $ Fld ""
  <$> option auto (short 'f' <> metavar "INT" <> help "field num")
  <*> many parseStructVal

parseDefn :: Parser [Fld]
parseDefn = parserOptionGroup "Definition" $ asum
  [ flag' defFlds $ long "dfs" <> help "radiotap defined fields"
  , fmap pure $ Fld
    <$> strOption (long "name" <> metavar "STR" <> help "field definition name")
    <*> option auto (long "num" <> metavar "INT" <> help "field definition num")
    <*> many parseStruct
  ]

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
  [ fmap U8  $ option auto $ long "u8"  <> metavar "WORD8"  <> help "u8 value"
  , fmap S8  $ option auto $ long "s8"  <> metavar "INT8"   <> help "s8 value"
  , fmap U16 $ option auto $ long "u16" <> metavar "WORD16" <> help "u16 value"
  , fmap S16 $ option auto $ long "s16" <> metavar "INT16"  <> help "s16 value"
  , fmap U32 $ option auto $ long "u32" <> metavar "WORD32" <> help "u32 value"
  , fmap S32 $ option auto $ long "s32" <> metavar "INT32"  <> help "s32 value"
  , fmap U64 $ option auto $ long "u64" <> metavar "WORD64" <> help "u64 value"
  , fmap S64 $ option auto $ long "s64" <> metavar "INT64"  <> help "s64 value"
  ]
