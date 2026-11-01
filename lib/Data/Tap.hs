module Data.Tap (Tap(..), getTap, putTap, Hdr(..), Field(..), Struct(..)
  , definedFields, prsntNums) where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Int
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup

data Tap = Tap{hdr :: Hdr, fields :: [Field]}
  deriving (Eq, Read, Show)

getTap :: [Field] -> Get Tap
getTap dfn = get >>= \h -> Tap h <$> getFields dfn h

putTap :: Tap -> Put
putTap (Tap h fs) = put h <> putStructs (structs =<< fs)

getFields :: [Field] -> Hdr -> Get [Field]
getFields dfn Hdr{prsnt=p:|ps} = do
  prsntStructs <- forM nums $ \n -> case lookupField dfn n of
    Nothing -> fail $ "present structs: undefined field num: " <> show n
    Just  f -> return $ structs f
  either fail return . elaborate dfn nums =<< getStructs (concat prsntStructs)
  where
    nums = prsntNums $ p:ps

elaborate :: [Field] -> [Int] -> [Struct] -> Either String [Field]
elaborate dfn = go
  where
    go [] [] = Right []
    go [] s@(_:_) = Left $ "elaborate: too many structs: " <> show s
    go ns@(_:_) [] = Left $ "elaborate: too many present: " <> show ns
    go (n:ns) strcts = case lookupField dfn n of
      Nothing -> Left $ "elaborate: undefined field num: " <> show n
      Just f -> case consume f strcts of
        Left err -> Left err
        Right (f', rest) -> (f':) <$> go ns rest

consume :: Field -> [Struct] -> Either String (Field, [Struct])
consume f@Field{structs=ds} strcts = case match ds strcts of
  Left err -> Left err
  Right (t, rest) -> Right (f{structs=t}, rest)

match :: [Struct] -> [Struct] -> Either String ([Struct], [Struct])
match [] rest = Right ([], rest)
match _ [] = Left "match: not enough structs"
match (d:ds) (s:rest) = case matchStruct d s of
  Left err -> Left err
  Right s' -> case match ds rest of
    Left err -> Left err
    Right (ms, rest') -> Right (s':ms, rest')

matchStruct :: Struct -> Struct -> Either String Struct
matchStruct dfn strct = case (dfn, strct) of
  (U8{},  U8  u8)  -> Right $ U8  u8
  (S8{},  S8  s8)  -> Right $ S8  s8
  (U16{}, U16 u16) -> Right $ U16 u16
  (S16{}, S16 s16) -> Right $ S16 s16
  (U32{}, U32 u32) -> Right $ U32 u32
  (S32{}, S32 s32) -> Right $ S32 s32
  (U64{}, U64 u64) -> Right $ U64 u64
  (S64{}, S64 s64) -> Right $ S64 s64
  (expct, actual)  -> Left $ "expected " ++ renderExp expct ++ " but got " ++ renderAct actual
  where
    renderExp e = case e of
      U8{}  -> "u8"
      S8{}  -> "s8"
      U16{} -> "u16"
      S16{} -> "s16"
      U32{} -> "u32"
      S32{} -> "s32"
      U64{} -> "u64"
      S64{} -> "s64"
    renderAct e = case e of
      U8  u8  -> "u8 "  <> show u8
      S8  s8  -> "s8 "  <> show s8
      U16 u16 -> "u16 " <> show u16
      S16 s16 -> "s16 " <> show s16
      U32 u32 -> "u32 " <> show u32
      S32 s32 -> "s32 " <> show s32
      U64 u64 -> "u64 " <> show u64
      S64 s64 -> "s64 " <> show s64

lookupField :: [Field] -> Int -> Maybe Field
lookupField dfn n = find ((n ==).num) dfn

prsntNums :: [Word32] -> [Int]
prsntNums = concat . zipWith f [0..]
  where
    f i w = [b + i * 32 | b <- [0..30], testBit w b]

data Hdr = Hdr{ver :: Word8, hpd :: Word8, len :: Word16, prsnt :: NonEmpty Word32}
  deriving (Eq, Read, Show)

instance Binary Hdr where
  get = Hdr <$> getWord8 <*> getWord8 <*> getWord16le <*> getPrsnt
    where
      getPrsnt = do
        p <- getWord32le
        if testBit p 31 then (p:|) <$> go else return $ p:|[]
        where
          go = do
            p <- getWord32le
            if testBit p 31 then (p:) <$> go else return [p]
  put h = do
    putWord8 (ver h) >> putWord8 (hpd h) >> putWord16le (len h)
    let p:|ps = prsnt h
    putWord32le p >> if testBit p 31 then go ps else mempty
    where
      go [] = mempty
      go (p:ps) = putWord32le p <> if testBit p 31 then go ps else mempty

data Field = Field{name :: String, num :: Int, structs :: [Struct]}
  deriving (Eq, Read, Show)

definedFields :: [Field]
definedFields =
  [ Field "TSFT" 0 [U64 0]
  , Field "Flags" 1 [U8 0]
  ]

data Struct = U8 Word8 | S8 Int8 | U16 Word16 | S16 Int16 | U32 Word32
  | S32 Int32 | U64 Word64 | S64 Int64
  deriving (Eq, Read, Show)

getStructs :: [Struct] -> Get [Struct]
getStructs [] = return []
getStructs [s] = pure <$> getStruct s
getStructs (a:b:s) = (:) <$> getStruct a <* replicateM_ (pad a b) getWord8 <*> getStructs (b:s)

getStruct :: Struct -> Get Struct
getStruct s = case s of
  U8{}  -> U8  <$> getWord8
  S8{}  -> S8  <$> getInt8
  U16{} -> U16 <$> getWord16le
  S16{} -> S16 <$> getInt16le
  U32{} -> U32 <$> getWord32le
  S32{} -> S32 <$> getInt32le
  U64{} -> U64 <$> getWord64le
  S64{} -> S64 <$> getInt64le

putStructs :: [Struct] -> Put
putStructs [] = mempty
putStructs [s] = putStruct s
putStructs (a:b:s) = putStruct a <> stimes (pad a b) (putWord8 0) <> putStructs (b:s)

putStruct :: Struct -> Put
putStruct s = case s of
  U8  w8  -> putWord8 w8
  S8  s8  -> putInt8  s8
  U16 w16 -> putWord16le w16
  S16 s16 -> putInt16le  s16
  U32 w32 -> putWord32le w32
  S32 s32 -> putInt32le  s32
  U64 w64 -> putWord64le w64
  S64 s64 -> putInt64le  s64

pad :: Struct -> Struct -> Int
pad a b = case (a,b) of
  (U8{}, U16{}) -> 1
  (U8{}, S16{}) -> 1
  (U8{}, U32{}) -> 3
  (U8{}, S32{}) -> 3
  (U8{}, U64{}) -> 7
  (U8{}, S64{}) -> 7
  (S8{}, U16{}) -> 1
  (S8{}, S16{}) -> 1
  (S8{}, U32{}) -> 3
  (S8{}, S32{}) -> 3
  (S8{}, U64{}) -> 7
  (S8{}, S64{}) -> 7
  (U16{},U32{}) -> 2
  (U16{},S32{}) -> 2
  (U16{},U64{}) -> 6
  (U16{},S64{}) -> 6
  (S16{},U32{}) -> 2
  (S16{},S32{}) -> 2
  (S16{},U64{}) -> 6
  (S16{},S64{}) -> 6
  (U32{},U64{}) -> 4
  (U32{},S64{}) -> 4
  (S32{},U64{}) -> 4
  (S32{},S64{}) -> 4
  (_    ,_    ) -> 0
