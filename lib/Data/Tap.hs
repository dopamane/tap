module Data.Tap (Tap(..), Hdr(..), Field(..), Struct(..), definedFields) where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup

data Tap = Tap{hdr :: Hdr, fields :: [Field]}
  deriving (Eq, Read, Show)

instance Binary Tap where
  get = undefined
  put (Tap h fs) = put h >> putFields fs

putFields :: [Field] -> Put
putFields = undefined

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
getStructs [s] = pure <$> case s of
  U8{} -> U8 <$> getWord8
  S8{} -> S8 <$> getInt8
  U16{} -> U16 <$> getWord16le
  S16{} -> S16 <$> getInt16le
  U32{} -> U32 <$> getWord32le
  S32{} -> S32 <$> getInt32le
  U64{} -> U64 <$> getWord64le
  S64{} -> S64 <$> getInt64le
getStructs structs = case structs of
  U8{}:U8{}:s -> (:) <$> fmap U8 getWord8 <*> getStructs (U8 0:s)
  U8{}:S8{}:s -> (:) <$> fmap U8 getWord8 <*> getStructs (S8 0:s)
  U8{}:U16{}:s -> (:) <$> fmap U8 getWord8 <* getPad 1 <*> getStructs (U16 0:s)
  U8{}:S16{}:s -> (:) <$> fmap U8 getWord8 <* getPad 1 <*> getStructs (S16 0:s)
  U8{}:U32{}:s -> (:) <$> fmap U8 getWord8 <* getPad 3 <*> getStructs (U32 0:s)
  U8{}:S32{}:s -> (:) <$> fmap U8 getWord8 <* getPad 3 <*> getStructs (S32 0:s)
  U8{}:U64{}:s -> (:) <$> fmap U8 getWord8 <* getPad 7 <*> getStructs (U64 0:s)
  U8{}:S64{}:s -> (:) <$> fmap U8 getWord8 <* getPad 7 <*> getStructs (S64 0:s)
  S8{}:U8{}:s -> (:) <$> fmap S8 getInt8 <*> getStructs (U8 0:s)
  S8{}:S8{}:s -> (:) <$> fmap S8 getInt8 <*> getStructs (S8 0:s)
  S8{}:U16{}:s -> (:) <$> fmap S8 getInt8 <* getPad 1 <*> getStructs (U16 0:s)
  S8{}:S16{}:s -> (:) <$> fmap S8 getInt8 <* getPad 1 <*> getStructs (S16 0:s)
  S8{}:U32{}:s -> (:) <$> fmap S8 getInt8 <* getPad 3 <*> getStructs (U32 0:s)
  S8{}:S32{}:s -> (:) <$> fmap S8 getInt8 <* getPad 3 <*> getStructs (S32 0:s)
  S8{}:U64{}:s -> (:) <$> fmap S8 getInt8 <* getPad 7 <*> getStructs (U64 0:s)
  S8{}:S64{}:s -> (:) <$> fmap S8 getInt8 <* getPad 7 <*> getStructs (S64 0:s)
  U16{}:U8{}:s -> (:) <$> fmap U16 getWord16le <*> getStructs (U8 0:s)
  U16{}:S8{}:s -> (:) <$> fmap U16 getWord16le <*> getStructs (S8 0:s)
  U16{}:U16{}:s -> (:) <$> fmap U16 getWord16le <*> getStructs (U16 0:s)
  U16{}:S16{}:s -> (:) <$> fmap U16 getWord16le <*> getStructs (S16 0:s)
  U16{}:U32{}:s -> (:) <$> fmap U16 getWord16le <* getPad 2 <*> getStructs (U32 0:s)
  U16{}:S32{}:s -> (:) <$> fmap U16 getWord16le <* getPad 2 <*> getStructs (S32 0:s)
  U16{}:U64{}:s -> (:) <$> fmap U16 getWord16le <* getPad 6 <*> getStructs (U64 0:s)
  U16{}:S64{}:s -> (:) <$> fmap U16 getWord16le <* getPad 6 <*> getStructs (S64 0:s)
  S16{}:U8{}:s -> (:) <$> fmap S16 getInt16le <*> getStructs (U8 0:s)
  S16{}:S8{}:s -> (:) <$> fmap S16 getInt16le <*> getStructs (S8 0:s)
  S16{}:U16{}:s -> (:) <$> fmap S16 getInt16le <*> getStructs (U16 0:s)
  S16{}:S16{}:s -> (:) <$> fmap S16 getInt16le <*> getStructs (S16 0:s)
  S16{}:U32{}:s -> (:) <$> fmap S16 getInt16le <* getPad 2 <*> getStructs (U32 0:s)
  S16{}:S32{}:s -> (:) <$> fmap S16 getInt16le <* getPad 2 <*> getStructs (S32 0:s)
  S16{}:U64{}:s -> (:) <$> fmap S16 getInt16le <* getPad 6 <*> getStructs (U64 0:s)
  S16{}:S64{}:s -> (:) <$> fmap S16 getInt16le <* getPad 6 <*> getStructs (S64 0:s)
  U32{}:U8{}:s -> (:) <$> fmap U32 getWord32le <*> getStructs (U8 0:s)
  U32{}:S8{}:s -> (:) <$> fmap U32 getWord32le <*> getStructs (S8 0:s)
  U32{}:U16{}:s -> (:) <$> fmap U32 getWord32le <*> getStructs (U16 0:s)
  U32{}:S16{}:s -> (:) <$> fmap U32 getWord32le <*> getStructs (S16 0:s)
  U32{}:U32{}:s -> (:) <$> fmap U32 getWord32le <*> getStructs (U32 0:s)
  U32{}:S32{}:s -> (:) <$> fmap U32 getWord32le <*> getStructs (S32 0:s)
  U32{}:U64{}:s -> (:) <$> fmap U32 getWord32le <* getPad 4 <*> getStructs (U64 0:s)
  U32{}:S64{}:s -> (:) <$> fmap U32 getWord32le <* getPad 4 <*> getStructs (S64 0:s)
  S32{}:U8{}:s -> (:) <$> fmap S32 getInt32le <*> getStructs (U8 0:s)
  S32{}:S8{}:s -> (:) <$> fmap S32 getInt32le <*> getStructs (S8 0:s)
  S32{}:U16{}:s -> (:) <$> fmap S32 getInt32le <*> getStructs (U16 0:s)
  S32{}:S16{}:s -> (:) <$> fmap S32 getInt32le <*> getStructs (S16 0:s)
  S32{}:U32{}:s -> (:) <$> fmap S32 getInt32le <*> getStructs (U32 0:s)
  S32{}:S32{}:s -> (:) <$> fmap S32 getInt32le <*> getStructs (S32 0:s)
  S32{}:U64{}:s -> (:) <$> fmap S32 getInt32le <* getPad 4 <*> getStructs (U64 0:s)
  S32{}:S64{}:s -> (:) <$> fmap S32 getInt32le <* getPad 4 <*> getStructs (S64 0:s)
  U64{}:U8{}:s -> (:) <$> fmap U64 getWord64le <*> getStructs (U8 0:s)
  U64{}:S8{}:s -> (:) <$> fmap U64 getWord64le <*> getStructs (S8 0:s)
  U64{}:U16{}:s -> (:) <$> fmap U64 getWord64le <*> getStructs (U16 0:s)
  U64{}:S16{}:s -> (:) <$> fmap U64 getWord64le <*> getStructs (S16 0:s)
  U64{}:U32{}:s -> (:) <$> fmap U64 getWord64le <*> getStructs (U32 0:s)
  U64{}:S32{}:s -> (:) <$> fmap U64 getWord64le <*> getStructs (S32 0:s)
  U64{}:U64{}:s -> (:) <$> fmap U64 getWord64le <*> getStructs (U64 0:s)
  U64{}:S64{}:s -> (:) <$> fmap U64 getWord64le <*> getStructs (S64 0:s)

getPad :: Int -> Get ()
getPad n = replicateM_ n getWord8

putStructs :: [Struct] -> Put
putStructs [] = mempty
putStructs [s] = putStruct s
putStructs structs = case structs of
  a@U8{}:b@U8{}:s   -> putStruct a <> putStructs (b:s)
  a@U8{}:b@S8{}:s   -> putStruct a <> putStructs (b:s)
  a@U8{}:b@U16{}:s  -> putStruct a <> putPad 1 <> putStructs (b:s)
  a@U8{}:b@S16{}:s  -> putStruct a <> putPad 1 <> putStructs (b:s)
  a@U8{}:b@U32{}:s  -> putStruct a <> putPad 3 <> putStructs (b:s)
  a@U8{}:b@S32{}:s  -> putStruct a <> putPad 3 <> putStructs (b:s)
  a@U8{}:b@U64{}:s  -> putStruct a <> putPad 7 <> putStructs (b:s)
  a@U8{}:b@S64{}:s  -> putStruct a <> putPad 7 <> putStructs (b:s)
  a@S8{}:b@U8{}:s   -> putStruct a <> putStructs (b:s)
  a@S8{}:b@S8{}:s   -> putStruct a <> putStructs (b:s)
  a@S8{}:b@U16{}:s  -> putStruct a <> putPad 1 <> putStructs (b:s)
  a@S8{}:b@S16{}:s  -> putStruct a <> putPad 1 <> putStructs (b:s)
  a@S8{}:b@U32{}:s  -> putStruct a <> putPad 3 <> putStructs (b:s)
  a@S8{}:b@S32{}:s  -> putStruct a <> putPad 3 <> putStructs (b:s)
  a@S8{}:b@U64{}:s  -> putStruct a <> putPad 7 <> putStructs (b:s)
  a@S8{}:b@S64{}:s  -> putStruct a <> putPad 7 <> putStructs (b:s)
  a@U16{}:b@U8{}:s  -> putStruct a <> putStructs (b:s)
  a@U16{}:b@S8{}:s  -> putStruct a <> putStructs (b:s)
  a@U16{}:b@U16{}:s -> putStruct a <> putStructs (b:s)
  a@U16{}:b@S16{}:s -> putStruct a <> putStructs (b:s)
  a@U16{}:b@U32{}:s -> putStruct a <> putPad 2 <> putStructs (b:s)
  a@U16{}:b@S32{}:s -> putStruct a <> putPad 2 <> putStructs (b:s)
  a@U16{}:b@U64{}:s -> putStruct a <> putPad 6 <> putStructs (b:s)
  a@U16{}:b@S64{}:s -> putStruct a <> putPad 6 <> putStructs (b:s)
  a@S16{}:b@U8{}:s  -> putStruct a <> putStructs (b:s)
  a@S16{}:b@S8{}:s  -> putStruct a <> putStructs (b:s)
  a@S16{}:b@U16{}:s -> putStruct a <> putStructs (b:s)
  a@S16{}:b@S16{}:s -> putStruct a <> putStructs (b:s)
  a@S16{}:b@U32{}:s -> putStruct a <> putPad 2 <> putStructs (b:s)
  a@S16{}:b@S32{}:s -> putStruct a <> putPad 2 <> putStructs (b:s)
  a@S16{}:b@U64{}:s -> putStruct a <> putPad 6 <> putStructs (b:s)
  a@S16{}:b@S64{}:s -> putStruct a <> putPad 6 <> putStructs (b:s)
  a@U32{}:b@U8{}:s  -> putStruct a <> putStructs (b:s)
  a@U32{}:b@S8{}:s  -> putStruct a <> putStructs (b:s)
  a@U32{}:b@U16{}:s -> putStruct a <> putStructs (b:s)
  a@U32{}:b@S16{}:s -> putStruct a <> putStructs (b:s)
  a@U32{}:b@U32{}:s -> putStruct a <> putStructs (b:s)
  a@U32{}:b@S32{}:s -> putStruct a <> putStructs (b:s)
  a@U32{}:b@U64{}:s -> putStruct a <> putPad 4 <> putStructs (b:s)
  a@U32{}:b@S64{}:s -> putStruct a <> putPad 4 <> putStructs (b:s)
  a@S32{}:b@U8{}:s  -> putStruct a <> putStructs (b:s)
  a@S32{}:b@S8{}:s  -> putStruct a <> putStructs (b:s)
  a@S32{}:b@U16{}:s -> putStruct a <> putStructs (b:s)
  a@S32{}:b@S16{}:s -> putStruct a <> putStructs (b:s)
  a@S32{}:b@U32{}:s -> putStruct a <> putStructs (b:s)
  a@S32{}:b@S32{}:s -> putStruct a <> putStructs (b:s)
  a@S32{}:b@U64{}:s -> putStruct a <> putPad 4 <> putStructs (b:s)
  a@S32{}:b@S64{}:s -> putStruct a <> putPad 4 <> putStructs (b:s)
  a@U64{}:b:s       -> putStruct a <> putStructs (b:s)
  a@S64{}:b:s       -> putStruct a <> putStructs (b:s)

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

putPad :: Int -> Put
putPad n = stimes n $ putWord8 0
