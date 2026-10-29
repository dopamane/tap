module Data.Tap (Tap(..), Hdr(..), Field(..)) where

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

data Field = TSFT Word64 | Flags Word8
  deriving (Eq, Read, Show)

data Struct = U8 Word8 | S8 Int8 | U16 Word16 | S16 Int16 | U32 Word32
  | S32 Int32 | U64 Word64 | S64 Int64
  deriving (Eq, Read, Show)

putStructs :: [Struct] -> Put
putStructs [] = mempty
putStructs [s] = putStruct s
putStructs structs = case structs of
  a@U8{}:b@U8{}:s   -> putStruct a <> putStructs (b:s)
  a@U8{}:b@S8{}:s   -> putStruct a <> putStructs (b:s)
  a@U8{}:b@U16{}:s  -> putStruct a <> padStruct 1 <> putStructs (b:s)
  a@U8{}:b@S16{}:s  -> putStruct a <> padStruct 1 <> putStructs (b:s)
  a@U8{}:b@U32{}:s  -> putStruct a <> padStruct 3 <> putStructs (b:s)
  a@U8{}:b@S32{}:s  -> putStruct a <> padStruct 3 <> putStructs (b:s)
  a@U8{}:b@U64{}:s  -> putStruct a <> padStruct 7 <> putStructs (b:s)
  a@U8{}:b@S64{}:s  -> putStruct a <> padStruct 7 <> putStructs (b:s)
  a@S8{}:b@U8{}:s   -> putStruct a <> putStructs (b:s)
  a@S8{}:b@S8{}:s   -> putStruct a <> putStructs (b:s)
  a@S8{}:b@U16{}:s  -> putStruct a <> padStruct 1 <> putStructs (b:s)
  a@S8{}:b@S16{}:s  -> putStruct a <> padStruct 1 <> putStructs (b:s)
  a@S8{}:b@U32{}:s  -> putStruct a <> padStruct 3 <> putStructs (b:s)
  a@S8{}:b@S32{}:s  -> putStruct a <> padStruct 3 <> putStructs (b:s)
  a@S8{}:b@U64{}:s  -> putStruct a <> padStruct 7 <> putStructs (b:s)
  a@S8{}:b@S64{}:s  -> putStruct a <> padStruct 7 <> putStructs (b:s)
  a@U16{}:b@U8{}:s  -> putStruct a <> putStructs (b:s)
  a@U16{}:b@S8{}:s  -> putStruct a <> putStructs (b:s)
  a@U16{}:b@U16{}:s -> putStruct a <> putStructs (b:s)
  a@U16{}:b@S16{}:s -> putStruct a <> putStructs (b:s)
  a@U16{}:b@U32{}:s -> putStruct a <> padStruct 2 <> putStructs (b:s)
  a@U16{}:b@S32{}:s -> putStruct a <> padStruct 2 <> putStructs (b:s)
  a@U16{}:b@U64{}:s -> putStruct a <> padStruct 6 <> putStructs (b:s)
  a@U16{}:b@S64{}:s -> putStruct a <> padStruct 6 <> putStructs (b:s)
  a@S16{}:b@U8{}:s  -> putStruct a <> putStructs (b:s)
  a@S16{}:b@S8{}:s  -> putStruct a <> putStructs (b:s)
  a@S16{}:b@U16{}:s -> putStruct a <> putStructs (b:s)
  a@S16{}:b@S16{}:s -> putStruct a <> putStructs (b:s)
  a@S16{}:b@U32{}:s -> putStruct a <> padStruct 2 <> putStructs (b:s)
  a@S16{}:b@S32{}:s -> putStruct a <> padStruct 2 <> putStructs (b:s)
  a@S16{}:b@U64{}:s -> putStruct a <> padStruct 6 <> putStructs (b:s)
  a@S16{}:b@S64{}:s -> putStruct a <> padStruct 6 <> putStructs (b:s)
  a@U32{}:b@U8{}:s  -> putStruct a <> putStructs (b:s)
  a@U32{}:b@S8{}:s  -> putStruct a <> putStructs (b:s)
  a@U32{}:b@U16{}:s -> putStruct a <> putStructs (b:s)
  a@U32{}:b@S16{}:s -> putStruct a <> putStructs (b:s)
  a@U32{}:b@U32{}:s -> putStruct a <> putStructs (b:s)
  a@U32{}:b@S32{}:s -> putStruct a <> putStructs (b:s)
  a@U32{}:b@U64{}:s -> putStruct a <> padStruct 4 <> putStructs (b:s)
  a@U32{}:b@S64{}:s -> putStruct a <> padStruct 4 <> putStructs (b:s)
  a@S32{}:b@U8{}:s  -> putStruct a <> putStructs (b:s)
  a@S32{}:b@S8{}:s  -> putStruct a <> putStructs (b:s)
  a@S32{}:b@U16{}:s -> putStruct a <> putStructs (b:s)
  a@S32{}:b@S16{}:s -> putStruct a <> putStructs (b:s)
  a@S32{}:b@U32{}:s -> putStruct a <> putStructs (b:s)
  a@S32{}:b@S32{}:s -> putStruct a <> putStructs (b:s)
  a@S32{}:b@U64{}:s -> putStruct a <> padStruct 4 <> putStructs (b:s)
  a@S32{}:b@S64{}:s -> putStruct a <> padStruct 4 <> putStructs (b:s)
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

padStruct :: Int -> Put
padStruct n = stimes n $ putWord8 0
