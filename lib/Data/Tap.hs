module Data.Tap (Tap(..), Hdr(..), Field(..)) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))

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
putStructs _ = undefined

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
