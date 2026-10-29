module Data.Tap (Hdr(..)) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.List.NonEmpty (NonEmpty(..))

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
            p' <- getWord32le
            if testBit p' 31 then (p':) <$> go else return [p']
  put h = do
    putWord8 (ver h) >> putWord8 (hpd h) >> putWord16le (len h)
    let p:|ps = prsnt h
    putWord32le p >> go ps
    where
      go [] = mempty
      go (p:ps) = putWord32le p <> if testBit p 31 then go ps else mempty
