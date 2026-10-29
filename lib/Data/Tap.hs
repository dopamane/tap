module Data.Tap (Hdr(..)) where

import Data.Word

data Hdr = Hdr{ver :: Word8, hpd :: Word8, len :: Word16, prsnt :: Word32}
  deriving (Eq, Read, Show)
