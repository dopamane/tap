module Main (main) where

import Data.Binary
import Data.Bits
import Data.List.NonEmpty (NonEmpty(..))
import Data.Tap
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain $ testGroup "Data.Tap" [hdrBinTrip]

hdrBinTrip :: TestTree
hdrBinTrip = testProperty "HdrBinTrip" $ property $ do
  h <- forAll genHdr
  tripping h encode $ \bs -> case decodeOrFail bs of
    Left  (_, _, er) -> Left  er
    Right (_, _, h') -> Right (h' :: Hdr)

genHdr :: MonadGen m => m Hdr
genHdr = Hdr <$> genWord8 <*> genWord8 <*> genWord16 <*> genPrsnt

genPrsnt :: MonadGen m => m (NonEmpty Word32)
genPrsnt = do
  p <- genWord32
  if testBit p 31 then (p:|) <$> go else return $ p:|[]
  where
    go = do
      p <- genWord32
      if testBit p 31 then (p:) <$> go else return [p]

genWord32 :: MonadGen m => m Word32
genWord32 = Gen.word32 Range.linearBounded

genWord16 :: MonadGen m => m Word16
genWord16 = Gen.word16 Range.linearBounded

genWord8 :: MonadGen m => m Word8
genWord8 = Gen.word8 Range.linearBounded
