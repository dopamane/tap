{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Tap
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Data.Tap" [hdrBinTrip, testDetail, testMkPrsnts]

testMkPrsnts :: TestTree
testMkPrsnts = testCaseSteps "mkPrsnts" $ \step -> do
  step "empty"
  mkPrsnts [] @?= 0 :| []
  step "singleton"
  mkPrsnts [0] @?= 1 :| []
  step "few"
  mkPrsnts [0, 3, 5] @?= 0b0000_0000_0000_0000_0000_0000_0010_1001 :| []
  step "two"
  mkPrsnts [0, 35] @?=
    0b1000_0000_0000_0000_0000_0000_0000_0001 :|
    [0b0000_0000_0000_0000_0000_0000_0000_1000]
  step "gap"
  mkPrsnts [2, 64] @?=
    0b1000_0000_0000_0000_0000_0000_0000_0100 :|
    [ 0b1000_0000_0000_0000_0000_0000_0000_0000
    , 0b0000_0000_0000_0000_0000_0000_0000_0001]

testDetail :: TestTree
testDetail = testCaseSteps "detail" $ \step -> do
  step "encd"
  BS.unpack (runPut $ putTap detailTap) @?= detailBS
  step "decd"
  runGet (getTap detailFlds) (BS.pack detailBS) @?= detailTap
  step "trip"
  runGet (getTap detailFlds) (runPut $ putTap detailTap) @?= detailTap

detailTap :: Tap
detailTap = Tap (Hdr 0 0 16 $ pure 0b0000_0000_0000_0000_0100_0001_0010_0010)
  detailFlds

detailFlds :: [Fld]
detailFlds = [Fld "foo" 1 [U8 0x10], Fld "bar" 5 [U16 3], Fld "baz" 8 [U8 3]
  , Fld "quz" 14 [U16 0xABCD]]

detailBS :: [Word8]
detailBS = [0,0,16,0,0b0010_0010,0b0100_0001,0,0,0x10,0,3,0,3,0,0xCD,0xAB]

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
