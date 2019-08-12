{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Chunks (Chunks(ChunksCons,ChunksNil))
import Data.Primitive (SmallArray)
import Data.Proxy (Proxy(Proxy))
import Test.QuickCheck (Arbitrary,Gen)
import Test.QuickCheck.Classes (eqLaws,semigroupLaws)
import Test.QuickCheck.Classes (monoidLaws,isListLaws)
import Test.Tasty (defaultMain,testGroup,TestTree)
import qualified GHC.Exts as Exts
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes as QCC
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Chunks"
  [ lawsToTest (eqLaws (Proxy :: Proxy (Chunks Integer)))
  , lawsToTest (semigroupLaws (Proxy :: Proxy (Chunks Integer)))
  , lawsToTest (monoidLaws (Proxy :: Proxy (Chunks Integer)))
  , lawsToTest (isListLaws (Proxy :: Proxy (Chunks Integer)))
  ]

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)


instance Arbitrary a => Arbitrary (Chunks a) where
  arbitrary = QC.choose (0,3 :: Int) >>= \case
    0 -> pure ChunksNil
    1 -> do
      a <- arbitrarySmallArray
      pure (ChunksCons a ChunksNil)
    2 -> do
      a <- arbitrarySmallArray
      b <- arbitrarySmallArray
      pure (ChunksCons a (ChunksCons b ChunksNil))
    3 -> do
      a <- arbitrarySmallArray
      b <- arbitrarySmallArray
      c <- arbitrarySmallArray
      pure (ChunksCons a (ChunksCons b (ChunksCons c ChunksNil)))
    _ -> error "Chunks.arbitrary: not possible"

instance Arbitrary a => Arbitrary (SmallArray a) where
  arbitrary = arbitrarySmallArray

arbitrarySmallArray :: Arbitrary a => Gen (SmallArray a)
arbitrarySmallArray = do
  n <- QC.choose (0,2 :: Int)
  fmap Exts.fromList (QC.vectorOf n QC.arbitrary)
