{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language TypeFamilies #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Data.Chunks
  ( Chunks(..)
  , reverse
  , reverseOnto
  , copy
  , copyReverse
  , concat
  , concatReverse
  ) where

import Prelude hiding (reverse,concat)

import Data.Primitive (SmallArray(..),SmallMutableArray(..))
import GHC.Exts (IsList,Int#,State#,SmallMutableArray#,Int(I#),(+#),(-#))
import GHC.Exts (SmallArray#)
import GHC.ST (ST(..))
import Control.Monad.ST.Run (runSmallArrayST)

import qualified GHC.Exts as Exts
import qualified Data.Foldable as F
import qualified Data.Primitive as PM

-- | A list of chunks. This is a foundation on top of
-- which efficient builder-like abstractions can be
-- implemented. There are no restrictions on the number
-- of elements in each chunk, although extremely small
-- chunks (singleton or doubleton chunks) may lead to
-- poor performance.
data Chunks a
  = ChunksCons !(SmallArray a) !(Chunks a)
  | ChunksNil
  deriving stock (Show)

instance Eq a => Eq (Chunks a) where
  (==) = eqChunks

instance IsList (Chunks a) where
  type Item (Chunks a) = SmallArray a
  toList = chunksToSmallArrayList
  fromList xs = F.foldr ChunksCons ChunksNil xs

chunksToSmallArrayList :: Chunks a -> [SmallArray a]
chunksToSmallArrayList ChunksNil = []
chunksToSmallArrayList (ChunksCons x xs) =
  x : chunksToSmallArrayList xs

eqChunks :: Eq a => Chunks a -> Chunks a -> Bool
eqChunks ChunksNil cs = allEmpty cs
eqChunks (ChunksCons x xs) cs = eqChunksConsLeft x 0 (PM.sizeofSmallArray x) xs cs

-- The first argument chunk belongs to the second argument chunks.
-- It is its head. 
eqChunksConsLeft :: Eq a => SmallArray a -> Int -> Int -> Chunks a -> Chunks a -> Bool
eqChunksConsLeft !_ !_ !len xs ChunksNil = case len of
  0 -> allEmpty xs
  _ -> False
eqChunksConsLeft x !off !len xs (ChunksCons y ys) =
  eqChunksConsBoth x off len y 0 (PM.sizeofSmallArray y) xs ys

eqChunksConsRight :: Eq a => Chunks a -> SmallArray a -> Int -> Int -> Chunks a -> Bool
eqChunksConsRight ChunksNil !_ !_ !len ys = case len of
  0 -> allEmpty ys
  _ -> False
eqChunksConsRight (ChunksCons x xs) !y !off !len ys =
  eqChunksConsBoth x 0 (PM.sizeofSmallArray x) y off len xs ys

eqChunksConsBoth :: Eq a => SmallArray a -> Int -> Int -> SmallArray a -> Int -> Int -> Chunks a -> Chunks a -> Bool
eqChunksConsBoth !xh !xoff !xlen !yh !yoff !ylen !xt !yt = case compare xlen ylen of
  LT -> eqRange xh xoff yh yoff xlen && eqChunksConsRight xt yh xlen (ylen - xlen) yt
  GT -> eqRange xh xoff yh yoff ylen && eqChunksConsLeft xh ylen (xlen - ylen) xt yt
  EQ -> xh == yh && eqChunks xt yt

eqRange :: Eq a => SmallArray a -> Int -> SmallArray a -> Int -> Int -> Bool
eqRange !xs !xoff !ys !yoff !len
  | len == 0 = True
  | otherwise =
      PM.indexSmallArray xs xoff == PM.indexSmallArray ys yoff &&
      eqRange xs (xoff + 1) ys (yoff + 1) (len - 1)

allEmpty :: Chunks a -> Bool
allEmpty ChunksNil = True
allEmpty (ChunksCons x xs) = case PM.sizeofSmallArray x of
  0 -> allEmpty xs
  _ -> False

instance Semigroup (Chunks a) where
  ChunksNil <> a = a
  cs@(ChunksCons _ _) <> ChunksNil = cs
  as@(ChunksCons _ _) <> bs@(ChunksCons _ _) =
    reverseOnto bs (reverse as)

instance Monoid (Chunks a) where
  mempty = ChunksNil

instance Foldable Chunks where
  {-# inline foldl' #-}
  {-# inline foldr #-}
  {-# inline length #-}
  foldl' = chunksFoldl'
  foldr = chunksFoldr
  length = chunksLength

chunksFoldl' :: (b -> a -> b) -> b -> Chunks a -> b
{-# inline chunksFoldl' #-}
chunksFoldl' f = go where
  go !acc ChunksNil = acc
  go !acc (ChunksCons x cs) = go (F.foldl' f acc x) cs

chunksFoldr :: (a -> b -> b) -> b -> Chunks a -> b
{-# inline chunksFoldr #-}
chunksFoldr f z0 = go where
  go ChunksNil = z0
  go (ChunksCons x cs) = F.foldr f (go cs) x

chunksLength :: Chunks a -> Int
{-# inline chunksLength #-}
chunksLength = chunksLengthGo 0

chunksLengthGo :: Int -> Chunks a -> Int
chunksLengthGo !n ChunksNil = n
chunksLengthGo !n (ChunksCons c cs) =
  chunksLengthGo (n + PM.sizeofSmallArray c) cs

-- | Reverse chunks but not the elements within each
-- chunk.
--
-- >>> reverse [[42,17,94],[6,12],[3,14]]
-- [[3,14],[6,12],[42,17,94]]
reverse :: Chunks a -> Chunks a
reverse = reverseOnto ChunksNil

-- | Variant of 'reverse' that allows the caller to provide
-- an initial list of chunks that the reversed chunks will
-- be pushed onto.
--
-- >>> reverseOnto [[15],[12,4]] [[42,17,94],[6,12],[3,14]]
-- [[3,14],[6,12],[42,17,94],[15],[12,4]]
reverseOnto :: Chunks a -> Chunks a -> Chunks a
reverseOnto !x ChunksNil = x
reverseOnto !x (ChunksCons y ys) =
  reverseOnto (ChunksCons y x) ys

-- | Copy the contents of the chunks into a mutable array.
-- Precondition: The destination must have enough space to
-- house the contents. This is not checked.
--
-- > dest (before): [x,x,x,x,x,x,x,x,x,x,x,x]
-- > copy dest 2 [[X,Y,Z],[A,B],[C,D]] (returns 9)
-- > dest (after):  [x,x,X,Y,Z,A,B,C,D,x,x,x]
copy ::
     SmallMutableArray s a -- ^ Destination
  -> Int -- ^ Destination offset
  -> Chunks a -- ^ Source
  -> ST s Int -- ^ Returns the next index into the destination after the payload
{-# inline copy #-}
copy (SmallMutableArray dst) (I# off) cs = ST
  (\s0 -> case copy# dst off cs s0 of
    (# s1, nextOff #) -> (# s1, I# nextOff #)
  )

copy# :: SmallMutableArray# s a -> Int# -> Chunks a -> State# s -> (# State# s, Int# #)
copy# _ off ChunksNil s0 = (# s0, off #)
copy# marr off (ChunksCons (SmallArray c) cs) s0 =
  let !sz = Exts.sizeofSmallArray# c in
  case Exts.copySmallArray# c 0# marr off sz s0 of
    s1 -> copy# marr (off +# sz) cs s1

-- | Copy the contents of the chunks into a mutable array,
-- reversing the order of the chunks. Precondition: The
-- destination must have enough space to house the contents.
-- This is not checked.
--
-- > dest (before): [x,x,x,x,x,x,x,x,x,x,x,x]
-- > copyReverse dest 10 [[X,Y,Z],[A,B],[C,D]] (returns 3)
-- > dest (after):  [x,x,x,C,D,A,B,X,Y,Z,x,x]
copyReverse ::
     SmallMutableArray s a -- ^ Destination
  -> Int -- ^ Destination range successor
  -> Chunks a -- ^ Source
  -> ST s Int -- ^ Returns the next index into the destination after the payload
{-# inline copyReverse #-}
copyReverse (SmallMutableArray dst) (I# off) cs = ST
  (\s0 -> case copyReverse# dst off cs s0 of
    (# s1, nextOff #) -> (# s1, I# nextOff #)
  )

copyReverse# :: SmallMutableArray# s a -> Int# -> Chunks a -> State# s -> (# State# s, Int# #)
copyReverse# _ off ChunksNil s0 = (# s0, off #)
copyReverse# marr prevOff (ChunksCons (SmallArray c) cs) s0 =
  let !sz = Exts.sizeofSmallArray# c
      !off = prevOff -# sz in
  case Exts.copySmallArray# c 0# marr off sz s0 of
    s1 -> copyReverse# marr off cs s1

concat :: Chunks a -> SmallArray a
{-# inline concat #-}
concat x = SmallArray (concat# x)

concat# :: Chunks a -> SmallArray# a
{-# noinline concat# #-}
concat# ChunksNil = case mempty of {SmallArray x -> x}
concat# (ChunksCons c cs) = case cs of
  ChunksNil -> case c of {SmallArray x -> x}
  ChunksCons d ds -> unSmallArray $ runSmallArrayST $ do
    let szc = PM.sizeofSmallArray c
        szd = PM.sizeofSmallArray d
        szboth = szc + szd
        len = chunksLengthGo szboth ds
    dst <- PM.newSmallArray len errorThunk
    PM.copySmallArray dst 0 c 0 szc
    PM.copySmallArray dst szc d 0 szd
    _ <- copy dst szboth ds
    PM.unsafeFreezeSmallArray dst

concatReverse :: Chunks a -> SmallArray a
{-# inline concatReverse #-}
concatReverse x = SmallArray (concatReverse# x)

concatReverse# :: Chunks a -> SmallArray# a
{-# noinline concatReverse# #-}
concatReverse# ChunksNil = case mempty of {SmallArray x -> x}
concatReverse# (ChunksCons c cs) = case cs of
  ChunksNil -> case c of {SmallArray x -> x}
  ChunksCons d ds -> unSmallArray $ runSmallArrayST $ do
    let szc = PM.sizeofSmallArray c
        szd = PM.sizeofSmallArray d
        szboth = szc + szd
        len = chunksLengthGo szboth ds
    dst <- PM.newSmallArray len errorThunk
    PM.copySmallArray dst (len - szc) c 0 szc
    PM.copySmallArray dst (len - (szc + szd)) d 0 szd
    _ <- copyReverse dst (len - (szc + szd)) ds
    PM.unsafeFreezeSmallArray dst

unSmallArray :: SmallArray a -> SmallArray# a
unSmallArray (SmallArray x) = x

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "Data.Chunks: mistake"
