{-# LANGUAGE GADTs
           , BangPatterns
           , RankNTypes
           , ScopedTypeVariables
   #-}

module Data.Set.Int64 (
Set, empty, singleton, fromList,
member, insert, delete,
intersection, union, difference,
) where

import Data.Word (Word64)
import Data.Int (Int64)
import qualified Data.Set.Word64.Internal as Internal
import qualified Data.Foldable as Foldable
import Prelude hiding (foldl, foldr)

data Set a where Set :: !Internal.Tree -> Set Int64

instance Show (Set a) where
   show si =
     "fromList " <> show (Foldable.toList (observe si))

instance Eq (Set i64) where
   Set sx == Set sy = sx == sy

observe :: Set a -> Set Int64
observe si@(Set _) = si

member :: a -> Set a -> Bool
member !i (Set sx) = Internal.member (int64ToWord64 i) sx

empty :: Set Int64
empty = Set Internal.empty

singleton :: Int64 -> Set Int64
singleton = Set . Internal.singleton . int64ToWord64

fromList :: [Int64] -> Set Int64
fromList = Foldable.foldl' (flip insert) empty

insert :: a -> Set a -> Set a
insert !i (Set sx) = Set (Internal.insert (int64ToWord64 i) sx)

delete :: a -> Set a -> Set a
delete !i (Set sx) = Set (Internal.delete (int64ToWord64 i) sx)

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr f z (Set sx) = case sx of
   Internal.Branch pm l r
      | Internal.suffixOf pm == Internal.suffixBitMask
      -> Internal.foldr f' (Internal.foldr f' z l) r
   _
      -> Internal.foldr f' z sx
   where
      f' = f . word64ToInt64

foldl :: (b -> a -> b) -> b -> Set a -> b
foldl f z (Set sx) = case sx of
   Internal.Branch pm l r
      | Internal.suffixOf pm == Internal.suffixBitMask
      -> Internal.foldl f' (Internal.foldl f' z r) l
   _
      -> Internal.foldl f' z sx
   where
      f' z' x = f z' (word64ToInt64 x)

instance Foldable Set where
   null (Set sw) = Internal.null sw
   foldr = foldr
   foldl = foldl
   elem i (Set sw) = Internal.member (int64ToWord64 i) sw
   maximum = foldl (\ _ x -> x) (error "maximum: empty Set")
   minimum = foldr (\ x _ -> x) (error "minimum: empty Set")

liftSet2 ::
   (Internal.Tree -> Internal.Tree -> Internal.Tree) ->
   Set i64 -> Set i64 -> Set i64
liftSet2 f (Set sx) (Set sy) = Set (f sx sy)

union :: Set i -> Set i -> Set i
union = liftSet2 Internal.union

intersection :: Set a -> Set a -> Set a
intersection = liftSet2 Internal.intersection

difference :: Set i -> Set i -> Set i
difference = liftSet2 Internal.difference

int64ToWord64 :: Int64 -> Word64
int64ToWord64 = fromIntegral

word64ToInt64 :: Word64 -> Int64
word64ToInt64 = fromIntegral
