{-# LANGUAGE GADTs
           , BangPatterns
           , RankNTypes
           , ScopedTypeVariables
           , FlexibleInstances
   #-}

module Data.Set.Int64 (
Int64Set, Set(..), singleton, fromList,
insert, delete, alterF,
intersection, union, difference,
toAscList, toDesList,
) where

import Data.Word (Word64)
import Data.Int (Int64)
import qualified Data.Set.Word64.Internal as Internal
import qualified Data.Foldable as Foldable
import Prelude hiding (foldMap, foldl, foldr)
import qualified GHC.Exts as Ext (build)

type Int64Set = Set Int64
data Set i64 where Set :: !Internal.Tree -> Set Int64

instance Show (Set i64) where
   show si =
     "fromList " <> show (toAscList (observe si))

instance Eq (Set i64) where
   Set sx == Set sy = sx == sy

instance Semigroup (Set i64) where (<>) = union
instance Monoid (Set Int64) where mempty = Set Internal.empty

singleton :: Int64 -> Set Int64
singleton = Set . Internal.singleton . int64ToWord64

fromList :: [Int64] -> Set Int64
fromList = Foldable.foldl' (flip insert) mempty

insert :: a -> Set a -> Set a
insert !i (Set sx) = Set (Internal.insert (int64ToWord64 i) sx)

delete :: a -> Set a -> Set a
delete !i (Set sx) = Set (Internal.delete (int64ToWord64 i) sx)

alterF :: (Functor m)=> (Bool -> m Bool) -> i64 -> Set i64 -> m (Set i64)
alterF = alterFWith fmap

alterFWith ::
   forall m i64.
   (forall x y. (x -> y) -> m x -> m y) ->
   (Bool -> m Bool) ->
   i64 -> Set i64 -> m (Set i64)
{-^ @alterFWith@ provides a version of 'alterF' that you can use with an alternative 'Functor' class.
-}
alterFWith !mapper f = go -- Hopefully this makes specialization (e.g. in 'alterF') more reliable.
   where
      go i (Set sx) = choose `mapper` f member_
         where
            x = int64ToWord64 i
            -- Reuse the information you have!
            member_ = Internal.member x sx
            (inserted, deleted)
               | member_ = (sx, Internal.delete x sx)
               | otherwise = (Internal.insert x sx, sx)
            choose True = Set inserted
            choose False = Set deleted
{-# INLINE alterFWith #-}

foldMap :: (Monoid b)=> (i64 -> b) -> Set i64 -> b
foldMap f (Set sx) = case sx of
   Internal.Branch pm l r
      | Internal.suffixOf pm == Internal.suffixBitMask
      -> Internal.foldMap f' r <> Internal.foldMap f' l
   _
      -> Internal.foldMap f' sx
   where
      f' = f . word64ToInt64

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr f z (Set sx) = case sx of
   Internal.Branch pm l r
      | Internal.suffixOf pm == Internal.suffixBitMask
      -> Internal.foldr f' (Internal.foldr f' z l) r
   _
      -> Internal.foldr f' z sx
   where
      f' = f . word64ToInt64

foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' f z (Set sx) = case sx of
   Internal.Branch pm l r
      | Internal.suffixOf pm == Internal.suffixBitMask
      -> Internal.foldr' f' (Internal.foldr' f' z l) r
   _
      -> Internal.foldr' f' z sx
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

foldl' :: (b -> a -> b) -> b -> Set a -> b
foldl' f z (Set sx) = case sx of
   Internal.Branch pm l r
      | Internal.suffixOf pm == Internal.suffixBitMask
      -> Internal.foldl' f' (Internal.foldl' f' z r) l
   _
      -> Internal.foldl' f' z sx
   where
      f' z' x = f z' (word64ToInt64 x)

instance Foldable Set where
   null (Set sw) = Internal.null sw
   foldMap = foldMap
   foldr = foldr
   foldr' = foldr'
   foldl = foldl
   foldl' = foldl'
   elem i (Set sw) = Internal.member (int64ToWord64 i) sw
   maximum = foldl (\ _ x -> x) (error "maximum: empty Set")
   minimum = foldr (\ x _ -> x) (error "minimum: empty Set")

toAscList :: Set w64 -> [w64]
toAscList xs = Ext.build (\ c n -> foldr c n xs)
{-# INLINE [~0] toAscList #-}
toDesList :: Set w64 -> [w64]
toDesList xs = Ext.build (\ c n -> foldl (flip c) n xs)
{-# INLINE [~0] toDesList #-}

union :: Set i -> Set i -> Set i
union = liftSet2 Internal.union

intersection :: Set a -> Set a -> Set a
intersection = liftSet2 Internal.intersection

difference :: Set i -> Set i -> Set i
difference = liftSet2 Internal.difference

observe :: Set a -> Set Int64
observe si@(Set _) = si

liftSet2 ::
   (Internal.Tree -> Internal.Tree -> Internal.Tree) ->
   Set i64 -> Set i64 -> Set i64
liftSet2 f (Set sx) (Set sy) = Set (f sx sy)

int64ToWord64 :: Int64 -> Word64
int64ToWord64 = fromIntegral

word64ToInt64 :: Word64 -> Int64
word64ToInt64 = fromIntegral
