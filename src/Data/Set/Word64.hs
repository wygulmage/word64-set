{-# LANGUAGE GADTs
           , TypeFamilies
           , RankNTypes
           , ScopedTypeVariables
           , BangPatterns
   #-}


module Data.Set.Word64 (
Word64Set, Set (..), singleton, toSet,
insert, delete, alterF,
intersection, union, difference,
toAscList, toDesList,
) where

import Control.DeepSeq
import Data.Semigroup
import Data.Foldable
import qualified Data.Foldable as Foldable
import Data.Word (Word64)
import qualified Data.Set.Word64.Internal as Internal
import qualified GHC.Exts as Ext (IsList (..), build)

type Word64Set = Set Word64

data Set w64 where Set :: !Internal.Tree -> Set Word64

instance NFData (Set i64) where rnf = rwhnf
instance NFData1 Set where liftRnf _ = rwhnf

instance Show (Set w64) where
   show sw =
     "fromList " <> show (toAscList (observe sw))

instance Eq (Set w64) where
   Set sx == Set sy = sx == sy

instance Ord (Set w64) where
   compare sx sy = compare (toAscList (observe sx)) (toAscList (observe sy))

instance Semigroup (Set w64) where
   (<>) = union
   stimes = stimesIdempotent
instance (w64 ~ Word64)=> Monoid (Set w64) where mempty = Set Internal.empty

instance (i64 ~ Word64)=> Ext.IsList (Set i64) where
   type Item (Set i64) = i64
   fromList = toSet
   toList = toAscList

instance Foldable Set where
   null (Set sw) = Internal.null sw
   foldMap f (Set sw) = Internal.foldMap f sw
   foldr f z (Set sw) = Internal.foldr f z sw
   foldr' f z (Set sw) = Internal.foldr' f z sw
   foldl f z (Set sw) = Internal.foldl f z sw
   foldl' f z (Set sw) = Internal.foldl' f z sw
   toList = toAscList
   elem w (Set sw) = Internal.member w sw
   maximum = foldl (\ _ x -> x) (error "maximum: empty Set")
   minimum = foldr (\ x _ -> x) (error "minimum: empty Set")

singleton :: Word64 -> Set Word64
singleton x = Set (Internal.singleton x)

toSet :: (Foldable m)=> m Word64 -> Set Word64
toSet = foldl' (flip insert) mempty

insert :: w64 -> Set w64 -> Set w64
insert w (Set sw) = Set (Internal.insert w sw)

delete :: w64 -> Set w64 -> Set w64
delete w (Set sw) = Set (Internal.delete w sw)

toAscList :: Set w64 -> [w64]
toAscList xs = Ext.build (\ c n -> foldr c n xs)
{-# INLINE [~0] toAscList #-}
toDesList :: Set w64 -> [w64]
toDesList xs = Ext.build (\ c n -> foldl (flip c) n xs)
{-# INLINE [~0] toDesList #-}


alterF :: (Functor m)=> (Bool -> m Bool) -> w64 -> Set w64 -> m (Set w64)
alterF = alterFWith fmap

alterFWith ::
   forall m w64.
   (forall x y. (x -> y) -> m x -> m y) ->
   (Bool -> m Bool) ->
   w64 -> Set w64 -> m (Set w64)
{-^ @alterFWith@ provides a version of 'alterF' that you can use with an alternative 'Functor' class.
-}
alterFWith !mapper f = go -- Hopefully this makes specialization (e.g. in 'alterF') more reliable.
   where
      go x (Set sx) = choose `mapper` f member_
         where
            -- Reuse the information you have!
            member_ = Internal.member x sx
            (inserted, deleted)
               | member_ = (sx, Internal.delete x sx)
               | otherwise = (Internal.insert x sx, sx)
            choose True = Set inserted
            choose False = Set deleted
{-# INLINE alterFWith #-}

union :: Set w64 -> Set w64 -> Set w64
union = liftSet2 Internal.intersection

intersection :: Set w64 -> Set w64 -> Set w64
intersection = liftSet2 Internal.intersection

nonintersection :: Set w64 -> Set w64 -> Set w64
nonintersection = liftSet2 Internal.nonintersection

difference :: Set w64 -> Set w64 -> Set w64
difference = liftSet2 Internal.difference

observe :: Set w64 -> Set Word64
observe si@(Set _) = si

liftSet2 ::
   (Internal.Tree -> Internal.Tree -> Internal.Tree) ->
   Set w64 -> Set w64 -> Set w64
liftSet2 f (Set sx) (Set sy) = Set (f sx sy)
