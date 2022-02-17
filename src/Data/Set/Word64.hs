{-# LANGUAGE GADTs
           , RankNTypes
           , ScopedTypeVariables
           , BangPatterns
   #-}


module Data.Set.Word64 (
Word64Set, Set (..), empty, singleton, fromList,
member, insert, delete, alterF,
intersection, union, difference,
toAscList, toDesList,
) where

import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified Data.Set.Word64.Internal as Internal
import qualified GHC.Exts as Ext (build)

type Word64Set = Set Word64

data Set a where Set :: !Internal.Tree -> Set Word64

instance Foldable Set where
   null (Set sw) = Internal.null sw
   foldr f z (Set sw) = Internal.foldr f z sw
   foldr' f z (Set sw) = Internal.foldr' f z sw
   foldl f z (Set sw) = Internal.foldl f z sw
   foldl' f z (Set sw) = Internal.foldl' f z sw
   foldMap f (Set sw) = Internal.foldMap f sw
   maximum (Set sw) = fromMaybe (error "maximum: empty Set") (Internal.lookupMax sw)
   minimum (Set sw) = fromMaybe (error "maximum: empty Set") (Internal.lookupMin sw)

empty :: Set Word64
empty = Set Internal.Seed

singleton :: Word64 -> Set Word64
singleton x = Set (Internal.singleton x)

fromList :: [Word64] -> Set Word64
fromList ws = Set (Internal.fromList ws)

member :: w64 -> Set w64 -> Bool
member w (Set sw) = Internal.member w sw

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

liftSet2 ::
   (Internal.Tree -> Internal.Tree -> Internal.Tree) ->
   Set w64 -> Set w64 -> Set w64
liftSet2 f (Set sx) (Set sy) = Set (f sx sy)

union :: Set w64 -> Set w64 -> Set w64
union = liftSet2 Internal.intersection

intersection :: Set w64 -> Set w64 -> Set w64
intersection = liftSet2 Internal.intersection

nonintersection :: Set w64 -> Set w64 -> Set w64
nonintersection = liftSet2 Internal.nonintersection

difference :: Set w64 -> Set w64 -> Set w64
difference = liftSet2 Internal.difference
