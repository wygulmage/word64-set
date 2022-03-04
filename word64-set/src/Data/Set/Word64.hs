{-# LANGUAGE GADTs
           , TypeFamilies
           , RankNTypes
           , ScopedTypeVariables
           , BangPatterns
   #-}

{- Sets of Word64s
This is based on the hard work of Donnacha Oisin Kidney and others.
-}

module Data.Set.Word64 (
Word64Set, Set (..), empty, singleton, fromList,
insert, delete, alterF,
intersection, union, disjointUnion, difference,
splitMember, maxView, minView,
toAscList, toDesList, size,
) where

import Control.DeepSeq
import Data.Semigroup
import Data.Foldable
import Data.Word (Word64)
import qualified Data.Set.Word64.Internal as Internal
import Text.Read
import qualified GHC.Exts as Ext (IsList (..), build)

type Word64Set = Set Word64

data Set w64 where Set :: !Internal.Tree -> Set Word64

instance NFData (Set i64) where rnf = rwhnf
instance NFData1 Set where liftRnf _ = rwhnf

instance Show (Set w64) where
   show sw =
     "fromList " <> show (toAscList (observe sw))
   {-# NOTINLINE show #-}

instance (w64 ~ Word64)=> Read (Set w64) where
   readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      fmap fromList readPrec
   {-# NOTINLINE readPrec #-}

   readListPrec = readListPrecDefault
   {-# NOTINLINE readListPrec #-}

instance Eq (Set w64) where
   Set sx == Set sy = sx == sy
   {-# INLINE (==) #-}

instance Ord (Set w64) where
   compare sx sy = compare (toAscList (observe sx)) (toAscList (observe sy))

instance Semigroup (Set w64) where
   (<>) = union
   {-# INLINE (<>) #-}
   stimes = stimesIdempotent

instance (w64 ~ Word64)=> Monoid (Set w64) where
   mempty = empty
   {-# INLINE mempty #-}
   mconcat = foldl' (<>) mempty
   {-# INLINE [~0] mconcat #-}

instance (i64 ~ Word64)=> Ext.IsList (Set i64) where
   type Item (Set i64) = i64
   fromList = fromList
   {-# INLINE fromList #-}
   toList = toAscList
   {-# INLINE toList #-}

instance Foldable Set where
   null (Set sw) = Internal.null sw
   {-# INLINE null #-}
   length = fromEnum . size
   {-# INLINE length #-}
   foldMap f (Set sw) = Internal.foldMap f sw
   {-# INLINABLE foldMap #-}
   foldr f z (Set sw) = Internal.foldr f z sw
   foldr' f z (Set sw) = Internal.foldr' f z sw
   foldl f z (Set sw) = Internal.foldl f z sw
   foldl' f z (Set sw) = Internal.foldl' f z sw
   toList = toAscList
   {-# INLINE toList #-}
   elem w (Set sw) = Internal.member w sw
   {-# INLINE elem #-}
   maximum = foldl (\ _ x -> x) (error "maximum: empty Set")
   {-# NOTINLINE maximum #-}
   minimum = foldr (\ x _ -> x) (error "minimum: empty Set")
   {-# NOTINLINE minimum #-}

size :: Set w64 -> Word64
size (Set sw) = Internal.size sw
{-# INLINE size #-}

empty :: Set Word64
empty = Set Internal.empty

singleton :: Word64 -> Set Word64
singleton x = Set (Internal.singleton x)

toSet :: (Foldable m)=> m Word64 -> Set Word64
toSet = foldl' (flip insert) mempty

fromList :: [Word64] -> Set Word64
fromList = toSet

insert :: w64 -> Set w64 -> Set w64
insert w (Set sw) = Set (Internal.insert w sw)
{-# INLINE insert #-}

delete :: w64 -> Set w64 -> Set w64
delete w (Set sw) = Set (Internal.delete w sw)
{-# INLINE delete #-}

toAscList :: Set w64 -> [w64]
toAscList xs = Ext.build (\ c n -> foldr c n xs)
{-# INLINE [~0] toAscList #-}
toDesList :: Set w64 -> [w64]
toDesList xs = Ext.build (\ c n -> foldl (flip c) n xs)
{-# INLINE [~0] toDesList #-}


alterF :: (Functor m)=> (Bool -> m Bool) -> w64 -> Set w64 -> m (Set w64)
{-^ @alterF@ is the general-purpose function for operating on a single element of a 'Set'.
@'getConst' . alterF 'Const'@ is equivalent to 'elem'.
@'runIdentity' . alterF (\_-> 'Identity' 'True')@ is equivalent to 'insert', although it has slightly different performance characteristics.
@'runIdentity' . alterF (\_-> 'Identity' 'False')@ is equivalent to 'delete', although it has slightly different performance characteristics.
@alterF (\ b -> (b, 'True'))@ inserts an element into a 'Set' and tells you whether it was already present.
@alterF (\ b -> (b, 'False'))@ deletes an element from a 'Set' and tells you whether it was already present.
-}
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
union = liftSet2 Internal.union
{-# INLINE union #-}

intersection :: Set w64 -> Set w64 -> Set w64
intersection = liftSet2 Internal.intersection
{-# INLINE intersection #-}

disjointUnion :: Set w64 -> Set w64 -> Set w64
disjointUnion = liftSet2 Internal.disjointUnion
{-# INLINE disjointUnion #-}

difference :: Set w64 -> Set w64 -> Set w64
difference = liftSet2 Internal.difference
{-# INLINE difference #-}

splitMember :: w64 -> Set w64 -> (Set w64, Bool, Set w64)
{-^ Split a 'Set' into the 'Set' of elements that are less than and the 'Set' of elements that are greater than a 'Word64', and indicate whether the provided 'Word64 was present in the 'Set'.
-}
splitMember w (Set sw) =
   case Internal.splitMember w sw of (l, mmbr, r) -> (Set l, mmbr, Set r)
{-# NOTINLINE splitMember #-}

minView :: Set w64 -> Maybe (w64, Set w64)
minView (Set sw) = case Internal.minView sw of
   Just (w, sw') -> Just (w, Set sw')
   Nothing -> Nothing
{-# NOTINLINE minView #-}

maxView :: Set w64 -> Maybe (w64, Set w64)
maxView (Set sw) = case Internal.maxView sw of
   Just (w, sw') -> Just (w, Set sw')
   Nothing -> Nothing
{-# NOTINLINE maxView #-}

observe :: Set w64 -> Set Word64
observe si@(Set _) = si
{-# INLINE observe #-}

liftSet2 ::
   (Internal.Tree -> Internal.Tree -> Internal.Tree) ->
   Set w64 -> Set w64 -> Set w64
liftSet2 f = \ (Set sx) (Set sy) -> Set (f sx sy)
{-# INLINE liftSet2 #-}
