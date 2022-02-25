{-# LANGUAGE GADTs
           , TypeFamilies
           , BangPatterns
           , RankNTypes
           , ScopedTypeVariables
   #-}

module Data.Set.Int64 (
Int64Set, Set(..), singleton, fromList,
insert, delete, alterF,
intersection, union, difference,
splitMember,
toAscList, toDesList,
) where

import Control.DeepSeq
import Data.Semigroup
import qualified Data.Foldable as Foldable
import Data.Word (Word64)
import Data.Int (Int64)
import qualified Data.Set.Word64.Internal as Internal
import Prelude hiding (foldMap, foldl, foldr)
import Text.Read
import qualified GHC.Exts as Ext (IsList (..), build)

type Int64Set = Set Int64
data Set i64 where Set :: !Internal.Tree -> Set Int64

instance NFData (Set i64) where rnf = rwhnf
instance NFData1 Set where liftRnf _ = rwhnf

instance Show (Set i64) where
   show si =
     "fromList " <> show (toAscList (observe si))

instance (i64 ~ Int64)=> Read (Set i64) where
   readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      fmap fromList readPrec

   readListPrec = readListPrecDefault

instance Eq (Set i64) where
   Set sx == Set sy = sx == sy

instance Ord (Set i64) where
{-^ total lexicographic ordering (rather than partial subset ordering) -}
   compare sx sy = compare (toAscList (observe sx)) (toAscList (observe sy))

instance Semigroup (Set i64) where
   (<>) = union
   stimes = stimesIdempotent
instance (i64 ~ Int64)=> Monoid (Set i64) where mempty = Set Internal.empty

instance (i64 ~ Int64)=> Ext.IsList (Set i64) where
   type Item (Set i64) = i64
   fromList = toSet
   toList = toAscList

instance Foldable Set where
   null (Set sw) = Internal.null sw
   length (Set sw) = fromIntegral (Internal.size sw)
   foldMap = foldMap
   foldr = foldr
   foldr' = foldr'
   foldl = foldl
   foldl' = foldl'
   toList = toAscList
   elem i (Set sw) = Internal.member (int64ToWord64 i) sw
   maximum = foldl (\ _ x -> x) (error "maximum: empty Set")
   minimum = foldr (\ x _ -> x) (error "minimum: empty Set")

singleton :: Int64 -> Set Int64
singleton = Set . Internal.singleton . int64ToWord64

toSet :: (Foldable.Foldable m)=> m Int64 -> Set Int64
toSet = Foldable.foldl' (flip insert) mempty

fromList :: [Int64] -> Set Int64
fromList = toSet

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
{-^ @alterFWith@ provides one version of 'alterF' that you can use with alternative 'Functor' classes.
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

hasNegativeBranch :: Internal.PrefixWithIndex -> Bool
hasNegativeBranch pm = Internal.suffixOf pm == Internal.suffixBitMask

foldMap :: (Monoid b)=> (i64 -> b) -> Set i64 -> b
foldMap f (Set sx) = case sx of
   Internal.Branch pm nat neg | hasNegativeBranch pm
      -> Internal.foldMap f' neg <> Internal.foldMap f' nat
   _
      -> Internal.foldMap f' sx
   where f' = f . word64ToInt64

foldr :: (i64 -> b -> b) -> b -> Set i64 -> b
foldr f z (Set sx) = case sx of
   Internal.Branch pm nat neg | hasNegativeBranch pm
      -> Internal.foldr f' (Internal.foldr f' z neg) nat
   _
      -> Internal.foldr f' z sx
   where f' = f . word64ToInt64

foldr' :: (i64 -> b -> b) -> b -> Set i64 -> b
foldr' f z (Set sx) = case sx of
   Internal.Branch pm nat neg | hasNegativeBranch pm
      -> Internal.foldr' f' (Internal.foldr' f' z nat) neg
   _
      -> Internal.foldr' f' z sx
   where f' = f . word64ToInt64

foldl :: (b -> i64 -> b) -> b -> Set i64 -> b
foldl f z (Set sx) = case sx of
   Internal.Branch pm nat neg | hasNegativeBranch pm
      -> Internal.foldl f' (Internal.foldl f' z neg) nat
   _
      -> Internal.foldl f' z sx
   where f' z' x = f z' (word64ToInt64 x)

foldl' :: (a -> i64 -> a) -> a -> Set i64 -> a
foldl' f z (Set sx) = case sx of
   Internal.Branch pm nat neg | hasNegativeBranch pm
      -> Internal.foldl' f' (Internal.foldl' f' z neg) nat
   _
      -> Internal.foldl' f' z sx
   where f' z' x = f z' (word64ToInt64 x)

toAscList :: Set i64 -> [i64]
toAscList xs = Ext.build (\ c n -> foldr c n xs)
{-# INLINE [~0] toAscList #-}
toDesList :: Set i64 -> [i64]
toDesList xs = Ext.build (\ c n -> foldl (flip c) n xs)
{-# INLINE [~0] toDesList #-}

union :: Set i64 -> Set i64 -> Set i64
union = liftSet2 Internal.union

intersection :: Set i64 -> Set i64 -> Set i64
intersection = liftSet2 Internal.intersection

difference :: Set i64 -> Set i64 -> Set i64
difference = liftSet2 Internal.difference

nonintersection :: Set i64 -> Set i64 -> Set i64
nonintersection = liftSet2 Internal.nonintersection

splitMember :: i64 -> Set i64 -> (Set i64, Bool, Set i64)
splitMember i (Set sw) = case sw of
   Internal.Branch pm l r
      | hasNegativeBranch pm
      -> if i >= 0
         then case Internal.splitMember (int64ToWord64 i) l of
            (l', mmbr, r') -> (Set l', mmbr, Set (Internal.union r' r))
         else case Internal.splitMember (int64ToWord64 i) r of
            (l', mmbr, r') -> (Set (Internal.union l' l), mmbr, Set r')
   _
      -> case Internal.splitMember (int64ToWord64 i) sw of
            (l, mmbr, r) -> (Set l, mmbr, Set r)

observe :: Set a -> Set Int64
observe si@(Set _) = si

liftSet2 ::
   (Internal.Tree -> Internal.Tree -> Internal.Tree) ->
   Set i64 -> Set i64 -> Set i64
liftSet2 f (Set sx) (Set sy) = Set (f sx sy)

{- According to 'Data.Word', "For coercing between any two integer types, use fromIntegral, which is specialized for all the common cases so should be fast enough. Coercing word types to and from integer types preserves representation, not sign.".
-}

int64ToWord64 :: Int64 -> Word64
{-^ Coerce one 'Int64' to one 'Word64', preserving representation. -}
int64ToWord64 = fromIntegral

word64ToInt64 :: Word64 -> Int64
{-^ Coerce one 'Word64' to one 'Int64', preserving representation. -}
word64ToInt64 = fromIntegral
