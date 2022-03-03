{-# LANGUAGE GADTs
           , TypeFamilies
           , BangPatterns
           , RankNTypes
           , ScopedTypeVariables
   #-}


module Data.Set.Int64 (
Int64Set, Set(..), empty, singleton, fromList,
insert, delete, alterF,
intersection, union, disjointUnion, difference,
splitMember, maxView, minView,
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
import qualified GHC.Exts as Ext


type Int64Set = Set Int64
data Set i64 where Set :: !Internal.Tree -> Set Int64

instance NFData (Set i64) where rnf = rwhnf
instance NFData1 Set where liftRnf _ = rwhnf

instance Show (Set i64) where
   show si =
     "fromList " <> show (toAscList (observe si))
   {-# NOTINLINE show #-}

instance (i64 ~ Int64)=> Read (Set i64) where
   readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      fmap fromList readPrec
   {-# NOTINLINE readPrec #-}

   readListPrec = readListPrecDefault
   {-# NOTINLINE readListPrec #-}

instance Eq (Set i64) where
   Set sx == Set sy = sx == sy
   {-# INLINE (==) #-}

instance Ord (Set i64) where
{-^ total lexicographic ordering (rather than partial subset ordering) -}
   compare sx sy = compare (toAscList (observe sx)) (toAscList (observe sy))

instance Semigroup (Set i64) where
   (<>) = union
   {-# INLINE (<>) #-}
   stimes = stimesIdempotent

instance (i64 ~ Int64)=> Monoid (Set i64) where
   mempty = empty
   {-# INLINE mempty #-}
   mconcat = Foldable.foldl' union empty
   {-# INLINE [~0] mconcat #-}

instance (i64 ~ Int64)=> Ext.IsList (Set i64) where
   type Item (Set i64) = i64
   fromList = fromList
   {-# INLINE fromList #-}
   toList = toAscList
   {-# INLINE toList #-}

instance Foldable Set where
   null (Set sw) = Internal.null sw
   {-# INLINE null #-}
   length (Set sw) = fromIntegral (Internal.size sw)
   {-# INLINE length #-}
   foldMap = foldMap
   {-# INLINABLE foldMap #-}
   foldr = foldr
   foldr' = foldr'
   foldl = foldl
   foldl' = foldl'
   toList = toAscList
   {-# INLINE toList #-}
   elem i (Set sw) = Internal.member (int64ToWord64 i) sw
   {-# INLINE elem #-}
   maximum = foldl (\ _ x -> x) (error "maximum: empty Set")
   {-# NOTINLINE maximum #-}
   minimum = foldr (\ x _ -> x) (error "minimum: empty Set")
   {-# NOTINLINE minimum #-}

empty :: Set Int64
empty = Set Internal.empty

singleton :: Int64 -> Set Int64
singleton = Set . Internal.singleton . int64ToWord64

toSet :: (Foldable.Foldable m)=> m Int64 -> Set Int64
toSet = Foldable.foldl' (flip insert) mempty

fromList :: [Int64] -> Set Int64
fromList = toSet

insert :: a -> Set a -> Set a
insert !i (Set sx) = Set (Internal.insert (int64ToWord64 i) sx)
{-# INLINE insert #-}

delete :: a -> Set a -> Set a
delete !i (Set sx) = Set (Internal.delete (int64ToWord64 i) sx)
{-# INLINE delete #-}

alterF :: (Functor m)=> (Bool -> m Bool) -> i64 -> Set i64 -> m (Set i64)
{-^ @alterF@ is the general-purpose function for operating on a single element of a 'Set'.
@'getConst' . alterF 'Const'@ is equivalent to 'elem'.
@'runIdentity' . alterF (\_-> 'Identity' 'True')@ is equivalent to 'insert', although it has slightly different performance characteristics.
@'runIdentity' . alterF (\_-> 'Identity' 'False')@ is equivalent to 'delete', although it has slightly different performance characteristics.
@alterF (\ b -> (b, 'True'))@ inserts an element into a 'Set' and tells you whether it was already present.
@alterF (\ b -> (b, 'False'))@ deletes an element from a 'Set' and tells you whether it was already present.
-}
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
      go i (Set sx) = choose `mapper` f found
         where
            x = int64ToWord64 i
            -- Reuse the information you have!
            found = Internal.member x sx
            (inserted, deleted)
               | found     = (sx, Internal.delete x sx)
               | otherwise = (Internal.insert x sx, sx)
            choose b
               | b         = Set inserted
               | otherwise = Set deleted
{-# INLINE alterFWith #-}

hasSignSplit :: Internal.PrefixWithIndex -> Bool
-- If the suffix is 63, the prefix must be 0, so the whole value is 63.
hasSignSplit pm = pm == Internal.suffixBitMask -- 63
{-# INLINE hasSignSplit #-}

-- Fold order is small (left) to large (right). But the prefix is an unsigned value, so the order of the root branches be fixed when a set contains both non-negative and negative values.

foldMap :: (Monoid b)=> (i64 -> b) -> Set i64 -> b
foldMap f (Set sx) = case sx of
   Internal.Branch pm nat neg | hasSignSplit pm
      -> Internal.foldMap f' neg <> Internal.foldMap f' nat
   _
      -> Ext.inline Internal.foldMap f' sx
   where f' = f . word64ToInt64
{-# INLINABLE foldMap #-}
-- `foldMap` is marked `INLINABLE` because we want it to specialize to the `Monoid' it uses.

-- Should I mark the directional folds NOTINLINE?

foldr :: (i64 -> b -> b) -> b -> Set i64 -> b
foldr f z (Set sx) = case sx of
   Internal.Branch pm nat neg | hasSignSplit pm
      -> Internal.foldr f' (Internal.foldr f' z nat) neg
   _
      -> Internal.foldr f' z sx
   where f' = f . word64ToInt64

foldr' :: (i64 -> b -> b) -> b -> Set i64 -> b
foldr' f z (Set sx) = case sx of
   Internal.Branch pm nat neg | hasSignSplit pm
      -> Internal.foldr' f' (Internal.foldr' f' z nat) neg
   _
      -> Internal.foldr' f' z sx
   where f' = f . word64ToInt64

foldl :: (b -> i64 -> b) -> b -> Set i64 -> b
foldl f z (Set sx) = case sx of
   Internal.Branch pm nat neg | hasSignSplit pm
      -> Internal.foldl f' (Internal.foldl f' z neg) nat
   _
      -> Internal.foldl f' z sx
   where f' z' x = f z' (word64ToInt64 x)

foldl' :: (a -> i64 -> a) -> a -> Set i64 -> a
foldl' f z (Set sx) = case sx of
   Internal.Branch pm nat neg | hasSignSplit pm
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
{-# INLINE union #-}

intersection :: Set i64 -> Set i64 -> Set i64
intersection = liftSet2 Internal.intersection
{-# INLINE intersection #-}

difference :: Set i64 -> Set i64 -> Set i64
difference = liftSet2 Internal.difference
{-# INLINE difference #-}

disjointUnion :: Set i64 -> Set i64 -> Set i64
disjointUnion = liftSet2 Internal.disjointUnion
{-# INLINE disjointUnion #-}

negativeBranch :: Internal.PrefixWithIndex -> Bool
negativeBranch pm =
   word64ToInt64 pm < 0
{-# INLINE negativeBranch #-}

-- In a `Branch pm l r`, if pm == 63 then it's the root branch, l is nonnegative, and r is negative; if word64ToInt64 pm < 0, both l and r are negative.

splitMember :: i64 -> Set i64 -> (Set i64, Bool, Set i64)
splitMember i (Set sw) = case sw of
   Internal.Branch pm nat neg | hasSignSplit pm -- root branch; nat is non-negative and neg is negative.
      -> if i >= 0
         -- Search for i in the non-negative branch and put the negative values in the 'less than' set.
         then case Internal.splitMember (int64ToWord64 i) nat of
            (lt, found, gt) -> (Set (Internal.branch pm lt neg), found, Set gt)
         -- Search for i in the negative values and split the negative values between the 'less than' set and the 'greater than' set.
         else case Internal.splitMember (int64ToWord64 i) neg of
            (lt, found, gt) -> (Set lt, found, Set (Internal.union nat gt))
   Internal.Branch pm _ _
         | word64ToInt64 pm < 0  &&  i >= 0
         -> (Set sw, False, mempty)
         | word64ToInt64 pm >= 0  &&  i < 0
         -> (mempty, False, Set sw)
   Internal.Leaf p _
         | word64ToInt64 p < 0  &&  i >= 0
         -> (Set sw, False, mempty)
         | word64ToInt64 p >= 0  &&  i < 0
         -> (mempty, False, Set sw)
   _
      -> case Internal.splitMember (int64ToWord64 i) sw of
            (l, found, r) -> (Set l, found, Set r)
{-# NOTINLINE splitMember #-}


maxView :: Set i64 -> Maybe (i64, Set i64)
maxView (Set sx) = case sx of
   Internal.Branch pm nat neg | hasSignSplit pm
      -> case Internal.deleteFindMax nat of
           (x, nat') -> Just (word64ToInt64 x, Set (Internal.branch pm nat' neg))
   _
      -> case Internal.maxView sx of
           Just (x, sx') -> Just (word64ToInt64 x, Set sx')
           Nothing -> Nothing
{-# NOTINLINE maxView #-}

minView :: Set i64 -> Maybe (i64, Set i64)
minView (Set sx) = case sx of
   Internal.Branch pm nat neg | hasSignSplit pm
      -> case Internal.deleteFindMin neg of
           (x, neg') -> Just (word64ToInt64 x, Set (Internal.branch pm nat neg'))
   _
      -> case Internal.minView sx of
           Just (x, sx') -> Just (word64ToInt64 x, Set sx')
           Nothing -> Nothing
{-# NOTINLINE minView #-}

observe :: Set a -> Set Int64
observe si@(Set _) = si
{-# INLINE observe #-}

liftSet2 ::
   (Internal.Tree -> Internal.Tree -> Internal.Tree) ->
   Set i64 -> Set i64 -> Set i64
liftSet2 f = \ (Set sx) (Set sy) -> Set (f sx sy)
{-# INLINE liftSet2 #-}

{- According to 'Data.Word', "For coercing between any two integer types, use fromIntegral, which is specialized for all the common cases so should be fast enough. Coercing word types to and from integer types preserves representation, not sign.".
-}

int64ToWord64 :: Int64 -> Word64
{-^ Coerce one 'Int64' to one 'Word64', preserving representation. -}
int64ToWord64 = fromIntegral
{-# INLINE int64ToWord64 #-}

word64ToInt64 :: Word64 -> Int64
{-^ Coerce one 'Word64' to one 'Int64', preserving representation. -}
word64ToInt64 = fromIntegral
{-# INLINE word64ToInt64 #-}
