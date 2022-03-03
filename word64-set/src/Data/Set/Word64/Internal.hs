{-# LANGUAGE BangPatterns
           , RankNTypes
           , ScopedTypeVariables
           , FlexibleInstances
   #-}

{- Prefix-tree structured sets of Word64s

This is based on the hard word of Daan Leijen, Joachim Breitner, Chris Okasaki, and others.
-}

module Data.Set.Word64.Internal (
PrefixWithIndex, Tree (..), empty, singleton, fromList,
insert, delete,
union, intersection, difference, disjointUnion,
filter, splitMember,
member, null, foldl, foldl', foldr, foldr', foldMap, size,
suffixOf, suffixBitMask, prefixOf,
) where

import Control.DeepSeq
import Data.Bits
import Data.Monoid
import Data.Word (Word64)
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.List as List
import Prelude hiding (filter, foldMap, foldr, null, foldl)


type Prefix = Word64
type BitMap = Word64
type BitIndex = Word64 -- actually Word6
type BitMask = Word64
type PrefixWithIndex = Word64
prefixWithIndex :: Prefix -> BitMask -> PrefixWithIndex
prefixWithIndex p m = p .|. lowestBitIndex m

data Tree
   = Branch {-# UNPACK #-} !PrefixWithIndex !Tree !Tree
   | Leaf {-# UNPACK #-} !Prefix {-# UNPACK #-} !BitMap
   | Seed
   deriving (Show)
{-^ The definition of 'Set' is a bit magical, and says "You can have a set of anything you want, as long as it's 'Word64'". This allows 'Set' to pretend to be polymorphic as long as its type is fixed by a 'Set' in the argument position. For example, 'Set' is 'Foldable' because 'Foldable' methods all take a 'Set' and return something else, but it is not a 'Functor' because the type of the result is determined by the unrestricted result of a function.
-}
-- 'Branch'es should always end in a 'Leaf', never a 'Seed'. The 'BitMap' of a 'Leaf' should never be empty (0).

instance NFData Tree where rnf = rwhnf

instance Eq Tree where
   Branch pmx lx rx == Branch pmy ly ry =
      pmx == pmy  &&  lx == ly  &&  rx == ry
   Leaf px mx == Leaf py my =
      px == py  &&  mx == my
   Seed == Seed =
      True
   _ == _ =
      False


null :: Tree -> Bool
null Seed = True
null _ = False

empty :: Tree
empty = Seed

singleton :: Word64 -> Tree
singleton x = Leaf (prefixOf x) (bitmapOf x)

fromList :: [Word64] -> Tree
fromList = List.foldl' (flip insert) empty


------ Do Things with Specific Word64s.

member :: Word64 -> Tree -> Bool
{-^ Is a 'Word64' in a 'Set Word64'? -}
member !_ Seed = False
member x (Leaf p m) = prefixOf x == p  &&  bitmapOf x .&. m /= 0
member x (Branch pm l r)
   | nomatch x (prefixOf pm) (bitmapOf pm)
   = False
   | zero x (bitmapOf pm)
   = member x l
   | otherwise
   = member x r

insert :: Word64 -> Tree -> Tree
{-^ Insert a 'Word64' into a set.
-}
-- insert !x sx = insertBM (prefixOf x) (bitmapOf x) sx
-- Need to duplicate the code from 'insertBM' to keep this 'polymorphic'
insert !x = insertBM (prefixOf x) (bitmapOf x)

insertBM :: Prefix -> BitMap -> Tree -> Tree
insertBM !p' !m' sx = case sx of
    Branch pm l r
       | nomatch p' p m
       -> link p' (Leaf p' m') p sx
       | zero p' m
       -> Branch pm (insertBM p' m' l) r
       | otherwise
       -> Branch pm l (insertBM p' m' r)
       where
          !p = prefixOf pm
          !m = bitmapOf pm
    Leaf p m
       | p == p'
       -> Leaf p (m' .|. m)
       | otherwise
       -> link p' (Leaf p' m') p sx
    Seed
       -> Leaf p' m'

delete :: Word64 -> Tree -> Tree
{-^ Delete a 'Word64' from a Set. -}
delete x = deleteBM (prefixOf x) (bitmapOf x)

deleteBM :: Prefix -> BitMap -> Tree -> Tree
deleteBM p' m' sx = case sx of
   Branch pm l r
      | nomatch p' p m
      -> sx
      | zero p' m
      -> branch pm (deleteBM p' m' l) r
      | otherwise
      -> branch pm l (deleteBM p' m' r)
      where
         !p = prefixOf pm
         !m = bitmapOf pm
   Leaf p m
      | p == p'
      -> leaf p (m .&. complement m')
      | otherwise
      -> sx
   Seed
      -> Seed


------ Combine Sets ------

union :: Tree -> Tree -> Tree
union sx@(Branch pmx lx rx) sy@(Branch pmy ly ry)
   | shorter ix iy = unionx
   | shorter iy ix = uniony
   | px == py      = Branch pmx (union lx ly) (union rx ry)
   | otherwise     = link px sx py sy
   where
      px = prefixOf pmx
      ix = suffixOf pmx
      mx = bitmapOf pmx
      py = prefixOf pmy
      iy = suffixOf pmy
      my = bitmapOf pmy
      unionx
         | nomatch py px mx = link px sx py sy
         | zero py mx       = Branch pmx (union lx sy) rx
         | otherwise        = Branch pmx lx (union rx sy)
      uniony
         | nomatch px py my = link px sx py sy
         | zero px my       = Branch pmy (union sx ly) ry
         | otherwise        = Branch pmy ly (union sx ry)
union sx@Branch{} (Leaf p m) = insertBM p m sx
union sx@Branch{} Seed = sx
union (Leaf p m) sy = insertBM p m sy
union Seed sy = sy


difference :: Tree -> Tree -> Tree
difference sx sy = case sy of
   Seed -> sx
   Leaf p m -> deleteBM p m sx
   Branch pmy ly ry -> case sx of
      Seed -> Seed
      Leaf p m -> differenceBM sy
         where
           differenceBM sw = case sw of
              Branch pm' l r
                  | nomatch p (prefixOf pm') (bitmapOf pm')
                  -> sx
                  | zero p (bitmapOf pm')
                  -> differenceBM l
                  | otherwise
                  -> differenceBM r
              Leaf p' m' | p == p' -> Leaf p (m .&. complement m')
              _ -> sx
      Branch pmx lx rx
         | shorter ix iy -> differencex
         | shorter iy ix -> differencey
         | px == py -> branch pmx (difference lx ly) (difference rx ry)
         | otherwise -> sx
         where
            ix = suffixOf pmx
            iy = suffixOf pmy
            mx = bitmapOf pmx
            my = bitmapOf pmy
            px = prefixOf pmx
            py = prefixOf pmy
            differencex
               | nomatch py px mx = sx
               | zero py mx       = branch pmx (difference lx sy) rx
               | otherwise        = branch pmx lx (difference rx sy)
            differencey
               | nomatch px py my = sx
               | zero px my       = difference sx ly
               | otherwise        = difference sx ry


disjointUnion :: Tree -> Tree -> Tree
{-^ @disjointUnion@ gives the symmetric difference of 'Tree's. -}
disjointUnion sx@(Branch pmx lx rx) sy@(Branch pmy ly ry)
   | shorter ix iy
   = disjointUnionx
   | shorter iy ix
   = disjointUniony
   | px == py
   = branch pmx (disjointUnion lx ly) (disjointUnion rx ry)
   | otherwise
   = link px sx py sy
   where
      ix = suffixOf pmx
      mx = bitmapOf pmx
      iy = suffixOf pmy
      my = bitmapOf pmy
      px = prefixOf pmx
      py = prefixOf pmy
      disjointUnionx
         | nomatch py px mx
         = link px sx py sy
         | zero py mx
         = branch pmx (disjointUnion lx sy) rx
         | otherwise
         = branch pmx lx (disjointUnion rx sy)
      disjointUniony
         | nomatch px py my
         = link px sx py sy
         | zero px my
         = branch pmy (disjointUnion sx ly) ry
         | otherwise
         = branch pmy ly (disjointUnion sx ry)
disjointUnion sx@Branch{} (Leaf p m) = disjointUnionBM p m sx
disjointUnion sx@Branch{} Seed = sx
disjointUnion (Leaf p m) sy = disjointUnionBM p m sy
disjointUnion Seed sy = sy

disjointUnionBM :: Prefix -> BitMap -> Tree -> Tree
disjointUnionBM p' m' sx = case sx of
    Branch pm l r
       | nomatch p' p m
       -> link p' (Leaf p' m') p sx
       | zero p' m
       -> branch pm (disjointUnionBM p' m' l) r
       | otherwise
       -> branch pm l (disjointUnionBM p' m' r)
       where
          !p = prefixOf pm
          !m = bitmapOf pm
    Leaf p m
      | p == p'
      -> leaf p (m .+. m')
      | otherwise
      -> link p' (Leaf p' m') p sx
    Seed
       -> Leaf p' m'


intersection :: Tree -> Tree -> Tree
intersection sx@(Branch pmx lx rx) sy@(Branch pmy ly ry)
   | shorter ix iy = intersectionx
   | shorter iy ix = intersectiony
   | px == py = branch pmx (intersection lx ly) (intersection rx ry)
   | otherwise = Seed
   where
      ix = suffixOf pmx
      iy = suffixOf pmy
      px = prefixOf pmx
      mx = bitmapOf pmx
      py = prefixOf pmy
      my = bitmapOf pmy
      intersectionx
         | nomatch py px mx
         = Seed
         | zero py mx
         = intersection lx sy
         | otherwise
         = intersection rx sy
      intersectiony
         | nomatch px py my
         = Seed
         | zero px my
         = intersection sx ly
         | otherwise
         = intersection sx ry
intersection sx@Branch{} (Leaf py my) = intersectionBM py my sx
intersection Branch{} Seed = Seed
intersection (Leaf p m) sy = intersectionBM p m sy
intersection Seed _ = Seed


intersectionBM :: Prefix -> BitMap -> Tree -> Tree
intersectionBM p m sw = case sw of
   Branch pm' l r
      | nomatch p (prefixOf pm') (bitmapOf pm')
      -> Seed
      | zero p (bitmapOf pm')
      -> intersectionBM p m l
      | otherwise
      -> intersectionBM p m r
   Leaf p' m'
      | p == p'
      -> leaf p (m .&. m')
      | otherwise
      -> Seed
   Seed
      -> Seed

shorter :: BitMask -> BitMask -> Bool
{-^ The higher the mask bit, the shorter the prefix. -}
shorter mx my = mx > my

-- instance Bits (Set Word64) where
--    (.&.) = intersection
--    (.|.) = union
--    xor sx sy = union sx sy `difference` intersection sx sy
--    zeroBits = empty
--    bitSizeMaybe _ = Nothing
--    complement = xor (fromList [0..maxBound]) -- oh god don't do this


filter :: (Word64 -> Bool) -> Tree -> Tree
filter f sw = case sw of
   Branch pm l r
      -> branch pm (filter f l) (filter f r)
   Leaf p m
      -> leaf p (foldl'Bits (\ m' w -> if f w then m' .|. bitmapOf w else m') 0 p m)
   Seed
      -> Seed

------ Summarize Entire Sets ----

foldMapLeaves :: (Monoid b)=> (Prefix -> BitMap -> b) -> Tree -> b
foldMapLeaves f = loop
   where
      loop Seed = mempty
      loop (Leaf p m) = f p m
      loop (Branch _ l r) = loop l <> loop r
{-# INLINE foldMapLeaves #-}

foldMap ::
   forall b. (Monoid b)=> (Word64 -> b) -> Tree -> b
foldMap = foldMapLeaves . foldMapBits
{-# INLINE foldMap #-}
-- @foldMap@ tries to be a little smart. @foldMap@ 'Data.Monoid.Last' will only traverse the right 'Branch'es, although it will still traverse all the elements of a 'Leaf'.

foldr :: forall a. (Word64 -> a -> a) -> a -> Tree -> a
foldr f = foldrLeaves (\ p m z -> foldrBits f z p m)
{-# INLINE foldr #-}

foldrLeaves :: forall b. (Prefix -> BitMap -> b -> b) -> b -> Tree -> b
foldrLeaves f z sw = case sw of
   Seed -> z
   Leaf p m -> f p m z
   Branch _ l r -> foldrLeaves f (foldrLeaves f z r) l
{-# INLINE foldrLeaves #-}

foldlLeaves :: forall b. (b -> Prefix -> BitMap -> b) -> b -> Tree -> b
foldlLeaves f z sw = case sw of
   Seed -> z
   Leaf p m -> f z p m
   Branch _ l r -> foldlLeaves f (foldlLeaves f z l) r
{-# INLINE foldlLeaves #-}

foldl :: forall b. (b -> Word64 -> b) -> b -> Tree -> b
foldl f = foldlLeaves (\ z p m -> foldlBits f z p m) -- This helps the function inline. (e.g. 'lookupMax')
{-# INLINE foldl #-}

foldlBits :: forall a. (a -> Word64 -> a) -> a -> Prefix -> BitMap -> a
{-^ Fold over the Word64s in a BitMap lazily, from highest to lowest. -}
foldlBits f z p = loop
   where
      loop :: BitMap -> a
      loop 0 = z
      loop m = loop m' `f` x
         where
            !x = p + highestBitIndex m
            !m' = m .+. highestBit m
{-# INLINE foldlBits #-}

foldr'Leaves :: (Prefix -> BitMap -> b -> b) -> b -> Tree -> b
foldr'Leaves f = loop
   where
      loop !z sx = case sx of
         Seed -> z
         Leaf p m -> f p m z
         Branch _ l r -> loop (loop z r) l
{-# INLINE foldr'Leaves #-}

foldr' :: forall b. (Word64 -> b -> b) -> b -> Tree -> b
foldr' f = foldr'Leaves (\ p m z -> foldr'Bits f z p m)
{-# INLINE foldr' #-}

foldr'Bits :: forall a. (Word64 -> a -> a) -> a -> Prefix -> BitMap -> a
foldr'Bits f z p = loop z
    where
      loop :: a -> BitMap -> a
      loop acc 0 = acc
      loop !acc m = loop (f x acc) m'
          where
            !x = p + highestBitIndex m
            !m' = m .+. highestBit m
{-# INLINE foldr'Bits #-}

foldl'Leaves :: (b -> Prefix -> BitMap -> b) -> b -> Tree -> b
foldl'Leaves f = loop
   where
      loop !z sx = case sx of
         Seed -> z
         Leaf p m -> f z p m
         Branch _ l r -> loop (loop z l) r
{-# INLINE foldl'Leaves #-}

foldl' :: forall b. (b -> Word64 -> b) -> b -> Tree -> b
foldl' = foldl'Leaves . foldl'Bits
{-# INLINE foldl' #-}

foldl'Bits :: forall a. (a -> Word64 -> a) -> a -> Prefix -> BitMap -> a
{-^ Fold over all the Word64s in a BitMap strictly, from lowest to highest. -}
foldl'Bits f z p = loop z
    where
      loop :: a -> BitMap -> a
      loop acc 0 = acc
      loop !acc m = loop (f acc x) m'
          where
            !x = p + lowestBitIndex m
            !m' = m .+. lowestBit m
{-# INLINE foldl'Bits #-}


foldrBits :: forall a. (Word64 -> a -> a) -> a -> Prefix -> BitMap -> a
{-^ Fold over the Word64s in a BitMap lazily, from lowest to highest. -}
foldrBits f z p = loop
   where
      loop :: BitMap -> a
      loop 0 = z
      loop m = x `f` loop m'
         where
            !x = p + lowestBitIndex m
            !m' = m .+. lowestBit m
{-# INLINE foldrBits #-}

foldMapBits :: forall a. (Monoid a)=> (Word64 -> a) -> Prefix -> BitMap -> a
foldMapBits f = foldrBits ((<>) . f) mempty
{-# INLINE foldMapBits #-}


size :: Tree -> Word64
size = foldl'Leaves (\ z _ m -> z + fromIntegral (popCount m)) 0
{-# INLINE size #-}


splitMember :: Word64 -> Tree -> (Tree, Bool, Tree)
splitMember w sw = case sw of
   Branch pm l r
      | not (nomatch w pre bmp)
      -> if zero w bmp
         then case splitMember w l of (l', mmbr, r') -> (l', mmbr, union r' r)
         else case splitMember w r of (l', mmbr, r') -> (union l l', mmbr, r')
      | otherwise
      -> if w < pre
         then (empty, False, sw)
         else (sw, False, empty)
      where
         pre = prefixOf pm
         bmp = bitmapOf pm
   Leaf pre bmp
      | pre > prefixOf w -> (empty, False, sw)
      | pre < prefixOf w -> (sw, False, empty)
      | otherwise        -> let
            !mmbr = not (disjointBits bmp bmpw)
            !l = leaf pre (bmp .&. lowBmp)
            !r = leaf pre (bmp .&. highBmp)
            bmpw = bitmapOf w
            lowBmp = bmpw - 1
            highBmp = complement (lowBmp + bmpw)
         in (l, mmbr, r)
   Seed
      -> (Seed, False, Seed)
{-# INLINE splitMember #-}

------ Internal ------

bitmapOf :: Word64 -> BitIndex
bitmapOf = bitmapOfSuffix . suffixOf
   where
      -- bitmapOfSuffix = bit . word64ToInt
      bitmapOfSuffix = unsafeShiftL 1 . word64ToInt -- don't need safety here; suffix is at most 63.
{-# INLINE bitmapOf #-}

prefixOf :: Word64 -> Prefix
prefixOf = (.&.) prefixBitMask
   where
      prefixBitMask = complement suffixBitMask
suffixOf :: Word64 -> Word64
suffixOf = (.&.) suffixBitMask

suffixBitMask :: BitMask
suffixBitMask = intToWord64 (word64Size - 1) -- hopefully 63

nomatch :: Word64 -> Prefix -> BitIndex -> Bool
{-^ Does the prefix of the 'Word64' not match the given prefix up to the index? -}
nomatch x p m = p /= mask x m

mask :: Word64 -> Word64 -> Word64
{-^ @mask x m@ unsets all the bits in @x@ below the lowest bit in @m@. -}
mask x m = x .&. (m .+. negate m) -- m .+. negate m sets all the bits in m above the lowest set bit, and unsets the lowest set bit and all the bits below.

branch :: Word64 -> Tree -> Tree -> Tree
{-^ Combine two ordered disjoint sets, removing empty ones. -}
branch _ l Seed = l
branch _ Seed r = r
branch pm l r = Branch pm l r
{-# INLINE branch #-}

leaf :: Prefix -> BitMap -> Tree
{-^ Combine a 'Prefix' and 'BitMap' to create a new 'Set'. -}
leaf _ 0 = Seed
leaf p m = Leaf p m
{-# INLINE leaf #-}

link :: Prefix -> Tree -> Prefix -> Tree -> Tree
link px sx py sy = linkWithMask (branchMask px py) px sx sy
   where
      linkWithMask m px sx sy
         -- | zero px m = Branch (p .|. m) sx sy
         | zero px m = Branch (prefixWithIndex p m) sx sy
         | otherwise = Branch (prefixWithIndex p m) sy sx
         where
            p = mask px m
      branchMask px py = highestBitMask (px .+. py)
         where
            highestBitMask = highestBit

------ Bit Hacks ------

lowestBit :: Word64 -> Word64
{-^ @lowestBit x@ is the smallest set bit in @x@. -}
lowestBit x = x .&. negate x

lowestBitIndex :: Word64 -> Word64
{-^ @lowestBitIndex x@ is the index of the smallest set bit in @x@. -}
lowestBitIndex = intToWord64 . countTrailingZeros

highestBit :: Word64 -> Word64
highestBit = uncheckedBit . highestBitIndexInt
   where uncheckedBit = unsafeShiftL 1

highestBitIndex :: Word64 -> Word64
{-^ @highestBitIndex@ is the index of the largest set bit in @x@. -}
highestBitIndex = intToWord64 . highestBitIndexInt

highestBitIndexInt :: Word64 -> Int
{-^ @highestBitIndex@ is the index of the largest set bit in @x@. -}
highestBitIndexInt x = (word64Size - 1) - countLeadingZeros x

disjointBits :: Word64 -> Word64 -> Bool
{-^ Are the parameters disjoint? -}
disjointBits x y = x .&. y  ==  0

zero :: Word64 -> Word64 -> Bool
{-^ Are the parameters disjoint? -}
zero = disjointBits

(.+.) :: (Bits a)=> a -> a -> a
(.+.) = xor
infixl 6 .+.

------ Conversion ------

intToWord64 :: Int -> Word64
intToWord64 = fromIntegral -- `toEnum` throws an error on a negative Int.

word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral -- `fromEnum` throws an error on a large Word64.

word64Size :: Int
word64Size = finiteBitSize (undefined :: Word64) -- hopefully 64.
