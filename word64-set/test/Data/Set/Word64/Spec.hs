{-# LANGUAGE GADTs #-}

module Data.Set.Word64.Spec where

import Data.Ord
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Word (Word64)
import Data.List (sort, sortOn)
import Data.Set.Word64
import Test.QuickCheck
import Test.Hspec

instance (w64 ~ Word64)=> Arbitrary (Set w64) where
   arbitrary = fmap fromList arbitrary

spec :: SpecWith ()
spec = do
   describe "==" $ do
      "sx == sx" `it` property prop_equal_self
      "(toList sx == toList sy) == (sx == sy)" `it` property prop_toList_eq

   describe "elem, insert, delete" $ do
      "elem is elem" `it` property prop_elem
      "member singleton" `it` property prop_member_singleton
      "delete deletes" `it` property prop_delete_elem
      "delete x . insert x = id" `it` property prop_insert_delete

   describe "alterF" $ do
      "identity" `it` property prop_alterF_identity
      "member" `it` property prop_alterF_member
      "insert" `it` property prop_alterF_insert
      "delete" `it` property prop_alterF_delete

   describe "union" $ do
      "`union sx sy` is the set of all elements of `sx` and all elements of `sy`" `it` property prop_union

   describe "intersection" $ do
      "`intersection sx sy` is the set of all elements that are shared between `sx` and `sy`" `it` property prop_intersection

   describe "disjointUnion" $ do
      "disjointUnion sx sy is the set of all elements that are in either sx or sy but not both sx and sy." `it` property prop_disjointUnion

   describe "difference" $ do
      "is difference" `it` property prop_difference

   describe "splitMember" $ do
      "member" `it` property prop_splitMember_member
      "ordered" `it` property prop_splitMember_ordered

   describe "maxView" $ do
      "returns maximum" `it` property prop_maxView_maximum
      "deletes maximum" `it` property prop_maxView_delete

   describe "minView" $ do
      "returns minimum" `it` property prop_minView_minimum
      "deletes minimum" `it` property prop_minView_delete

   describe "to/from list" $ do
      "identity" `it` property prop_to_from_list_identity

   describe "toAscList" $ do
      "sorted" `it` property prop_toAscList_sorted

   describe "toDesList" $ do
      "sorted" `it` property prop_toDesList_sorted

   describe "maximum" $ do
      "default" `it` property prop_maximum_default

   describe "minimum" $ do
      "default" `it` property prop_minimum_default

   describe "null" $ do
      "is empty" `it` property prop_null

   describe "foldl'" $ do
      "is strict foldl" `it` property prop_foldl'_is_strict_foldl

   describe "foldr'" $ do
      "is strict foldr" `it` property prop_foldr'_is_strict_foldr

   describe "size" $ do
      "is the number of elements in the Set" `it` property prop_size

   describe "read, show" $ do
      "read . show === id" `it` property prop_show_read


--- Show, Read
prop_show_read :: Word64Set -> Bool
prop_show_read sx = read (show sx) == sx

--- Eq
prop_equal_self :: Word64Set -> Bool
prop_equal_self sx = sx == sx
prop_toList_eq :: Word64Set -> Word64Set -> Bool
prop_toList_eq sx sy = (sx == sy) == (toList sx == toList sy)

--- IsList
prop_to_from_list_identity sx = sx == fromList (toDesList sx)

------ Foldable
isElem f sx = all (`f` sx) (toList sx)
prop_elem :: Word64Set -> Bool
prop_elem = isElem elem

prop_toAscList_sorted sx = toAscList sx == sort (toAscList sx)
prop_toDesList_sorted sx = toDesList sx == sortOn Down (toAscList sx)

prop_foldl'_is_strict_foldl sx =
   foldl' (flip (:)) [] sx == toDesList sx

prop_foldr'_is_strict_foldr sx =
   foldr' (:) [] sx == toAscList sx

mayFoldl1' :: (Foldable m)=> (a -> a -> a) -> m a -> Maybe a
mayFoldl1' f = foldl' (\ z x -> maybe (Just x) (Just . f x) z) Nothing

prop_minimum_default :: Word64Set -> Bool
prop_minimum_default sx =
  mayFoldl1' min sx
  ==
  if null sx then Nothing else Just (minimum sx)

prop_maximum_default :: Word64Set -> Bool
prop_maximum_default sx =
  mayFoldl1' max sx
  ==
  if null sx then Nothing else Just (maximum sx)

prop_null :: Word64Set -> Bool
prop_null sx = null sx == (sx == mempty)

prop_size :: Word64Set -> Bool
prop_size sx = size sx == foldl' (\ n _ -> n + 1) 0 sx

------ singleton, insert, delete
prop_member_singleton x = elem x (singleton x)
prop_insert_elem x sx = elem x (insert x sx)
prop_delete_elem x sx = notElem x (delete x sx)
prop_insert_delete x sx =
   elem x sx' && -- insert inserts.
   notElem x sx'' && -- delete deletes.
   ((sx'' == sx)  /=  (sx' == sx)) -- insert only inserts and delete only deletes.
   where
      sx' = insert x sx
      sx'' = delete x sx'

------ alterF
prop_alterF_member x sx = getConst (alterF Const x sx) == elem x sx
prop_alterF_insert x sx = runIdentity (alterF (\_-> Identity True) x sx) == insert x sx
prop_alterF_delete x sx = runIdentity (alterF (\_-> Identity False) x sx) == delete x sx
prop_alterF_identity x sx = runIdentity (alterF Identity x sx) == sx


------ union, intersection, disjointUnion, difference
prop_union :: Word64Set -> Word64Set -> Bool
prop_union = isUnion union
isUnion :: (Foldable m, Eq a)=> (m a -> m a -> m a) -> m a -> m a -> Bool
isUnion f sx sy = let sz = f sx sy in
   all (\ x -> elem x sx || elem x sy) sz &&
   all (`elem` sz) sx &&
   all (`elem` sz) sy

prop_intersection :: Word64Set -> Word64Set -> Bool
prop_intersection = isIntersection intersection
isIntersection :: (Foldable m, Eq a)=> (m a -> m a -> m a) -> m a -> m a -> Bool
isIntersection f sx sy = let sz = f sx sy in
   all (\ x -> elem x sx && elem x sy) sz &&
   all (\ x -> elem x sz /= notElem x sy) sx &&
   all (\ x -> elem x sz /= notElem x sx) sy

prop_disjointUnion :: Word64Set -> Word64Set -> Bool
prop_disjointUnion = isDisjointUnion disjointUnion
isDisjointUnion f sx sy =
   all (\ x -> elem x sx /= elem x sy) sz &&
   all (\ x -> elem x sz /= elem x sy) sx &&
   all (\ x -> elem x sz /= elem x sx) sy
   where sz = f sx sy

prop_difference :: Word64Set -> Word64Set -> Bool
prop_difference = isDifference difference
isDifference :: (Foldable m, Eq a)=> (m a -> m a -> m a) -> m a -> m a -> Bool
isDifference f sx sy = let sz = f sx sy in
   all (\ x -> elem x sx && notElem x sy) sz  &&
   all (\ x -> elem x sz /= elem x sy) sx


--- splitMember, maxView, minView
prop_splitMember_member x sx = elem x sx == case splitMember x sx of (_, b, _) -> b
prop_splitMember_ordered x sx =
   case splitMember x sx of (sl, _, sg) -> all (x >) sl && all (x <) sg

prop_maxView_maximum sx = all ((maximum sx ==) . fst) (maxView sx)
prop_maxView_delete sx = all (\ (x, sx') -> sx' == delete x sx) (maxView sx)

prop_minView_minimum sx = all ((minimum sx ==) . fst) (minView sx)
prop_minView_delete sx = all (\ (x, sx') -> sx' == delete x sx) (minView sx)
