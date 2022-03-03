{-# LANGUAGE GADTs #-}

module Data.Set.Int64.Spec where

import Data.Ord
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int (Int64)
import Data.List (sort, sortOn)
import Data.Set.Int64
import Test.QuickCheck
import Test.Hspec

instance (i64 ~ Int64)=> Arbitrary (Set i64) where
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
      "not member" `it` property prop_splitMember_not_member
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

   describe "read, show" $ do
      "read . show === id" `it` property prop_show_read


prop_equal_self :: Int64Set -> Bool
prop_equal_self sw = sw == sw
prop_toList_eq :: Int64Set -> Int64Set -> Bool
prop_toList_eq sx sy = (sx == sy) == (toList sx == toList sy)

isElem f sx = all (`f` sx) (toList sx)
prop_elem :: Int64Set -> Bool
prop_elem = isElem elem
prop_member_singleton w = elem w (singleton w)
prop_insert_elem x sx = elem x (insert x sx)
prop_delete_elem x sx = notElem x (delete x sx)
prop_insert_delete x sx =
   elem x sx' && -- insert inserts.
   notElem x sx'' && -- delete deletes.
   ((sx'' == sx)  /=  (sx' == sx)) -- insert only inserts and delete only deletes.
   where
      sx' = insert x sx
      sx'' = delete x sx'

prop_alterF_member w sw = getConst (alterF Const w sw) == elem w sw
prop_alterF_insert w sw = runIdentity (alterF (\_-> Identity True) w sw) == insert w sw
prop_alterF_delete w sw = runIdentity (alterF (\_-> Identity False) w sw) == delete w sw
prop_alterF_identity w sw = runIdentity (alterF Identity w sw) == sw

prop_union_commutative sx sy = union sx sy == union sy sx
prop_union_self si = union si si == si
prop_union_empty si = union empty si == si

prop_union :: Int64Set -> Int64Set -> Bool
prop_union = isUnion union
isUnion :: (Foldable m, Eq a)=> (m a -> m a -> m a) -> m a -> m a -> Bool
isUnion f sx sy = let sz = f sx sy in
   all (\ x -> elem x sx || elem x sy) sz &&
   all (`elem` sz) sx &&
   all (`elem` sz) sy


prop_intersection_commutative sx sy = intersection sx sy == intersection sy sx
prop_intersection_self si = intersection si si == si
prop_intersection_empty si = intersection empty si == mempty

prop_intersection :: Int64Set -> Int64Set -> Bool
prop_intersection = isIntersection intersection
isIntersection :: (Foldable m, Eq a)=> (m a -> m a -> m a) -> m a -> m a -> Bool
isIntersection f sx sy = let sz = f sx sy in
   all (\ x -> elem x sx && elem x sy) sz &&
   all (\ x -> elem x sz == elem x sy) sx &&
   all (\ x -> elem x sz == elem x sx) sy

prop_disjointUnion :: Int64Set -> Int64Set -> Bool
prop_disjointUnion = isDisjointUnion disjointUnion
isDisjointUnion f sx sy =
   all (\ x -> elem x sx /= elem x sy) sz &&
   all (\ x -> elem x sz /= elem x sy) sx &&
   all (\ x -> elem x sz /= elem x sx) sy
   where sz = f sx sy

prop_difference_self sx = difference sx sx == mempty
prop_difference_empty_1 sx = difference mempty sx == mempty
prop_difference_empty_2 sx = difference sx mempty == sx

prop_difference :: Int64Set -> Int64Set -> Bool
prop_difference = isDifference difference
isDifference :: (Foldable m, Eq a)=> (m a -> m a -> m a) -> m a -> m a -> Bool
isDifference f sx sy = let sz = f sx sy in
   all (\ x -> elem x sx && notElem x sy) sz  &&
   all (\ x -> elem x sz /= elem x sy) sx


prop_splitMember_member x sx = elem x sx == case splitMember x sx of (_, b, _) -> b
prop_splitMember_not_member x sx =
   case splitMember x sx of (sl, _, sg) -> not (elem x sl || elem x sg)
prop_splitMember_ordered x sx =
   case splitMember x sx of (sl, _, sg) -> all (x >) sl && all (x <) sg

prop_maxView_maximum sx = all ((maximum sx ==) . fst) (maxView sx)
prop_maxView_delete sx = all (\ (x, sx') -> sx' == delete x sx) (maxView sx)

prop_minView_minimum sx = all ((minimum sx ==) . fst) (minView sx)
prop_minView_delete sx = all (\ (x, sx') -> sx' == delete x sx) (minView sx)

prop_to_from_list_identity sx = sx == fromList (toDesList sx)

prop_toAscList_sorted sx = toAscList sx == sort (toAscList sx)
prop_toDesList_sorted sx = toDesList sx == sortOn Down (toAscList sx)

prop_foldl'_is_strict_foldl sx =
   foldl' (flip (:)) [] sx == toDesList sx

prop_foldr'_is_strict_foldr sx =
   foldr' (:) [] sx == toAscList sx

prop_minimum_default :: Int64Set -> Bool
prop_minimum_default sx =
  foldl' (\ z x -> maybe (Just x) (Just . min x) z) Nothing sx
  ==
  if null sx then Nothing else Just (minimum sx)

prop_maximum_default :: Int64Set -> Bool
prop_maximum_default sx =
  foldl' (\ z x -> maybe (Just x) (Just . max x) z) Nothing sx
  ==
  if null sx then Nothing else Just (maximum sx)

prop_null :: Int64Set -> Bool
prop_null sx = null sx == (sx == mempty)

prop_show_read :: Int64Set -> Bool
prop_show_read sx = read (show sx) == sx
