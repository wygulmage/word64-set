{-# LANGUAGE GADTs #-}

import Test.Hspec
import Test.QuickCheck
import Data.Foldable
import Data.Word (Word64)
import Data.Int (Int64)
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Set.Word64.Internal as Internal
import qualified Data.Set.Int64 as Int64
import qualified Data.Set.Word64 as Word64

main :: IO ()
main = hspec $ do
   describe "==" $ do
      "Word64Set sx == sx" `it` property prop_Word64_equal_self
      "Int64Set sx == sx" `it` property prop_Int64_equal_self

   describe "elem, insert, delete" $ do
      "Int64Set member x (insert x sx)" `it` property prop_Int64_member_insert
      "Word64Set member x (insert x sx)" `it` property prop_Word64_member_insert
      "Int64Set member x (delete x sx)" `it` property prop_Int64_member_delete
      "Word64Set member x (delete x sx)" `it` property prop_Word64_member_delete

   describe "alterF" $ do
      "Int64Set identity" `it` property prop_Int64_alterF_identity
      "Word64Set identity" `it` property prop_Word64_alterF_identity
      "Int64Set member" `it` property prop_Int64_alterF_member
      "Word64Set member" `it` property prop_Word64_alterF_member
      "Int64Set insert" `it` property prop_Int64_alterF_insert
      "Word64Set insert" `it` property prop_Word64_alterF_insert
      "Int64Set delete" `it` property prop_Int64_alterF_delete
      "Word64Set delete" `it` property prop_Word64_alterF_delete

   describe "union" $ do
      "Int64Set commutative" `it` property prop_Int64_union_commutative
      "Word64Set commutative" `it` property prop_Word64_union_commutative
      "Int64Set self" `it` property prop_Int64_union_self
      "Word64Set self" `it` property prop_Word64_union_self
      "Int64Set empty" `it` property prop_Int64_union_empty
      "Word64Set empty" `it` property prop_Word64_union_empty

   describe "intersection" $ do
      "Int64Set commutative" `it` property prop_Int64_intersection_commutative
      "Word64Set commutative" `it` property prop_Word64_intersection_commutative
      "Int64Set self" `it` property prop_Int64_intersection_self
      "Word64Set self" `it` property prop_Word64_intersection_self
      "Int64Set empty" `it` property prop_Int64_intersection_empty
      "Word64Set empty" `it` property prop_Word64_intersection_empty

   describe "difference" $ do
      "Int64Set self" `it` property prop_Int64_difference_self
      "Word64Set self" `it` property prop_Word64_difference_self
      "Int64Set difference empty" `it` property prop_Int64_difference_empty_1
      "Word64Set difference empty" `it` property prop_Word64_difference_empty_1
      "Int64Set (`difference` empty)" `it` property prop_Int64_difference_empty_2
      "Word64Set (`difference` empty)" `it` property prop_Word64_difference_empty_2


instance Arbitrary Internal.Tree where
   arbitrary = fmap Internal.fromList arbitrary

instance (w64 ~ Word64)=> Arbitrary (Word64.Set w64) where
   arbitrary = fmap Word64.fromList arbitrary

instance (i64 ~ Int64)=> Arbitrary (Int64.Set i64) where
   arbitrary = fmap Int64.fromList arbitrary

--- ==

prop_Int64_equal_self :: Int64.Int64Set -> Bool
prop_Int64_equal_self si = si == si

prop_Word64_equal_self :: Word64.Word64Set -> Bool
prop_Word64_equal_self sw = sw == sw

-- elem, insert, delete

prop_Word64_member_insert w sw = elem w (Word64.insert w sw)
prop_Int64_member_insert w sw = elem w (Int64.insert w sw)

prop_Word64_member_delete w sw = not (elem w (Word64.delete w sw))
prop_Int64_member_delete w sw = not (elem w (Int64.delete w sw))

prop_Word64_alterF_member w sw = getConst (Word64.alterF Const w sw) == elem w sw
prop_Int64_alterF_member w sw = getConst (Int64.alterF Const w sw) == elem w sw

prop_Word64_alterF_insert w sw = runIdentity (Word64.alterF (\_-> Identity True) w sw) == Word64.insert w sw
prop_Int64_alterF_insert w sw = runIdentity (Int64.alterF (\_-> Identity True) w sw) == Int64.insert w sw

prop_Word64_alterF_delete w sw = runIdentity (Word64.alterF (\_-> Identity False) w sw) == Word64.delete w sw
prop_Int64_alterF_delete w sw = runIdentity (Int64.alterF (\_-> Identity False) w sw) == Int64.delete w sw

prop_Word64_alterF_identity w sw = runIdentity (Word64.alterF Identity w sw) == sw
prop_Int64_alterF_identity w sw = runIdentity (Int64.alterF Identity w sw) == sw


--- union

prop_Int64_union_commutative sx sy = Int64.union sx sy == Int64.union sy sx
prop_Word64_union_commutative sx sy = Word64.union sx sy == Word64.union sy sx

prop_Int64_union_self si = Int64.union si si == si
prop_Word64_union_self si = Word64.union si si == si

prop_Int64_union_empty si = Int64.union si Int64.empty == si
prop_Word64_union_empty si = Word64.union Word64.empty si == si

--- intersection

prop_Int64_intersection_commutative sx sy = Int64.intersection sx sy == Int64.intersection sy sx
prop_Word64_intersection_commutative sx sy = Word64.intersection sx sy == Word64.intersection sy sx

prop_Int64_intersection_self si = Int64.intersection si si == si
prop_Word64_intersection_self si = Word64.intersection si si == si

prop_Int64_intersection_empty si = Int64.intersection si Int64.empty == Int64.empty
prop_Word64_intersection_empty si = Word64.intersection Word64.empty si == Word64.empty

--- difference

prop_Int64_difference_empty_1 sx = Int64.difference mempty sx == mempty
prop_Word64_difference_empty_1 sx = Word64.difference mempty sx == mempty

prop_Int64_difference_empty_2 sx = Int64.difference sx mempty == sx
prop_Word64_difference_empty_2 sx = Word64.difference sx mempty == sx

prop_Int64_difference_self sx = Int64.difference sx sx == mempty
prop_Word64_difference_self sx = Word64.difference sx sx == mempty

{- Note on pointless tests:
Always test the outward-facing API, even if it seems that checking only the internal API would be quicker and just as effective. Checking the outward-facing API will find 'impossible' mistakes like defining Word64.union to use Internal.intersection.
-}
