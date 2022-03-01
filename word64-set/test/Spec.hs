{-# LANGUAGE GADTs #-}

import Test.Hspec
import Test.QuickCheck
import Data.Set.Word64.Spec

main :: IO ()
main = hspec $ do
   describe "==" $ do
      describe "sx == sx" $ do
         "Word64Set sx == sx" `it` property prop_equal_self
      describe "(toList sx == toList sy) == (sx == sy)" $ do
         "Word64Set" `it` property prop_toList_eq

   describe "elem, insert, delete" $ do
      describe "member singleton" $ do
         "Word64" `it` property prop_member_singleton
      describe "member x (insert x sx)" $ do
         "Word64Set" `it` property prop_member_insert
      describe "not (member x (delete x sx))" $ do
         "Word64Set" `it` property prop_member_delete

   describe "alterF" $ do
      describe "identity" $ do
         "Word64Set" `it` property prop_alterF_identity
      describe "member" $ do
         "Word64Set" `it` property prop_alterF_member
      describe "insert" $ do
         "Word64Set" `it` property prop_alterF_insert
      describe "delete" $ do
         "Word64Set" `it` property prop_alterF_delete

   describe "union" $ do
      describe "commutative" $ do
         "Word64Set" `it` property prop_union_commutative
      describe "self" $ do
         "Word64Set" `it` property prop_union_self
      describe "empty" $ do
         "Word64Set" `it` property prop_union_empty

   describe "intersection" $ do
      describe "commutative" $ do
         "Word64Set" `it` property prop_intersection_commutative
      describe "self" $ do
         "Word64Set" `it` property prop_intersection_self
      describe "empty" $ do
         "Word64Set" `it` property prop_intersection_empty

   describe "difference" $ do
      describe "self" $ do
         "Word64Set" `it` property prop_difference_self
      describe "difference empty" $ do
         "Word64Set" `it` property prop_difference_empty_1
      describe "(`difference` empty)" $ do
         "Word64Set" `it` property prop_difference_empty_2

   describe "splitMember" $ do
      describe "member" $ do
         "Word64" `it` property prop_splitMember_member
      describe "not member" $ do
         "Word64" `it` property prop_splitMember_not_member
      "ordered" `it` property prop_splitMember_ordered

   describe "to/from list" $ do
      describe "identity" $ do
         "Word64" `it` property prop_to_from_list_identity

   describe "toAscList" $ do
      describe "sorted" $ do
         "Word64" `it` property prop_toAscList_sorted

   describe "toDesList" $ do
      describe "sorted" $ do
         "Word64" `it` property prop_toDesList_sorted

   describe "maximum" $ do
      describe "default" $ do
         "Word64" `it` property prop_maximum_default

   describe "minimum" $ do
      describe "default" $ do
         "Word64" `it` property prop_minimum_default

   describe "null" $ do
      "Word64" `it` property prop_null

   describe "read, show" $ do
      describe "read . show === id" $ do
         "Word64" `it` property prop_show_read
