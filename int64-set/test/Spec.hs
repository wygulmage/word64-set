{-# LANGUAGE GADTs #-}

import Test.Hspec
import Test.QuickCheck
import Data.Set.Int64.Spec

main :: IO ()
main = hspec $ do
   describe "==" $ do
      "sx == sx" `it` property prop_equal_self
      "(toList sx == toList sy) == (sx == sy)" `it` property prop_toList_eq

   describe "elem, insert, delete" $ do
      "member singleton" `it` property prop_member_singleton
      "member x (insert x sx)" `it` property prop_member_insert
      "not (member x (delete x sx))" `it` property prop_member_delete

   describe "alterF" $ do
      describe "identity" $ do
         "Int64Set" `it` property prop_alterF_identity
      describe "member" $ do
         "Int64Set" `it` property prop_alterF_member
      describe "insert" $ do
         "Int64Set" `it` property prop_alterF_insert
      describe "delete" $ do
         "Int64Set" `it` property prop_alterF_delete

   describe "union" $ do
      describe "commutative" $ do
         "Int64Set" `it` property prop_union_commutative
      describe "self" $ do
         "Int64Set" `it` property prop_union_self
      describe "empty" $ do
         "Int64Set" `it` property prop_union_empty

   describe "intersection" $ do
      describe "commutative" $ do
         "Int64Set" `it` property prop_intersection_commutative
      describe "self" $ do
         "Int64Set" `it` property prop_intersection_self
      describe "empty" $ do
         "Int64Set" `it` property prop_intersection_empty

   describe "difference" $ do
      "difference sx sx = empty" `it` property prop_difference_self
      "difference empty" `it` property prop_difference_empty_1
      "(`difference` empty)" `it` property prop_difference_empty_2

   describe "splitMember" $ do
      "member" `it` property prop_splitMember_member
      "not member" `it` property prop_splitMember_not_member
      "ordered" `it` property prop_splitMember_ordered

   describe "to/from list" $ do
      "identity" `it` property prop_to_from_list_identity

   describe "toAscList" $ do
      describe "sorted" $ do
         "Int64" `it` property prop_toAscList_sorted

   describe "toDesList" $ do
      describe "sorted" $ do
         "Int64" `it` property prop_toDesList_sorted

   describe "maximum" $ do
      describe "default" $ do
         "Int64" `it` property prop_maximum_default

   describe "minimum" $ do
      describe "default" $ do
         "Int64" `it` property prop_minimum_default

   describe "null" $ do
      "Int64" `it` property prop_null

   describe "read, show" $ do
      describe "read . show === id" $ do
         "Int64" `it` property prop_show_read
