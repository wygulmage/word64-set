{-# LANGUAGE GADTs #-}

import Test.Hspec
import Test.QuickCheck
import Data.Foldable
import Data.Word (Word64)
import Data.Int (Int64)
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Set.Int64 as Int64
import qualified Data.Set.Int64.Spec as I64

main :: IO ()
main = hspec $ do
   describe "==" $ do
      describe "sx == sx" $ do
         "Int64Set sx == sx" `it` property I64.prop_equal_self
      describe "(toList sx == toList sy) == (sx == sy)" $ do
         "Int64Set" `it` property I64.prop_toList_eq

   describe "elem, insert, delete" $ do
      describe "member singleton" $ do
         "Int64" `it` property I64.prop_member_singleton
      describe "member x (insert x sx)" $ do
         "Int64Set" `it` property I64.prop_member_insert
      describe "not (member x (delete x sx))" $ do
         "Int64Set" `it` property I64.prop_member_delete

   describe "alterF" $ do
      describe "identity" $ do
         "Int64Set" `it` property I64.prop_alterF_identity
      describe "member" $ do
         "Int64Set" `it` property I64.prop_alterF_member
      describe "insert" $ do
         "Int64Set" `it` property I64.prop_alterF_insert
      describe "delete" $ do
         "Int64Set" `it` property I64.prop_alterF_delete

   describe "union" $ do
      describe "commutative" $ do
         "Int64Set" `it` property I64.prop_union_commutative
      describe "self" $ do
         "Int64Set" `it` property I64.prop_union_self
      describe "empty" $ do
         "Int64Set" `it` property I64.prop_union_empty

   describe "intersection" $ do
      describe "commutative" $ do
         "Int64Set" `it` property I64.prop_intersection_commutative
      describe "self" $ do
         "Int64Set" `it` property I64.prop_intersection_self
      describe "empty" $ do
         "Int64Set" `it` property I64.prop_intersection_empty

   describe "difference" $ do
      describe "self" $ do
         "Int64Set" `it` property I64.prop_difference_self
      describe "difference empty" $ do
         "Int64Set" `it` property I64.prop_difference_empty_1
      describe "(`difference` empty)" $ do
         "Int64Set" `it` property I64.prop_difference_empty_2

   describe "splitMember" $ do
      describe "member" $ do
         "Int64" `it` property I64.prop_splitMember_member
      describe "not member" $ do
         "Int64" `it` property I64.prop_splitMember_not_member

   describe "to/from list" $ do
      describe "identity" $ do
         "Int64" `it` property I64.prop_to_from_list_identity

   describe "toAscList" $ do
      describe "sorted" $ do
         "Int64" `it` property I64.prop_toAscList_sorted

   describe "toDesList" $ do
      describe "sorted" $ do
         "Int64" `it` property I64.prop_toDesList_sorted

   describe "maximum" $ do
      describe "default" $ do
         "Int64" `it` property I64.prop_maximum_default

   describe "minimum" $ do
      describe "default" $ do
         "Int64" `it` property I64.prop_minimum_default

   describe "null" $ do
      "Int64" `it` property I64.prop_null

   describe "read, show" $ do
      describe "read . show === id" $ do
         "Int64" `it` property I64.prop_show_read

