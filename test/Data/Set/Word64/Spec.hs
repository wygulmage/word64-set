{-# LANGUAGE GADTs #-}

module Data.Set.Word64.Spec where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Word (Word64)
import Data.Set.Word64
import Test.QuickCheck

instance (w64 ~ Word64)=> Arbitrary (Set w64) where
   arbitrary = fmap fromList arbitrary

prop_equal_self :: Word64Set -> Bool
prop_equal_self sw = sw == sw

prop_member_singleton w = elem w (singleton w)

prop_member_insert w sw = elem w (insert w sw)

prop_member_delete w sw = not (elem w (delete w sw))

prop_alterF_member w sw = getConst (alterF Const w sw) == elem w sw
prop_alterF_insert w sw = runIdentity (alterF (\_-> Identity True) w sw) == insert w sw
prop_alterF_delete w sw = runIdentity (alterF (\_-> Identity False) w sw) == delete w sw
prop_alterF_identity w sw = runIdentity (alterF Identity w sw) == sw

prop_union_commutative sx sy = union sx sy == union sy sx
prop_union_self si = union si si == si
prop_union_empty si = union empty si == si


prop_intersection_commutative sx sy = intersection sx sy == intersection sy sx
prop_intersection_self si = intersection si si == si
prop_intersection_empty si = intersection empty si == mempty


prop_difference_self sx = difference sx sx == mempty
prop_difference_empty_1 sx = difference mempty sx == mempty
prop_difference_empty_2 sx = difference sx mempty == sx

prop_to_from_list_identity sx = sx == fromList (toDesList sx)
