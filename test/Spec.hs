{-# LANGUAGE GADTs #-}

import Test.QuickCheck
import Data.Foldable
import Data.Word (Word64)
import Data.Int (Int64)
import qualified Data.Set.Word64.Internal as Internal
import qualified Data.Set.Int64 as Int64
import qualified Data.Set.Word64 as Word64

main :: IO ()
main = do
   quickCheck prop_Word64_equal_self
   quickCheck prop_Int64_equal_self
   quickCheck prop_Word64_union_commutative
   quickCheck prop_Int64_union_commutative
   quickCheck prop_Word64_union_self
   quickCheck prop_Int64_union_self
   quickCheck prop_Word64_union_empty
   quickCheck prop_Int64_union_empty
   quickCheck prop_Int64_intersection_commutative
   quickCheck prop_Word64_intersection_commutative


instance Arbitrary Internal.Tree where
   arbitrary = fmap Internal.fromList arbitrary

instance (w64 ~ Word64)=> Arbitrary (Word64.Set w64) where
   arbitrary = fmap Word64.fromList arbitrary

instance (i64 ~ Int64)=> Arbitrary (Int64.Set i64) where
   arbitrary = fmap Int64.fromList arbitrary

prop_Int64_equal_self :: Int64.Int64Set -> Bool
prop_Int64_equal_self si = si == si

prop_Word64_equal_self :: Word64.Word64Set -> Bool
prop_Word64_equal_self sw = sw == sw

prop_Int64_union_commutative sx sy = Int64.union sx sy == Int64.union sy sx
prop_Word64_union_commutative sx sy = Word64.union sx sy == Word64.union sy sx

prop_Int64_union_self si = Int64.union si si == si
prop_Word64_union_self si = Word64.union si si == si

prop_Int64_union_empty si = Int64.union si Int64.empty == si
prop_Word64_union_empty si = Word64.union Word64.empty si == si

prop_Int64_intersection_commutative sx sy = Int64.intersection sx sy == Int64.intersection sy sx

prop_Word64_intersection_commutative sx sy = Word64.intersection sx sy == Word64.intersection sy sx

{- Note on pointless tests:
Always test the outward-facing API, even if it seems that checking only the internal API would be quicker and just as effective. Checking the outward-facing API will find 'impossible' mistakes like defining Word64.union to use Internal.intersection.
-}
