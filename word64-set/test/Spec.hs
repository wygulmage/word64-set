import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Set.Word64.Spec

main :: IO ()
main = hspec
   -- $ modifyMaxSuccess (const 1000)
   $ modifyMaxSize (const 10000)
   spec
