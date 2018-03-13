import Evaluator
import Test.Hspec

spec :: Spec
spec = do
       describe "Main problems" $ do
            it "Outputs the given input" $ do
                output <- interpret "for 1,2,3 in A.csv do 1,2,3"
                output `shouldBe` ""
            it "Solves the first exercice - Conjunction" $ do
                output <- interpret "(for 1,2 in A.csv) & (for 3,4 in B.csv) do 1,3,2,4"
                output `shouldBe` ""
            it "Solves the second exercice - Conjunction and variable repetition" $ do
                output <- interpret "(for 1,3 in A.csv) & (for 3,1 in B.csv) do 1,3,1"
                output `shouldBe` ""
            it "Solves the third exercice - Equality" $ do
                output <- interpret "(for 1,2 in A.csv) & (for 3,4 in B.csv) do 1,3,2,4"
                output `shouldBe` ""
            it "Solves the fourth exercice - Existantial quantification" $ do
                output <- interpret "(for 1,2 in A.csv) & (for 3,4 in B.csv) do 1,3,2,4"
                output `shouldBe` ""
            it "Solves the fifth exercice - Existantial quantification and conjunction" $ do
                output <- interpret "(for 1,2 in A.csv) & (for 3,4 in B.csv) do 1,3,2,4"
                output `shouldBe` ""


main :: IO ()
main = hspec spec


