import Evaluator
import Test.Hspec

-- If you use different file names for each example be sure to change them
-- accordingly in the tests

spec :: Spec
spec = do
       describe "1: Conjunction" $ do
        it "1a" $ do
            output <- testInterpret "(for 1,2 in A.csv) & (for 3,4 in B.csv) do 1,3,2,4"
            output `shouldBe` "Pawel, Julian, Sobocinski, Rathke, "
        it "1b" $ do
            output <- testInterpret "(for 1,2 in A.csv) & (for 3,4 in B.csv) do 1,3,2,4"
            output `shouldBe` "1, 3, 2, 4, \n1, 3, 2, 4, \n1, 3, 2, 4, \n1, 3, 2, 4, \n"
        it "1c" $ do
            output <- testInterpret "(for 1,2 in A.csv) & (for 3,4 in B.csv) do 1,3,2,4"
            output `shouldBe` ""
       describe "2: Conjunction & Variable Repetition" $ do
        it "2a" $ do
            output <- testInterpret "(for 1,2 in A.csv) & (for 2,3 in B.csv) do 1,2,3"
            output `shouldBe` "Guido, Carillo, Ferrari, \nSofiane, Boufal, Maseratti, \n"
        it "2b" $ do
            output <- testInterpret "(for 1,2 in A.csv) & (for 2,3 in B.csv) do 1,2,3"
            output `shouldBe` "1, 2, 2, \n1, 3, 1, \n1, 3, 2, \n"
        it "2c" $ do
            output <- testInterpret "(for 1,2 in A.csv) & (for 2,3 in B.csv) do 1,2,3"
            output `shouldBe` "1, 2, 1, \n1, 2, 1, \n1, 2, 1, \n1, 2, 1, \n"
       describe "3: Equality" $ do
       -- Not sure about this one, is it really (1=1)?
        it "3a" $ do
            output <- testInterpret "(for 1 in A.csv) & (for 1 in B.csv) & (1=1) do 1,1"
            output `shouldBe` "1, 1, \n2, 2, \n2, 2, \n"
        it "3b" $ do
            output <- testInterpret "(for 1 in A.csv) & (for 1 in B.csv) & (1=1) do 1,1"
            output `shouldBe` "Alice, Alice, \nBob, Bob, \n"
        it "3c" $ do
            output <- testInterpret "(for 1 in A.csv) & (for 1 in B.csv) & (1=1) do 1,1"
            output `shouldBe` ""
--        describe "4: Existantial Quantification" $ do
--        describe "5: Existantial Quantification & Conjuction" $ do



main :: IO ()
main = hspec spec


