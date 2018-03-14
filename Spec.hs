import Evaluator
import Test.Hspec

-- If you use different file names for each example be sure to change them
-- accordingly in the tests

spec :: Spec
spec = do
       describe "A: Conjunction" $ do
        it "A1" $ do
            output <- testInterpret "(for 1,2 in A1A.csv) & (for 3,4 in A1B.csv) do 1,3,2,4"
            output `shouldBe` "Pawel, Julian, Sobocinski, Rathke, \n"
        it "A2" $ do
            output <- testInterpret "(for 1,2 in A2A.csv) & (for 3,4 in A2B.csv) do 1,3,2,4"
            output `shouldBe` "1, 3, 2, 4, \n1, 3, 2, 4, \n1, 3, 2, 4, \n1, 3, 2, 4, \n"
        it "A3" $ do
            output <- testInterpret "(for 1,2 in A3A.csv) & (for 3,4 in A3B.csv) do 1,3,2,4"
            output `shouldBe` ""
       describe "B: Conjunction & Variable Repetition" $ do
        it "B1" $ do
            output <- testInterpret "(for 1,2 in B1A.csv) & (for 2,3 in B1B.csv) do 1,2,3"
            output `shouldBe` "Guido, Carillo, Ferrari, \nSofiane, Boufal, Maserati, \n"
        it "B2" $ do
            output <- testInterpret "(for 1,2 in B2A.csv) & (for 2,3 in B2B.csv) do 1,2,3"
            output `shouldBe` "1, 2, 2, \n1, 3, 1, \n1, 3, 2, \n"
        it "B3" $ do
            output <- testInterpret "(for 1,2 in B3A.csv) & (for 2,3 in B3B.csv) do 1,2,3"
            output `shouldBe` "1, 2, 1, \n1, 2, 1, \n1, 2, 1, \n1, 2, 1, \n"
       describe "C: Equality" $ do
        it "C1" $ do
            output <- testInterpret "((for 1 in C1A.csv) & (for 2 in C1B.csv)) & (1=2) do 1,2"
            output `shouldBe` "1, 1, \n2, 2, \n2, 2, \n"
        it "C2" $ do
            output <- testInterpret "((for 1 in C2A.csv) & (for 2 in C2B.csv)) & (1=2) do 1,2"
            output `shouldBe` "Alice, Alice, \nBob, Bob, \n"
        it "C3" $ do
            output <- testInterpret "((for 1 in C3A.csv) & (for 2 in C3B.csv)) & (1=2) do 1,2"
            output `shouldBe` ""
       describe "D: Existantial Quantification" $ do
        it "D1" $ do
            output <- testInterpret "ifexist 2 in (for 1,2 in D1A.csv) do 1"
            output `shouldBe` "Antonello, \nLeonardo, \nMichelangelo, \n"
        it "D2" $ do
            output <- testInterpret "ifexist 2 in (for 1,2 in D2A.csv) do 1"
            output `shouldBe` "Jolion, \nJulian, \nJulian, \nJulian, \n"
       describe "E: Existantial Quantification & Conjuction" $ do
        it "E1" $ do
            output <- testInterpret "ifexist 2 in ((for 1,2 in E1A.csv) & (for 2,3 in E1B.csv)) do 1,3"
            output `shouldBe` "Guido, Ferrari, \nSofiane, Maserati, \n"
        it "E2" $ do
            output <- testInterpret "ifexist 2 in ((for 1,2 in E2A.csv) & (for 2,3 in E2B.csv)) do 1,3"
            output `shouldBe` "1, 1, \n1, 2, \n1, 2, \n"
        it "E3" $ do
            output <- testInterpret "ifexist 2 in ((for 1,2 in E3A.csv) & (for 2,3 in E3B.csv)) do 1,3"
            output `shouldBe` "1, 1, \n1, 1, \n1, 1, \n1, 1, \n"



main :: IO ()
main = hspec spec


