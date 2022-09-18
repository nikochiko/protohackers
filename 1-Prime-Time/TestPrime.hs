module TestPrime where

import Test.Hspec
import Prime

main :: IO ()
main = hspec $ do
    describe "Prime" $ do
        it "1 is not prime" $ do
            isPrime 1 `shouldBe` False

        it "2 is Prime" $ do
            isPrime 2 `shouldBe` True

        it "7 is Prime" $ do
            isPrime 7 `shouldBe` True

        it "307 is Prime" $ do
            isPrime 307 `shouldBe` True

        it "4133 is Prime" $ do
            isPrime 4133 `shouldBe` True

        it "4 is not Prime" $ do
            isPrime 4 `shouldBe` False

        it "9 is not Prime" $ do
            isPrime 4 `shouldBe` False

        it "4137 is not Prime" $ do
            isPrime 4137 `shouldBe` False
