{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Breeze as B
import Breeze (defaultOptions)

main :: IO ()
main = hspec $ do
    describe "render" $ do
        it "parses nodes" $ do
            B.render "<b></b>" defaultOptions `shouldBe` "HE.b [] []"