{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Breeze as B
import Breeze (defaultOptions, Options(..))
import Control.Exception as CE

main :: IO ()
main = hspec $ do
    describe "rendering" $ do
        it "parses nodes" $ do
            B.render "<b></b>" defaultOptions `shouldBe` "HE.b [] []"

        it "parses text" $ do
            B.render "hi" defaultOptions `shouldBe` "HE.text \"hi\""

        it "parses custom nodes" $ do
            B.render "<hi></hi>" defaultOptions `shouldBe` "HE.createElement \"hi\" [] []"

        it "parses void nodes" $ do
            B.render "<input>" defaultOptions `shouldBe` "HE.input []"

        it "parses nodes without attributes" $ do
            B.render "<br>" defaultOptions `shouldBe` "HE.br "
            B.render "<hr>" defaultOptions `shouldBe` "HE.hr "

        it "parses quoted attributes" $ do
            B.render "<b srclang=\"abc\"></b>" defaultOptions `shouldBe` "HE.b [HA.srclang \"abc\"] []"

        it "parses custom attributes" $ do
            B.render "<a abc=\"abc\"></a>" defaultOptions `shouldBe` "HE.a [HA.createAttribute \"abc\" \"abc\"] []"

        it "parses unquoted attributes" $ do
            B.render "<b style=\"abc\"></b>" defaultOptions `shouldBe` "HE.b [HA.style abc] []"

        it "parses kebab case attributes" $ do
            B.render "<b strikethrough-position=\"4\"></b>" defaultOptions `shouldBe` "HE.b [HA.strikethroughPosition \"4\"] []"

        it "parses attribute list" $ do
            B.render "<b strikethrough-position=\"4\" src=\"oi\"></b>" defaultOptions `shouldBe` "HE.b [HA.strikethroughPosition \"4\", HA.src \"oi\"] []"

        it "errors on malformed nodes" $ do
            CE.evaluate (B.render "<b>" defaultOptions) `shouldThrow` errorCall "Unmatched tag: b"

        it "parses children nodes" $ do
            B.render "<div class=\"c\"><b></b><a>my link</a></div>" defaultOptions `shouldBe` "HE.div [HA.class \"c\"] [HE.b [] [], HE.a [] [HE.text \"my link\"]]"
            B.render "<html><head></head><body><div class=\"c\"><b></b><a>my link</a></div><article>hey</article><div></div></body></html>" defaultOptions `shouldBe` "HE.html [] [HE.head [] [], HE.body [] [HE.div [HA.class \"c\"] [HE.b [] [], HE.a [] [HE.text \"my link\"]], HE.article [] [HE.text \"hey\"], HE.div [] []]]"

    describe "options" $ do
        it "ignores errors" $ do
            B.render "<b>" defaultOptions {ignoreErrors  = True} `shouldBe` "HE.b []"

        it "standalone module" $ do
            B.render "<div><b style=\"abc:def\">oi</b><a>dd</a></div" defaultOptions{standaloneModule = True} `shouldBe` "module Breeze.Output where\n\nimport Flame (Html)\nimport Flame.Html.Element as HE\nimport Flame.Html.Attribute as HA\n\nview :: forall model message. model -> Html message\nview model = HE.div [] [HE.b [HA.style abc:def] [HE.text \"oi\"], HE.a [] [HE.text \"dd\"]]"