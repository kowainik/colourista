{-# LANGUAGE ImplicitParams #-}

module Test.Colourista
    ( colouristaSpec
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

import Colourista (formatWith, italic, red, reset, yellow)
import Colourista.Short (b, i, u)


colouristaSpec :: Spec
colouristaSpec = describe "Colourista tests" $ do
    describe "Colour codes are actual strings" $ do
        it "Yellow: String" $
            yellow @String `shouldBe` "\ESC[93m"
        it "Yellow: Text" $
            yellow @Text `shouldBe` "\ESC[93m"
        it "Yellow: ByteString" $
            yellow @ByteString `shouldBe` "\ESC[93m"
        it "Reset: Text" $
            reset @Text `shouldBe` "\ESC[0m"

    describe "Colourista.Short" $ do
        it "Bold" $
            b @Text "bold" `shouldBe` "\ESC[1mbold\ESC[0m"
        it "Italic" $
            i @Text "italic" `shouldBe` "\ESC[3mitalic\ESC[0m"
        it "Underline" $
            u @Text "underline" `shouldBe` "\ESC[4munderline\ESC[0m"

    describe "'formatWith' works" $ do
        it "Format with empty list" $
            formatWith @Text [] "text" `shouldBe` "text"
        it "Format with red italic" $
            formatWith @Text [red, italic] "text" `shouldBe` "\ESC[91m\ESC[3mtext\ESC[0m"
