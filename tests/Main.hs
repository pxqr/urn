{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Data.URN
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "parsing" $ do
    it "should be case-insensitive ('urn:' part)" $ do
      parseURN "URN:foo:a123,456"
        `shouldBe` Just (URN "foo" "a123,456")
      parseURN "urn:foo:a123,456"
        `shouldBe` Just (URN "foo" "a123,456")

    it "should be case-insensitive (NID part)" $ do
      parseURN "urn:foo:a123,456"
        `shouldBe` Just (URN "foo" "a123,456")
      parseURN "urn:FOO:a123,456"
        `shouldBe` Just (URN "foo" "a123,456")

    it "should be case-sensitive (NSS part)" $ do
      parseURN "urn:foo:AbC"
        `shouldBe` Just (URN "foo" "AbC")
      parseURN "urn:FOO:aBc"
        `shouldBe` Just (URN "foo" "aBc")

    it "should unescape NSS" $ do
      parseURN "urn:foo:%25" `shouldBe` Just (URN "foo" "%")

  describe "rendering" $ do
    it "should render with renderURN" $ do
      renderURN (URN "uuid" "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
        `shouldBe` "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6"

    it "should render with show" $ do
      show (URN "uuid" "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
        `shouldBe` "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6"

    it "should escape NSS part" $ do
      show (URN "foo" "%") `shouldBe` "urn:foo:%25"

  describe "parsing+rendering" $ do
    it "iso" $ do
      let urn = URN "foo" ":_: :_:"
      read (show urn) `shouldBe` urn

  describe "lexical equivalence" $ do
    let (===) = shouldBe :: URN -> URN -> IO ()
    let (/==) = (\ a b -> shouldSatisfy a (/= b)) :: URN -> URN -> IO ()
{-
           1- URN:foo:a123,456
           2- urn:foo:a123,456
           3- urn:FOO:a123,456
           4- urn:foo:A123,456
           5- urn:foo:a123%2C456
           6- URN:FOO:a123%2c456
-}
    it "URNs 1, 2, and 3 are all lexically equivalent" $ do
      "URN:foo:a123,456" === "urn:foo:a123,456"
      "URN:foo:a123,456" === "urn:FOO:a123,456"
      "urn:foo:a123,456" === "urn:FOO:a123,456"

    it "URN 4 is not lexically equivalent any of the other URNs\
       \of the above set." $ do
      "urn:foo:A123,456" /== "URN:foo:a123,456"
      "urn:foo:A123,456" /== "urn:foo:a123,456"
      "urn:foo:A123,456" /== "urn:FOO:a123,456"
      "urn:foo:A123,456" /== "urn:foo:a123%2C456"
      "urn:foo:A123,456" /== "URN:FOO:a123%2c456"

    it "URNs 5 and 6 are only lexically equivalent to each other." $ do
      "urn:foo:a123%2C456" === "URN:FOO:a123%2c456"

      "URN:FOO:a123%2c456" /== "urn:foo:A123,456"

-- in contrast to the RFC, some the following URNs are equal. This
-- happen because we unescape NSS while parsing but RFC2141 says about
-- "normalizing the case of any %-escaping" only (with no escaping)
      "urn:foo:a123%2C456" === "URN:foo:a123,456"
      "urn:foo:a123%2C456" === "urn:foo:a123,456"
      "urn:foo:a123%2C456" === "urn:FOO:a123,456"
      "urn:foo:a123%2C456" /== "urn:foo:A123,456"

      "URN:FOO:a123%2c456" === "URN:foo:a123,456"
      "URN:FOO:a123%2c456" === "urn:foo:a123,456"
      "URN:FOO:a123%2c456" === "urn:FOO:a123,456"
