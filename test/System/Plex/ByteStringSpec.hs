
{-# LANGUAGE OverloadedStrings #-}

module System.Plex.ByteStringSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import System.Plex.ByteString
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy ( ByteString )

--import qualified Data.List as L
import Data.Maybe

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

shouldSatisfyM :: IO a -> (a -> Bool) -> Expectation
shouldSatisfyM f pred = (pred <$> f) `shouldReturn` True

isJustAnd :: Maybe a -> (a -> Bool) -> Bool
m `isJustAnd` f =
  isJust m && fmap f m == Just True

spec :: Spec
spec = do
  describe "cmd" $ do
    it "returns safely with no command" $ 
      cmd "" [] `shouldSatisfyM` ("error executing" `L.isPrefixOf`)
    it "returns safely with unexecutable command" $ 
      cmd "/" [] `shouldSatisfyM` ("error executing" `L.isPrefixOf`)
    it "returns safely with nonexistent command" $ 
      cmd "/zzz" [] `shouldSatisfyM` ("error executing" `L.isPrefixOf`)
    it "returns safely with bad args" $ 
      cmd "ls" ["/zzz"] `shouldSatisfyM` ("ls: cannot access" `L.isPrefixOf`)
    it "gives a result for an okay command" $ 
      cmd "ls" ["-d", "/"] `shouldReturn` "/\n"

  describe "cmdTimeout" $ do
    it "returns safely with negative timeout" $ 
      cmdTimeout "ls" [] (-1) `shouldReturn` Nothing
    it "returns safely with 0 timeout" $ 
      cmdTimeout "ls" [] 0 `shouldReturn` Nothing
    it "returns safely with positive timeout ~ >= 10^5 usecs" $ 
      cmdTimeout "ls" [] ((10^5) :: Int) `shouldSatisfyM` isJust
    it "returns safely with no command" $ 
      cmdTimeout "" [] ((10^6) :: Int) `shouldSatisfyM` (`isJustAnd` ("error executing" `L.isPrefixOf`))
    it "returns safely with unexecutable command" $ 
       cmdTimeout "/" [] ((10^6) :: Int) `shouldSatisfyM` (`isJustAnd` ("error executing" `L.isPrefixOf`))
    it "returns safely with nonexistent command" $ 
       cmdTimeout "/zzz" [] ((10^6) :: Int) `shouldSatisfyM` (`isJustAnd` ("error executing" `L.isPrefixOf`))
    it "returns safely with bad args" $ 
       cmdTimeout "ls" ["/zzz"] ((10^6) :: Int) `shouldSatisfyM` (`isJustAnd` ("ls: cannot access" `L.isPrefixOf`))
    it "gives a result for an okay command" $ 
       cmdTimeout "ls" ["-d", "/"] ((10^6) :: Int) `shouldReturn` Just "/\n"





