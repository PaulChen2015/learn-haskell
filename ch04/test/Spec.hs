{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)


main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "halve" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = halve (fromIntegral input) `shouldBe` expected

data Case = Case { description :: String
                 , input       :: [a]
                 , expected    :: ([a], [a])
                 }

cases :: [Case]
cases = [ Case { description = "even list"
               , input       = [1,2,3,4]
               , expected    = ([1,2],[3,4])
               }
        , Case { description = "empty list"
               , input       = []
               , expected    = ([],[])
               }
        ]
