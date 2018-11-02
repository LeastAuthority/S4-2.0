module Main where

-- Provide a test entrypoint that can easily generate results in JUnit XML for
-- the easy consumption of a CI system like CircleCI.

import System.IO
  ( FilePath
  )

import System.IO.Error
  ( isDoesNotExistError
  )

import System.Environment
  ( getEnv
  )

import Control.Exception
  ( tryJust
  )

import Test.Hspec.Formatters.Jenkins
  ( xmlFormatter
  )

import Test.Hspec.Runner
  ( hspec
  , hspecWith
  , defaultConfig
  , configFormatter
  , configOutputFile
  )

import Spec
  ( spec
  )

main :: IO ()
main = do
  -- If we're doing CI, just do what CI wants.
  ci <- tryJust (Just . isDoesNotExistError) $ getEnv "CI"
  case ci of
    Left _ -> hspec spec
    Right _ ->
      hspecWith
      defaultConfig
      { configFormatter = Just xmlFormatter
      , configOutputFile = Right ("test-results/http-tests/results.xml" :: FilePath)
      } spec
