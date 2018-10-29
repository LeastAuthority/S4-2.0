module Main where

import S4.Server
  ( startServer
  )

main :: IO ()
main = startServer 8080
