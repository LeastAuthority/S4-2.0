module Main where

import S4.Server
  ( startServer
  )

-- Run the S4 server.
main :: IO ()
main = startServer 8080
