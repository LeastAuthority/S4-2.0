{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding
  ( putStrLn
  )

import Control.Monad
  ( forever
  )
import Control.Monad.STM
  ( atomically
  )
import Control.Concurrent.Async
  ( async
  , wait
  )

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Encoding
  ( encodeUtf8
  , decodeUtf8
  )

import qualified MagicWormhole
import qualified Crypto.Spake2 as Spake2

main :: IO ()
main =
  let
    appID = MagicWormhole.AppID "lothar.com/wormhole/text-or-file-xfer"
    -- appID = MagicWormhole.AppID "tahoe-lafs.org/invite"
    endpoint = "ws://localhost:4000/v1"
  in
    case MagicWormhole.parseWebSocketEndpoint endpoint of
      Nothing ->
        TextIO.putStrLn "Failed"
      Just wsEndpoint -> do
        side <- MagicWormhole.generateSide
        MagicWormhole.runClient wsEndpoint appID side $ \session -> do
          TextIO.putStrLn "Nameplate: "
          nameplate <- getLine >>= return . Text.pack
          mailbox <- MagicWormhole.claim session (MagicWormhole.Nameplate nameplate)
          peer <- MagicWormhole.open session mailbox
          TextIO.putStrLn "Password: "
          password <- getLine >>= return . Text.pack
          let code = nameplate <> "-" <> password
          let spake2Password = Spake2.makePassword $ encodeUtf8 code
          TextIO.putStrLn $ "Code is " <> code
          TextIO.putStrLn "Connecting ..."
          MagicWormhole.withEncryptedConnection peer spake2Password $ \conn -> do
            TextIO.putStrLn "Connection established"
            reader <- async $ forever $ do
                  (MagicWormhole.PlainText bytes) <- atomically $ MagicWormhole.receiveMessage conn
                  let text = decodeUtf8 bytes
                  TextIO.putStrLn $ "Received: " <> text

            writer <- async $ forever $ do
                  line <- getLine >>= return . encodeUtf8 . Text.pack
                  MagicWormhole.sendMessage conn (MagicWormhole.PlainText line)

            wait writer
