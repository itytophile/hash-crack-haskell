{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Alphabet
import           Control.Concurrent
import qualified Control.Logging               as L
import           Control.Monad
import qualified Crypto.Hash.MD5               as MD5
import qualified Data.ByteString               as B
import           Data.Char                     (ord)
import           Data.Text                     as T (Text, pack, unpack, words)
import           Debug.Trace                   (trace)
import           Network.Socket                (withSocketsDo)
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection (Connection)
import           Text.Hex                      (decodeHex, encodeHex)

handleMessage :: Connection -> Text -> IO ()
handleMessage conn msg = L.log ( "Request received: " <> msg) >> execute conn (T.words msg)

execute :: Connection -> [Text] -> IO ()
execute conn ["search", hash, start, end]
  = maybe (L.log "Non valid hash received") (crack conn start end) (decodeHex hash)
  >> loop conn

execute conn ["stop"]
  = L.log "wants to stop" >> loop conn

execute conn ["exit"]
  = L.log "wants to exit"

execute conn _
  = L.log "error" >> loop conn

loop :: Connection -> IO ()
loop conn = WS.receiveData conn >>= handleMessage conn

crack :: Connection -> Text -> Text -> B.ByteString -> IO ()
crack conn start end hash =
  L.log ( "cracking " <> encodeHex hash <>  " on [" <>  start <> ";" <> end <> ")...")
  >>  maybe (L.log "No solution found!") (found conn hash) (crackLoop hash (T.unpack start) (T.unpack end))

found :: Connection -> B.ByteString -> Text -> IO ()
found conn hash solution
  = L.log ("Solution found: " <> solution)
  >> WS.sendTextData conn ("found " <> encodeHex hash <> " " <> solution)

crackLoop :: B.ByteString -> String -> String -> Maybe Text
crackLoop hash start end
  | start == end = Nothing
  | MD5.hash (toByteString start) == hash = Just (T.pack start)
  | otherwise = crackLoop hash (incrementWord start) end

app :: WS.ClientApp ()
app conn = putStrLn "Connected!"
  >> WS.sendTextData conn ("slave" :: Text)
  >> loop conn
  >> WS.sendClose conn ("Bye!" :: Text)

lol :: Text
lol = "lol" <> "lil"

main :: IO ()
main = L.withStdoutLogging $ withSocketsDo $ WS.runClient "127.0.0.1" 3000 "/ws" app

alphabet = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

toByteString :: String -> B.ByteString
toByteString text = B.pack $ fmap (toEnum . ord) text
