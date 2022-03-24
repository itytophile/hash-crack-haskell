{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Alphabet                      (incrementWord)
import           Control.Concurrent            (ThreadId, forkIO, killThread)
import qualified Control.Logging               as L
import           Control.Monad                 (forM_)
import qualified Crypto.Hash.MD5               as MD5
import qualified Data.ByteString               as B
import           Data.Char                     (ord)
import           Data.Text                     as T (Text, pack, unpack, words)
import           Debug.Trace                   (trace)
import           Network.Socket                (withSocketsDo)
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection (Connection)
import           Text.Hex                      (decodeHex, encodeHex)

main :: IO ()
main = L.withStdoutLogging $ withSocketsDo $ WS.runClient "127.0.0.1" 3000 "/ws" app

app :: WS.ClientApp ()
app conn
  = putStrLn "Connected!"
  >> WS.sendTextData conn ("slave" :: Text)
  >> listeningLoop conn Nothing
  >> WS.sendClose conn ("Bye!" :: Text)

listeningLoop :: Connection -> Maybe ThreadId -> IO ()
listeningLoop conn threadId
  = WS.receiveData conn
  >>= handleMessage conn threadId

handleMessage :: Connection -> Maybe ThreadId  -> Text -> IO ()
handleMessage conn threadId msg
  = L.log ( "Request received: " <> msg)
  >> execute conn threadId (T.words msg)

execute :: Connection -> Maybe ThreadId -> [Text] -> IO ()
execute conn threadId ["search", hash, start, end]
  = forM_ threadId killThread
  >> maybe (Nothing <$ L.log "Non valid hash received") (fmap Just . forkIO . crack conn start end) (decodeHex hash)
  >>= listeningLoop conn

execute conn Nothing ["stop"]
  = L.log "Nothing to stop"
  >> listeningLoop conn Nothing

execute conn (Just threadId) ["stop"]
  = L.log "Wants to stop"
  >> killThread threadId
  >> listeningLoop conn Nothing

execute conn threadId ["exit"] = L.log "wants to exit"

execute conn threadId _
  = L.log "error"
  >> listeningLoop conn threadId

crack :: Connection -> Text -> Text -> B.ByteString -> IO ()
crack conn start end hash
  = L.log ( "cracking " <> encodeHex hash <>  " on [" <>  start <> ";" <> end <> ")...")
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

toByteString :: String -> B.ByteString
toByteString text = B.pack $ fmap (toEnum . ord) text
