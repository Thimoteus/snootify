{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.Aeson.Types
import System.Process (runCommand, ProcessHandle)
import Control.Concurrent (threadDelay)
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Network.HTTP.Conduit (simpleHttp)
import Control.Applicative
import Control.Monad
import System.IO (openTempFile, hPutStr, hGetContents)

notify :: String -> String -> IO ProcessHandle
notify title msg = runCommand $ "notify-send " ++ readify title ++ ' ':(readify msg)
  where
  readify :: String -> String
  readify = quotify . escapify
  escapify :: String -> String
  escapify = foldr acc ""
    where
    acc :: Char -> String -> String
    acc c str
      | c == '"' = '\\':'"':str
      | otherwise = c:str
  quotify :: String -> String
  quotify str = '"' : str ++ "\""

findLineInCfg :: String -> String -> String
findLineInCfg key = value . head . filter findLine . lines
  where
  findLine :: String -> Bool
  findLine confLine = take (length key) confLine == key
  value :: String -> String
  value = dropWhile (== ' ') . dropWhile (== '=') . dropWhile (== ' ') . drop (length key)

data Messages = Messages {
  children :: [Message]
  } deriving (Show)

data Message = Message {
  author :: T.Text,
  dest :: T.Text,
  body :: T.Text,
  theId :: T.Text
} deriving (Show)

instance FromJSON Messages where
  parseJSON (Object o) = Messages <$>
    ((o .: "data") >>= (.: "children"))
  parseJSON _ = mzero

instance FromJSON Message where
  parseJSON (Object o) =
    Message <$>
      ((o .: "data") >>= (.: "author")) <*>
      ((o .: "data") >>= (.: "dest")) <*>
      ((o .: "data") >>= (.: "body")) <*>
      ((o .: "data") >>= (.: "id"))
  parseJSON _ = mzero

getMessages :: B.ByteString -> [Message]
getMessages = getMessages' . decode
  where
  getMessages' :: Maybe Messages -> [Message]
  getMessages' Nothing = []
  getMessages' (Just msgs) = children msgs

notificationFromMessage :: Message -> (String, String)
notificationFromMessage msg =
  ( "New message: " ++ T.unpack (author msg) ++ " → " ++ T.unpack (dest msg)
  , trunc8 . T.unpack $ body msg )
    where
    trunc8 :: String -> String
    trunc8 str
      | length str > 100 = take 97 str ++ "..."
      | otherwise = str

filterMessages :: [Message] -> [String] -> [Message]
filterMessages msgs oldIds = filter (\m -> (T.unpack $ theId m) `notElem` oldIds) msgs

main = do
  home <- getHomeDirectory
  config <- readFile (home ++ "/.config/snootify.conf")
  let rssAddr = findLineInCfg "rss-address" config
      delayInSeconds = 1000000 * (read $ findLineInCfg "repeat-interval" config)
  if delayInSeconds < 1
  then oneTimeLookup rssAddr
  else repeatLookup rssAddr delayInSeconds []

-- delay is improper, i.e. < 1 second
oneTimeLookup :: String -> IO ()
oneTimeLookup rss = do
  let tmpFile = "/tmp/snootifytmp"
  msgs <- simpleHttp rss
  oldIdsExist <- doesFileExist tmpFile
  oldIds <- if oldIdsExist
            then readFile tmpFile
            else return []
  notifyNewMessages msgs (lines oldIds)
  writeFile tmpFile (unlines . map (T.unpack . theId) $ getMessages msgs)
-- delay is proper, i.e. >= 1 second
repeatLookup :: String -> Int -> [String] -> IO ()
repeatLookup rss delay oldIds = do
  msgs <- simpleHttp rss
  notifyNewMessages msgs oldIds
  threadDelay delay
  repeatLookup rss delay (map (T.unpack . theId) $ getMessages msgs)
notifyNewMessages :: B.ByteString -> [String] -> IO [ProcessHandle]
notifyNewMessages new old = mapM (\(title,body) -> notify title body) . map notificationFromMessage $ filterMessages (getMessages new) old
