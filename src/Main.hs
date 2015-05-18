{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.Aeson.Types
import System.Process (runCommand, ProcessHandle)
import Control.Concurrent (threadDelay)
import System.Environment (getEnv)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Network.HTTP.Conduit (simpleHttp)
import Control.Applicative
import Control.Monad
import System.IO (openTempFile, hPutStr, hGetContents)

notify :: String -> String -> IO ProcessHandle
notify title msg = runCommand $ "notify-send " ++ quotify title ++ ' ':(quotify msg)
  where
  quotify :: String -> String
  quotify str = '\'' : str ++ "'"

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
  home <- getEnv "HOME"
  config <- readFile (home ++ "/.config/snootify.conf")
  let rssAddr = findLineInCfg "rss-address" config
      delayInSeconds = 1000000 * (read $ findLineInCfg "repeat-interval" config)
  messages <- simpleHttp rssAddr
  theLoop rssAddr delayInSeconds []
    where
    theLoop rss delay oldIds = do
      msgs <- simpleHttp rss
      mapM (\(title,body) -> notify title body) . map notificationFromMessage $ filterMessages (getMessages msgs) oldIds
      if delay > 0
        then do
          threadDelay delay
          theLoop rss delay (map (T.unpack . theId) $ getMessages msgs)
        else return ()
