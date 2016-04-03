{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char (isDigit)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage = fst . head . readP_to_S parseLogMessage

parseLogMessage :: ReadP LogMessage
parseLogMessage = parseLogMessage' <|> parseUnknown

parseLogMessage' :: ReadP LogMessage
parseLogMessage' = do
  messageType <- parseMessageType
  skipSpaces
  timestamp <- parseTimeStamp
  skipSpaces
  message <- parseAnyString
  return $ LogMessage messageType timestamp message

parseMessageType :: ReadP MessageType
parseMessageType = parseInfo <|> parseWarning <|> parseError
  where
    parseInfo    = string "I " >> return Info
    parseWarning = string "W " >> return Warning
    parseError   = string "E " >> parseInt >>= \level -> return $ Error level
    
parseTimeStamp :: ReadP TimeStamp
parseTimeStamp = parseInt

parseUnknown :: ReadP LogMessage
parseUnknown = parseAnyString >>= \message -> return $ Unknown message

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseAnyString :: ReadP String
parseAnyString = munch1 (const True)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf        = Node Leaf lm Leaf
insert lm (Node left lm' right)
  | getTimeStamp lm < getTimeStamp lm' = Node (insert lm left) lm' right
  | otherwise                          = Node left lm' (insert lm right)

getTimeStamp :: LogMessage -> Maybe TimeStamp
getTimeStamp (Unknown _)                = Nothing
getTimeStamp (LogMessage _ timestamp _) = Just timestamp

getMessage :: LogMessage -> String
getMessage (Unknown msg)        = msg
getMessage (LogMessage _ _ msg) = msg

build :: [LogMessage] -> MessageTree
build = foldl (\acc x -> insert x acc) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                 = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter isRelevant . inOrder . build 
  where 
    isRelevant (LogMessage (Error level) _ _) = level >= 50
    isRelevant _                              = False
