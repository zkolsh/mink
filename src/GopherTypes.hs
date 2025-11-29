{-# LANGUAGE OverloadedStrings #-}
module GopherTypes(ElementType(..), Element(..), parseLine, recvGopher) where

import qualified Data.ByteString as BS
import Data.Char
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString
import Text.Read (readMaybe)

data ElementType = PlainText
                 | Directory
                 | PhoneBook
                 | Error
                 | Macintosh
                 | DOS
                 | Unix
                 | IndexSearch
                 | Telnet
                 | Binary
                 | Redundant
                 | Tn3270
                 | Gif
                 | Image
                 | Information
                deriving (Eq, Show)

instance Enum ElementType where
        fromEnum PlainText = ord '0'
        fromEnum Directory = ord '1'
        fromEnum PhoneBook = ord '2'
        fromEnum Error = ord '3'
        fromEnum Macintosh = ord '4'
        fromEnum DOS = ord '5'
        fromEnum Unix = ord '6'
        fromEnum IndexSearch = ord '7'
        fromEnum Telnet = ord '8'
        fromEnum Binary = ord '9'
        fromEnum Redundant = ord '+'
        fromEnum Tn3270 = ord 'T'
        fromEnum Gif = ord 'g'
        fromEnum Image = ord 'I'
        fromEnum Information = ord 'i'
        toEnum 48 = PlainText
        toEnum 49 = Directory
        toEnum 50 = PhoneBook
        toEnum 51 = Error
        toEnum 52 = Macintosh
        toEnum 53 = DOS
        toEnum 54 = Unix
        toEnum 55 = IndexSearch
        toEnum 56 = Telnet
        toEnum 57 = Binary
        toEnum 43 = Redundant
        toEnum 84 = Tn3270
        toEnum 103 = Gif
        toEnum 73 = Image
        toEnum 105 = Information
        toEnum _ = undefined

elementType :: Char -> Maybe ElementType
elementType x = case x of
        '0' -> Just PlainText
        '1' -> Just Directory
        '2' -> Just PhoneBook
        '3' -> Just Error
        '4' -> Just Macintosh
        '5' -> Just DOS
        '6' -> Just Unix
        '7' -> Just IndexSearch
        '8' -> Just Telnet
        '9' -> Just Binary
        '+' -> Just Redundant
        'T' -> Just Tn3270
        'g' -> Just Gif
        'I' -> Just Image
        'i' -> Just Information
        _   -> Nothing

data Element = Element {
        elType :: ElementType,
        elName :: String,
        elRsrc :: String,
        elHost :: (HostName, PortNumber)
} deriving (Eq, Show)

parseLine :: String -> Maybe Element
parseLine [] = Nothing
parseLine (t' : xs) = do
        t <- elementType t'
        [name, selector, domain, port'] <- pure . take 4 $ splitOn "\t" xs
        port <- readMaybe port'
        pure Element {
                elType = t,
                elName = name,
                elRsrc = selector,
                elHost = (domain, port)
        }

recvGopher :: Socket -> IO BS.ByteString
recvGopher sock = go BS.empty
        where eot = "\r\n.\r\n"
              strip bytes = fromMaybe bytes (BS.stripSuffix eot bytes)
              go acc = do
                chunk <- recv sock 4096
                let acc' = BS.append acc chunk
                if BS.null chunk
                        then pure acc
                        else if BS.isInfixOf eot acc'
                                then pure $! strip acc'
                                else go acc'
