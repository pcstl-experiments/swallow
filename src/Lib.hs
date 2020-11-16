{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc,
      removeWhiteSpace,
      eatSingleToken,
      PositionInfo(..)
    ) where

import Data.Text (Text)
import Data.Either (isLeft)
import Data.List (partition)
import System.IO (withFile, hSetEncoding, utf8, IOMode(ReadMode))
import qualified Data.Char as Char
import System.Environment (getArgs)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Control.Monad.Trans (liftIO, MonadIO)

readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 fp = liftIO $ withFile fp ReadMode $ \h -> do
  hSetEncoding h utf8
  TextIO.hGetContents h

someFunc :: IO ()
someFunc = do
  putStrLn "hello, world!"
  -- input <- getArgs
  -- if length input /= 1
  --   then putStrLn "Invalid args."
  --   else do
  --     let file = head input
      

data PositionInfo = Position
  { line   :: Int
  , column :: Int
  } deriving Show

data Token
  = Name Text PositionInfo
  | Arrow PositionInfo
  | LeftParen PositionInfo
  | RightParen PositionInfo
  deriving Show

isIdentifierChar :: Char -> Bool
isIdentifierChar = Char.isAlphaNum

-- tryTokens :: [Text] -> (Text, PositionInfo) -> (Maybe Token, (Text, PositionInfo))
-- tryTokens ["(", ")", "->"]

eatSingleToken :: (Text, PositionInfo) -> (Maybe Token, (Text, PositionInfo))
eatSingleToken (t, pos)
  | Text.isPrefixOf "->" t =
    (Just $ Arrow pos, (Text.drop 2 t, pos { column = 2 + (column pos) }))
  | Text.isPrefixOf "(" t =
    (Just $ LeftParen pos, (Text.drop 1 t, pos { column = 1 + (column pos) }))   
  | Text.isPrefixOf ")" t =
    (Just $ RightParen pos, (Text.drop 1 t, pos { column = 1 + (column pos) }))
  | otherwise =
    let text        = Text.takeWhile isIdentifierChar t
        tokenLength = Text.length text
    in if not . Text.null $ text
       then (Just $ Name text pos, (Text.drop tokenLength t, pos { column = tokenLength + (column pos) }))
       else (Nothing, (t, pos))

-- tokenize :: Text -> Either [Text] [Token]
-- tokenize text =
--   if null errors
--   then Right tokens
--   else Left errors
--   where
--     (errors, tokens) = partition isLeft tokensAndErrors
--     tokensAndErrors  = makeSingleToken <$> nonWhiteSpace
--     nonWhiteSpace    = Text.split Char.isSpace text

removeWhiteSpace :: Text -> [(Text, Int)]
removeWhiteSpace t = reverse $! eatWhiteSpace 0 [] t 
  where eatWhiteSpace n acc t =
          let (spaces, rest) = Text.span Char.isSpace t
              increment      = Text.length spaces
          in if Text.null rest
             then acc
             else eatText (n+increment) acc rest

        eatText n acc t =
          let (text, rest) = Text.span (not . Char.isSpace) t
              increment    = Text.length text
              newAcc       = ((text,n):acc)
          in if Text.null rest
             then newAcc
             else eatWhiteSpace (n+increment) newAcc rest

-- tokenizeSingleLine :: [Text] -> (Either [Text] [Token], [Text])
-- tokenizeSingleLine []           = (Left [], [])
-- tokenizeSingleLine (line:lines) = (tokenize line, lines)

-- tokenizeUntilError :: Text -> 

data Term
  = Var Text
  | Arr Text Term
