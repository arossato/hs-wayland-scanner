{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.Scanner.Text
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports some functions for 'Text' manipulation.
------------------------------------------------------------------------
module Graphics.Wayland.Scanner.Text where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

capitalize :: Text -> Text
capitalize t =
  case T.uncons t of
    Nothing -> t
    Just (c, rest) -> T.cons (toUpper c) rest

lowerFirst :: Text -> Text
lowerFirst t =
  case T.uncons t of
    Nothing -> t
    Just (c, rest) -> T.cons (toLower c) rest

toCamel :: Text -> Text
toCamel t =
  let parts = T.splitOn "_" t
  in case parts of
       []     -> t
       (x:xs) -> x <> T.concat (map capitalize xs)

toHsType :: Text -> Text
toHsType = capitalize . toCamel

toHsFcn :: Text -> Text
toHsFcn = lowerFirst . toCamel

wordWrap :: Int -> Text -> Text
wordWrap maxLen = unlines' . concatMap (wrapLine maxLen) . T.lines

wrapLine :: Int -> Text -> [Text]
wrapLine maxLen line = go (T.words line) T.empty []
  where
    go [] cur acc = map haddockIdent $ reverse $ flush cur acc
    go (w:ws) cur acc
      | T.null cur          = go ws w acc
      | T.length cur + 1 + T.length w <= maxLen
                            = go ws (cur <> " " <> w) acc
      | otherwise           = go ws w (cur : acc)
    flush cur acc = if T.null cur then acc else cur : acc

haddockIdent :: Text -> Text
haddockIdent = T.unwords . map go . T.words
  where
    go w
      | "__" `T.isInfixOf` w = w
      | "_"  `T.isInfixOf` w = "@" <> w <> "@"
      | otherwise            = w

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
