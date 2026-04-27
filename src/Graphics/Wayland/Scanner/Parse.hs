{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.Scanner.Parse
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports some functions for parsing the xml into the
-- AST.
------------------------------------------------------------------------
module Graphics.Wayland.Scanner.Parse where

import qualified Data.Text as T
import Data.Text (Text)
import Text.XML.Light

import Graphics.Wayland.Scanner.Types

-- | Look up an attribute by name, returning empty Text if absent.
attr :: String -> Element -> Text
attr name = maybe T.empty T.pack . findAttr (unqual name)

-- | Find direct children with the given tag name.
children :: String -> Element -> [Element]
children name = findChildren (unqual name)

parseWlTypeArg :: Element -> Arg
parseWlTypeArg el =
  let t   = attr "type"       el
      n   = attr "name"       el
      d   = parseDesc         el
      en  = attr "enum"       el
      an  = attr "allow-null" el
      obj = case findAttr (unqual "interface") el of
              Nothing -> Untyped d
              Just x  -> Typed   d (T.pack x)
  in case t of
       "object" -> ArgObject n obj $ an /= "false"
       "new_id" -> ArgNewId  n obj
       "array"  -> ArgArray  n d
       "uint"   -> ArgValue  n d $ TUint   $ if T.null en then Nothing else Just en
       "int"    -> ArgValue  n d $ TInt    $ if T.null en then Nothing else Just en
       "string" -> ArgValue  n d $ TString $ an == "true"
       _        -> ArgValue  n d $ parseWlTypeValue t

parseWlTypeValue :: Text -> WlType
parseWlTypeValue "fixed" = TFixed
parseWlTypeValue "fd"    = TFd
parseWlTypeValue t       = error $ "wayland type not supported: " ++ show t

parseEnum :: Element -> EnumDecl
parseEnum el =
  let name     = attr "name"     el
      bitfield = attr "bitfield" el
      entry e  = EnumEntry (attr "name" e) (parseDesc e)
  in EnumDecl name (parseDesc el) (bitfield == "true") $ map entry (children "entry" el)

parseMes :: Element -> Message
parseMes el =
  let name     = attr "name" el
      since    = if T.null (attr "since" el) then "1" else attr "since" el
      argTypes = map parseWlTypeArg $ children "arg" el
  in Message name (parseDesc el) argTypes (read $ T.unpack since)

parseIface :: Name -> Element -> Interface
parseIface n el =
  let version = attr "version" el
      iface   = attr "name" el
      events  = map parseMes  $ children "event"   el
      reqs    = map parseMes  $ children "request" el
      enums   = map parseEnum $ children "enum"    el
  in Interface iface n (parseDesc el) events reqs enums (read $ T.unpack version)

parseDesc :: Element -> Text
parseDesc el =
  let summary   = attr "summary" el
      descText  = T.pack . concatMap strContent $ children "description" el
      normalize = T.unlines . map (T.unwords . T.words) . T.splitOn "\n\n"
  in if T.null descText then summary else normalize descText

parseProtocol :: Element -> Protocol
parseProtocol el =
  let name   = attr "name" el
      ifaces = map (parseIface name) $ children "interface" el
  in Protocol name (parseDesc el) ifaces
