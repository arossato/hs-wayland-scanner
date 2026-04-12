{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.Scanner.Main
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- An implementation of the Wayland Message Definition Language
------------------------------------------------------------------------
module Graphics.Wayland.Scanner.Main where

import Control.Monad
import Data.List
import qualified Data.Text.IO as T
import System.Environment
import System.Exit

import Graphics.Wayland.Scanner

main :: IO ()
main = do
  as <- getArgs
  when (null as) $ putStrLn usage >> exitFailure
  cfg <- decodeArgs defaultConfig as
  res <- mapM T.readFile (protocols cfg) >>= generate cfg
  putStrLn $ "Protocol Haskell bindings written in " ++ genPrefix cfg
  putStrLn   "Generated modules:"
  mapM_ T.putStrLn res
  exitSuccess

defaultConfig :: HwsConfig
defaultConfig = HwsConfig
  { genPrefix   = "generated"
  , hsNameSpace = "Graphics"
  , protoRole   = Client
  , protocols   = []
  , cbitsPrefix = "cbits"
  , srcPrefix   = "src"
  }

decodeArgs :: HwsConfig -> [String] -> IO HwsConfig
decodeArgs c [] = return c
decodeArgs c (arg:args) =
  case arg of
    "-h"   -> putStrLn longUsage >> exitSuccess
    "-p"      | s : args' <- args
           -> decodeArgs c {genPrefix = s} args'
    "-n"      | s : args' <- args
           -> decodeArgs c {hsNameSpace = s} args'
    "-r"      | "Client" : args' <- args
           -> decodeArgs c {protoRole = Client} args'
    "-r"      | "client" : args' <- args
           -> decodeArgs c {protoRole = Client} args'
    "-r"      | "Server" : args' <- args
           -> decodeArgs c {protoRole = Server} args'
    "-r"      | "server" : args' <- args
           -> decodeArgs c {protoRole = Server} args'
    "-c"      | s : _ <- args
           -> read <$> readFile s
    "--cbits" | s : args' <- args
           -> decodeArgs c {cbitsPrefix = s} args'
    "--src"   | s : args' <- args
           -> decodeArgs c {srcPrefix = s} args'
    _ | arg `hasExt` ".xml"
        -> decodeArgs c {protocols = arg : protocols c} args
      | otherwise -> putStrLn ("Unknow arg: " ++ arg ++ "\n" ++ usage) >> exitFailure

hasExt :: FilePath -> String -> Bool
hasExt f e = e `isSuffixOf` f

usage :: String
usage = "Usage: hws [-h] [-p PATH] [-n STRING] [-r [Client|Server]] [-c PATH] [--cbits PATH] [--src PATH] [PROTOCOLS]"

longUsage :: String
longUsage = "Usage: hws [OPTIONS] [PROTOCOLS]\n\nOptions:\n" ++ details
  where
    details = "\
      \-h                 Print help\n\
      \-p PATH            Root directory for generated files (Default: \"./generated\")\n\
      \-n STRING          Namespace for generated modules (Default: \"Graphics\")\n\
      \-r ROLE            Generate Client or Server protocols (Default: \"Client\")\n\
      \-c PATH            Path to a configuration file\n\
      \--cbits PATH       Sub-directory for generated C files (Default: \"cbits\")\n\
      \--src PATH         Sub-directory for generated Haskell files (Default: \"src\")\n\
      \[PROTOCOLS]        The Wayland XML files to be processed\n\
      \"
