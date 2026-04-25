{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.Scanner.Generate
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports some functions for generating Haskell and C
-- code.
------------------------------------------------------------------------
module Graphics.Wayland.Scanner.Generate where

import Control.Monad
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Text.XML.Light
import System.Directory
import System.FilePath
import System.Process

import Graphics.Wayland.Scanner.Parse
import Graphics.Wayland.Scanner.Render
import Graphics.Wayland.Scanner.RenderC
import Graphics.Wayland.Scanner.Solve
import Graphics.Wayland.Scanner.Text
import Graphics.Wayland.Scanner.Types

-- | File generation
generate' :: HwsConfig -> [Text] -> IO ()
generate' cfg = void . generate cfg

-- | Returns the list of generated Haskell modules
generate :: HwsConfig -> [Text] -> IO [Text]
generate cfg@(HwsConfig prefix nameSpace role _ cbits _) xmlSources = do
  let parseDoc i =
        case parseXMLDoc i of
          Nothing  -> error ("invalid XML: " ++ show i)
          Just res -> res
      roleName     = if role == Server then "server" else "client"
      docs         = map parseDoc      xmlSources
      parsedProtos = map parseProtocol docs
      ifaceMap     = buildIfaceMap     parsedProtos
      solvedProtos = map (solveProtocol ifaceMap) parsedProtos
      include p    = T.pack $
                     "#include \"" ++ T.unpack (solvedProtoName p) ++ "-" ++ roleName ++ "-protocol.c\"\n" ++
                     "#include \"" ++ T.unpack (solvedProtoName p) ++ "-" ++ roleName ++ ".c\""
  generateDirs       cfg
  generateModuleCore cfg
  mapM_     (generateModule     cfg) solvedProtos
  zipWithM_ (generateCbits      cfg) (protocols cfg) solvedProtos
  mapM_     (generateFFIWrapper cfg) solvedProtos
  T.writeFile (prefix </> cbits </> "wayland-" ++ roleName ++ "-protocols.c") $
    T.unlines $ autogenWrapperComment : map include solvedProtos
  let generated  p = T.intercalate "." $ map toHsType [T.pack nameSpace, "Wayland", T.pack roleName, "Protocol", solvedProtoName p]
      defModules p = T.intercalate "." $ map toHsType [T.pack nameSpace, "Wayland.Protocol", solvedProtoName p]
      coreModule   = T.intercalate "." $ map toHsType [T.pack nameSpace, "Wayland", T.pack roleName, "Core"]
      modules    p = [generated p, defModules p]
  return $ coreModule : concatMap modules solvedProtos

generateDirs :: HwsConfig -> IO ()
generateDirs (HwsConfig prefix nameSpace role _ cbits src) = do
  let dir  = T.unpack $ T.replace "." "/"  $ T.pack nameSpace
  createDirectoryIfMissing True $ prefix </> src </> dir </> "Wayland" </> show role </> "Protocol"
  createDirectoryIfMissing True $ prefix </> src </> dir </> "Wayland/Protocol"
  createDirectoryIfMissing True $ prefix </> cbits

generateModule :: HwsConfig -> SolvedProtocol -> IO ()
generateModule cfg@(HwsConfig prefix nameSpace role _ _ src) proto = do
  let dir       = T.unpack $ T.replace "." "/"  $ T.pack nameSpace
      hsCode    = renderProtocol role $ addImplicitRequestsToProtocol role proto
      roleName  = show role
      protoFile = src </> dir </> "Wayland" </> roleName </> "Protocol" </> T.unpack (toHsType $ solvedProtoName proto)
      enumsFile = src </> dir </> "Wayland" </> "Protocol" </> T.unpack (toHsType $ solvedProtoName proto)
  T.writeFile (prefix </> protoFile ++ ".hsc") $ T.unlines $ moduleHeader     cfg proto ++ map snd hsCode
  T.writeFile (prefix </> enumsFile ++ ".hsc") $ T.unlines $ moduleEnumHeader cfg proto ++ map fst hsCode

generateModuleCore :: HwsConfig -> IO ()
generateModuleCore cfg@(HwsConfig prefix nameSpace role _ _ src) = do
  let dir  = T.unpack $ T.replace "." "/"  $ T.pack nameSpace
  T.writeFile (prefix </> src </> dir </> "Wayland" </> show role </> "Core.hs") $ T.unlines $ moduleCore cfg

generateCbits :: HwsConfig -> FilePath -> SolvedProtocol -> IO ()
generateCbits (HwsConfig prefix _ role _ cbits _) infile proto = do
  let roleName = map toLower $ show role
      hFileOpts = [roleName ++ "-header", infile, prefix </> cbits </> T.unpack (solvedProtoName proto) ++ "-" ++ roleName ++ "-protocol.h"]
      cFileOpts = ["private-code",        infile, prefix </> cbits </> T.unpack (solvedProtoName proto) ++ "-" ++ roleName ++ "-protocol.c"]
  mapM_ (callProcess "wayland-scanner") [hFileOpts,cFileOpts]

generateFFIWrapper :: HwsConfig -> SolvedProtocol -> IO ()
generateFFIWrapper (HwsConfig prefix _ role _ cbits _) (SolvedProtocol name _ ifaces _ _) = do
  let roleName = map toLower $ show role
      gen (Interface iface _ _ evs reqs _ _) =
        renderCWrapper role iface (if role == Server then evs else addImplicitRequests name iface reqs) <>
        if null evs || role == Server then "" else renderCListener iface
      file = autogenComment <> "#include <" <> name <> "-" <> T.pack roleName <> "-protocol.h>\n"  <> T.unlines (map gen ifaces)
  T.writeFile (prefix </> cbits </> T.unpack name ++ "-" ++ roleName ++ ".c") file

needImplicitRequest :: [Name]
needImplicitRequest =
  [ "wl_registry"
  , "wl_compositor"
  , "wl_callback"
  , "wl_shm"
  , "wl_data_device"
  , "wl_shell"
  , "wl_shell_surface"
  , "wl_seat"
  , "wl_pointer"
  , "wl_keyboard"
  , "wl_touch"
  , "wl_output"
  ]

implicitRequest :: Message
implicitRequest = Message "destroy" "" [] 1

addImplicitRequests :: Name -> Name -> [Message] -> [Message]
addImplicitRequests "wayland" name msgs =
  if name `elem` needImplicitRequest
  then msgs ++ [implicitRequest]
  else msgs
addImplicitRequests _ _ msgs = msgs

addImplicitRequestsToProtocol :: Role -> SolvedProtocol -> SolvedProtocol
addImplicitRequestsToProtocol Server p = p
addImplicitRequestsToProtocol _ (SolvedProtocol name desc ifaces deps enums) =
  let ifaces' = flip map ifaces $ \i -> i {ifaceReqs = addImplicitRequests (ifaceProtocol i) (ifaceName i) (ifaceReqs i)}
  in SolvedProtocol name desc ifaces' deps enums
