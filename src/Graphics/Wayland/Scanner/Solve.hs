{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.Scanner.Solve
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports functions to solve `Protocol` dependencies and
-- `enum` types.
------------------------------------------------------------------------
module Graphics.Wayland.Scanner.Solve where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T

import Graphics.Wayland.Scanner.Types

type IfaceMap = Map Name Interface

buildIfaceMap :: [Protocol] -> IfaceMap
buildIfaceMap protos =
  Map.fromList
    [ (ifaceName i, i)
    | p <- protos
    , i <- interfaces p
    ]

-- | Solve 'Protocol' dependencies and 'enum' types.
solveProtocol :: IfaceMap -> Protocol -> SolvedProtocol
solveProtocol ifaceMap (Protocol name desc ifaces) =
  let getArg (ArgObject _ (Typed _ n) _) = Just n
      getArg (ArgNewId  _ (Typed _ n)  ) = Just n
      getArg  _                          = Nothing
      getDeps (Message _ _ as _)       = mapMaybe getArg as
      collectArgs (Interface _ _ _ evs reqs _ _) =
        concatMap getDeps evs ++ concatMap getDeps reqs
      objArgs     = concatMap collectArgs ifaces
      enumResults = map collectEnumTypes ifaces

      -- Merge all individual EnumMaps
      mergedEnums = Map.unionsWith Set.union (map fst enumResults)

      -- Gather enums external interface references
      enumArgs = concatMap snd enumResults

      -- Combine both sources of interface dependencies
      allArgs = nub (objArgs ++ enumArgs)

      checkArg iface =
        case Map.lookup iface ifaceMap of
          Just  i -> if ifaceProtocol i /= name then Just $ ifaceProtocol i else Nothing
          Nothing -> error $ "Unsolved external dependency: unknown interface '" <>
                     T.unpack iface <> "' (protocol '" <> T.unpack name <> "')"

  in SolvedProtocol name desc ifaces (nub $ mapMaybe checkArg allArgs) mergedEnums

collectEnumTypes :: Interface -> (EnumMap, [Name])
collectEnumTypes (Interface name _ _ evs reqs _ _) =
  let qualifyEnum e =
        case T.splitOn "." e of
          [i, _] -> (i, e)                   -- External reference
          _      -> (name, name <> "." <> e) -- Local enum
      getArgEnum (ArgValue _ _ (TInt  (Just e))) = Just (qualifyEnum e, EInt )
      getArgEnum (ArgValue _ _ (TUint (Just e))) = Just (qualifyEnum e, EUint)
      getArgEnum  _                              = Nothing
      getEnum (Message _ _ as _) = mapMaybe getArgEnum as
      -- results contain ((iface, fullyQualifiedEnum), EnumType)
      results = concatMap getEnum evs ++ concatMap getEnum reqs
      -- Build the EnumMap
      enumMap = Map.fromListWith Set.union
                  [ (fqEnum, Set.singleton t) | ((_, fqEnum), t) <- results ]

      -- External interfaces
      extIfaces = nub [ i | ((i, _), _) <- results, i /= name ]

  in (enumMap, extIfaces)
