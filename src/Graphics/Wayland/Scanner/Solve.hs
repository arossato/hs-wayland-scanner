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
  let getArg (ArgObject (Typed _ n) _) = Just n
      getArg (ArgNewId  (Typed _ n)  ) = Just n
      getArg  _                        = Nothing
      getDeps (Message _ _ as _)       = mapMaybe getArg as
      collectArgs (Interface _ _ _ evs reqs _ _) =
        concatMap getDeps evs ++ concatMap getDeps reqs
      args = nub $ concatMap collectArgs ifaces
      checkArg iface =
        case Map.lookup iface ifaceMap of
          Just  i -> if ifaceProtocol i /= name then Just $ ifaceProtocol i else Nothing
          Nothing -> error $ "Unsolved external dependency: unknown interface '" <>
                     T.unpack iface <> "' (protocol '" <> T.unpack name <> "')"
  in SolvedProtocol name desc ifaces
     (nub $ mapMaybe checkArg args)
     (Map.unionsWith Set.union $ map collectEnumTypes ifaces)

collectEnumTypes :: Interface -> EnumMap
collectEnumTypes (Interface name _ _ evs reqs _ _) =
  let nsEnum e =
        case T.splitOn "." e of
          [_,_] -> e
          _     -> name <> "." <> e
      getArgEnum (ArgValue _ (TInt  (Just e))) = Just (nsEnum e, Set.singleton EInt )
      getArgEnum (ArgValue _ (TUint (Just e))) = Just (nsEnum e, Set.singleton EUint)
      getArgEnum  _                            = Nothing
      getEnum (Message _ _ as _) = mapMaybe getArgEnum as
      collectedEnums = nub $ concatMap getEnum evs ++ concatMap getEnum reqs
   in Map.fromListWith Set.union collectedEnums
