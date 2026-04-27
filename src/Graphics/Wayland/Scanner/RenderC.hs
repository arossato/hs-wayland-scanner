{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.Scanner.RenderC
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports some functions for rendering the AST into C.
------------------------------------------------------------------------
module Graphics.Wayland.Scanner.RenderC where

import qualified Data.Text as T
import Data.Text (Text)

import Graphics.Wayland.Scanner.Text
import Graphics.Wayland.Scanner.Types

renderCArg :: Role -> Arg -> Name -> Text
renderCArg role arg argName =
  case arg of
    (ArgNewId _ (Untyped  _)   ) -> error "new_id cannot be a function argument"
    (ArgNewId _ (Typed  _ t)   ) -> if role == Server
                                    then "struct wl_resource *" <> argName
                                    else "struct " <> t <> " *" <> argName
    (ArgObject _ (Typed  _ t) _) -> if role == Server
                                    then "struct wl_resource *" <> argName
                                    else "struct " <> t <> " *" <> argName
    (ArgObject _ (Untyped  _) _) -> "struct wl_proxy *" <> argName
    (ArgValue  _ _  (TInt  _)  ) -> "int32_t "          <> argName
    (ArgValue  _ _  (TUint _)  ) -> "uint32_t "         <> argName
    (ArgValue  _ _   TFixed    ) -> "int32_t "          <> argName
    (ArgValue  _ _  (TString _)) -> "const char *"      <> argName
    (ArgValue  _ _   TFd       ) -> "int32_t "          <> argName
    (ArgArray  _ _             ) -> "struct wl_array *" <> argName

renderCReturn :: Role -> Maybe ObjectType -> Text
renderCReturn _    (Just (Untyped _)) = "void * "
renderCReturn role (Just          t)  = renderCArg role (ArgNewId "" t) ""
renderCReturn _    Nothing            = "void"

-- | Generate request c wrappers
renderCWrapper :: Role -> Name -> [Message] -> Text
renderCWrapper Server "wl_display" _ = ""
renderCWrapper role iface reqs = unlines' $ map gen reqs
  where
    gen (Message name _ args _) =
      let cName = iface <> (if role == Server then "_send_" else "_") <> name
          (ret, args') = if role == Server then (Nothing, args) else
            case splitArgs args of
              -- a new_id without interface: add wl_interface and version
              (Just (Untyped d), as) ->
                ( Just (Untyped d)
                , as ++
                  [ ArgObject   "" (Typed d "wl_interface") False
                  , ArgValue "" "" (TUint Nothing)
                  ]
                )
              (mt, as) -> (mt, as)
          idents = map (T.pack . return) ['a'..'z']
          defArg = if role == Client
                   then "struct " <> iface <> " *" <> iface
                   else "struct wl_resource *resource_"
          cArgs   = T.intercalate ", "  $ defArg : zipWith (renderCArg role) args' idents
          cArgs'  = T.intercalate ", "  $ take (length args') idents
          cArgs'' = if T.null cArgs' then "" else ", " <> cArgs'
      in T.unlines
           [ renderCReturn role ret <> " ffi_" <> cName <> "(" <> cArgs <> ")"
           , "{"
           , "  return " <> cName <> "(" <> (if role == Server then "resource_" else iface) <> cArgs'' <> ");"
           , "}"
           ]

renderCListener :: Name -> Text
renderCListener cName =
  T.unlines
  [ "int ffi_" <> cName <> "_add_listener(struct " <> cName <> " *" <>
    cName <> ", const struct " <> cName <>"_listener *listener, void *data)"
  , "{"
  , "  " <> cName <> "_add_listener(" <> cName <> ", listener, data);"
  , "}"
  ]
