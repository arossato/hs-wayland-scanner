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

renderCArg :: Role -> Arg -> Text -> Text
renderCArg r a an
  | (ArgNewId  (Untyped  _)  ) <- a = error "new_id cannot be a function argument"
  | (ArgNewId  (Typed  _ t)  ) <- a = if r == Server then "struct wl_resource *" <> an else "struct " <> t <> " *" <> an
  | (ArgObject (Untyped  _) _) <- a = "struct wl_proxy *"    <> an
  | (ArgObject (Typed  _ t) _) <- a = if r == Server then "struct wl_resource *" <> an else "struct " <> t <> " *" <> an
  | (ArgValue  _  (TInt  _)  ) <- a = "int32_t "             <> an
  | (ArgValue  _  (TUint _)  ) <- a = "uint32_t "            <> an
  | (ArgValue  _   TFixed    ) <- a = "int32_t "             <> an
  | (ArgValue  _  (TString _)) <- a = "const char *"         <> an
  | (ArgValue  _   TFd       ) <- a = "int32_t "             <> an
  | (ArgArray  _             ) <- a = "struct wl_array *"    <> an

renderCReturn :: Role -> Maybe ObjectType -> Text
renderCReturn _ (Just (Untyped _)) = "void * "
renderCReturn r (Just          t)  = renderCArg r (ArgNewId t) ""
renderCReturn _ Nothing            = "void"

-- | Generate request c wrappers
renderCWrapper :: Role -> Name -> [Message] -> Text
renderCWrapper Server "wl_display" _ = ""
renderCWrapper r iface reqs = unlines' $ map gen reqs
  where
    gen (Message name _ args _) =
      let cName = iface <> (if r == Server then "_send_" else "_") <> name
          (ret, args') = if r == Server then (Nothing, args) else
            case splitArgs args of
              -- a new_id without interface: add wl_interface and version
              (Just (Untyped d), as) ->
                ( Just (Untyped d)
                , as ++
                  [ ArgObject   (Typed d "wl_interface") False
                  , ArgValue "" (TUint Nothing)
                  ]
                )
              (mt, as) -> (mt, as)
          idents = map (T.pack . return) ['a'..'z']
          defArg = if r == Client
                   then "struct " <> iface <> " *" <> iface
                   else "struct wl_resource *resource_"
          cArgs = T.intercalate ", " $ defArg : zipWith (renderCArg r) args' idents
          cArgs'  = T.intercalate ", "  $ take (length args') idents
          cArgs'' = if T.null cArgs' then "" else ", " <> cArgs'
      in T.unlines
           [ renderCReturn r ret <> " ffi_" <> cName <> "(" <> cArgs <> ")"
           , "{"
           , "  return " <> cName <> "(" <> (if r == Server then "resource_" else iface) <> cArgs'' <> ");"
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

