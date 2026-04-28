{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.Scanner.Render
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports some functions for rendering the AST into
-- Haskell.
------------------------------------------------------------------------
module Graphics.Wayland.Scanner.Render where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)

import Graphics.Wayland.Scanner.Template
import Graphics.Wayland.Scanner.Text
import Graphics.Wayland.Scanner.Types

-- | Name is the interface where the Arg is occuring
renderArg :: Name -> Arg -> Text
renderArg iface arg =
  case arg of
    (ArgNewId  _ (Untyped  _)  ) -> "Ptr WlInterface -> Word32 -> Word32" -- A special case: new_id without interface
    (ArgNewId  n (Typed  d t)  ) -> "Ptr " <> toHsType t <> " "  <> formatArgComment (formatName n d)
    (ArgObject n (Typed d  t) b) -> "Ptr " <> toHsType t <> " "  <> formatArgComment (formatName n d) <> maybeNull b
    (ArgObject n (Untyped  d) b) -> "Ptr () " <> formatArgComment ((formatName n d) <> " (Opaque pointer: cast with 'castPtr')") <> maybeNull b
    (ArgValue  n d  (TInt  e)  ) -> maybe "Int32"  formatEnum e  <> " " <> formatArgComment (formatName n d)
    (ArgValue  n d  (TUint e)  ) -> maybe "Word32" formatEnum e  <> " " <> formatArgComment (formatName n d)
    (ArgValue  n d   TFixed    ) -> "Int32 "       <> formatArgComment (formatName n d)
    (ArgValue  n d  (TString b)) -> "CString "     <> formatArgComment (formatName n d) <> maybeNull b
    (ArgValue  n d   TFd       ) -> "CInt "        <> formatArgComment (formatName n d)
    (ArgArray  n d             ) -> "Ptr WlArray " <> formatArgComment (formatName n d)
  where
    maybeNull b = if b then " (__Maybe @NULL@__)" else ""
    formatName "" d = d
    formatName n "" = "__" <> n <> "__"
    formatName n d  = "__" <> n <> "__" <> ": " <> d
    formatEnum e =
      case T.splitOn "." e of
        [x,y] -> T.toUpper $ x     <> "_" <> y
        _     -> T.toUpper $ iface <> "_" <> e

-- | Name is the iface name
renderReturn :: Name -> Maybe ObjectType -> Text
renderReturn _  Nothing           = "IO ()"
renderReturn n (Just (Typed d t)) =
  "IO (" <> renderArg n (ArgNewId "" $ Typed "" t) <> ") " <> formatArgComment d
renderReturn _ (Just (Untyped _)) =
  "Ptr WlInterface "      <> formatArgComment "Interface descriptor (e.g. 'wl_compositor_interface')" <>
  "\n    -> Word32 "      <> formatArgComment "Version to bind" <>
  "\n    -> IO (Ptr ()) " <> formatArgComment "Opaque pointer to the bound object; cast with 'castPtr'"

-- | Generate request bidings
renderRequest :: RoleRender -> Name -> [Text]
renderRequest (RoleRender r _ smsgs reqSep _ _ _ defArg) iface =
  formatHaddockSubSec (toHsType iface <> if r == Server then " Events" else " Requests") : map gen smsgs
  where
    gen (Message name desc args since) =
      let (ret, args') = if r == Server then (Nothing, args) else splitArgs args
          cName        = iface <> reqSep <> name
          hsArgs       = defArg : map (renderArg iface) args'
          comment      = formatTopLevelComment $ desc <> "\n__Since version " <> (T.pack $ show since) <> "__"
          typeSig      = formatArgs $ hsArgs ++ [renderReturn iface ret]
      in T.unlines
         [ comment
         , "foreign import ccall \"ffi_" <> cName <> "\""
         , "  " <> cName <> " :: "
         , "    " <> typeSig
         ]

-- | Generate storable instances for listeners
renderStorable :: RoleRender -> Name -> [Text]
renderStorable (RoleRender _ [] _ _ _ _ _ _) _ = []
renderStorable (RoleRender _ rmsgs _ _ suffix structSuffix _ _) iface =
  let hsIface = toHsType iface
      structName = hsIface <> structSuffix
      pokeLines =
        [ "    (#poke struct " <> iface <> suffix <> ", " <>
          name <> ") ptr (" <> toHsFcn (iface <> "_" <> name) <> " l)"
        | (Message name _ _ _) <- rmsgs
        ]
  in [ "-- | Storable instance for the " <> iface <> " interface"
     , "instance Storable " <> structName <> " where"
     , "  sizeOf    _ = #size struct "      <> iface <> suffix
     , "  alignment _ = #alignment struct " <> iface <> suffix
     , ""
     , "  poke ptr l = do"
     ] ++ pokeLines ++
     [ ""
     , "  peek _ = error \"peek not implemented\""
     ]

-- | Render 'enum' values
renderEntry :: Name -> EnumEntry -> Text
renderEntry ename (EnumEntry entry desc) =
  let name = T.toUpper $ ename <> "_" <> entry
  in T.unlines
     [ formatTopLevelComment desc
     , "pattern " <> name <> " :: " <> T.toUpper ename
     , "pattern " <> name <> " = #const " <> name
     ]

-- | Render 'enum'
renderEnum :: EnumMap -> Name -> EnumDecl -> Text
renderEnum enumMap iface (EnumDecl name desc bf enums) =
  let ename = T.toUpper $ iface <> "_" <> name
      check et =
        case Set.toList et of
          []      -> ("Word32", Nothing)
          [EInt]  -> ("Int32" , Nothing)
          [EUint] -> ("Word32", Nothing)
          (_:_)   -> ("Int32" , Just $ "__Note: enum " <> ename <> " used as both int and uint; using Int32__")
      (etype, warning) =
        case Map.lookup (iface <> "." <> name) enumMap of
          Just et -> check et
          Nothing -> ("Word32", Nothing)
  in T.unlines $
     [ formatTopLevelComment
       (desc <> maybe "" id warning <>
        if bf
        then "\n__Bitmask__: values of this enum are bitflags and may be combined using bitwise OR."
        else "")
     , "type " <> ename <> " = " <> etype
     ] ++ map (renderEntry ename) enums

-- | Render wrappers for callbacks
renderCallback :: RoleRender -> Name -> [Text]
renderCallback (RoleRender _ []  _ _ _ _ _ _) _ = []
renderCallback (RoleRender _ rmsgs _ _ _ _ defArgs _) iface = map gen rmsgs
  where
    gen (Message name desc args since) =
      let hsName = toHsType (iface <> "_" <> name) <> "Cb"
          comment = formatTopLevelComment $ desc <> "\n__Since version " <> (T.pack $ show since) <> "__"
          args'   = formatArgs $ defArgs ++ map (renderArg iface) args ++ ["IO ()"]
          typeSig = T.unwords ["type", hsName, "=\n   ", args']
          wrapper = T.unwords
                    [ "foreign import ccall \"wrapper\""
                    , "mk" <> hsName
                    , "::"
                    , hsName
                    , "-> IO (FunPtr " <> hsName <> ")"
                    ]
      in T.unlines [comment, typeSig, wrapper]

-- | Render the listener
renderListener :: RoleRender -> Name -> [Text]
renderListener (RoleRender _ []  _ _ _ _ _ _) _ = []
renderListener (RoleRender r rmsgs _ _ _ structSuffix _ _) iface =
  let hsIface = toHsType iface
      fieldName n = iface <> "_" <> n
      structName = hsIface <> structSuffix
      fields =
        [ toHsFcn (fieldName n) <> " :: FunPtr " <> toHsType (fieldName n) <>
          "Cb -- ^ See '" <> toHsType (fieldName n) <> "Cb'"
        | (Message n _ _ _) <- rmsgs]
      fieldsTxt =
        case fields of
          [] -> []
          (f:fs) -> "  { " <> f : map ("  , " <>) fs  ++ ["  }"]
  in [ formatHaddockSubSec (toHsType iface <> if r == Client then " Events" else " Requests")
     , "data " <> structName <> " = " <> structName
     ] ++ fieldsTxt

-- | Render an 'Interface': produce the 'enum's and the generated
-- file.
renderInterface :: Role -> EnumMap -> Interface -> (Text,Text)
renderInterface r enumMap i@(Interface iface _ desc evs _ enums _) =
  let callbacks = renderCallback (roleRender r i) iface
      listener  = renderListener (roleRender r i) iface
      rendEnums = T.unlines $ map (renderEnum enumMap iface) enums
      storable  = renderStorable (roleRender r i) iface
      requests  = if r == Server && iface == "wl_display"
                  then []
                  else renderRequest (roleRender r i) iface
      ifacePtr  = [ "foreign import ccall \"&" <> iface <> "_interface\""
                  , "  " <> iface <> "_interface :: Ptr WlInterface"
                  ]
      addListen = if null evs || r == Server
                  then []
                  else
                    [ "foreign import ccall \"ffi_" <> iface <> "_add_listener\""
                    , "  "  <> iface <> "_add_listener :: Ptr " <> toHsType iface <>
                      " -> Ptr " <> toHsType iface <> "Listener -> Ptr () -> IO CInt"
                    ]

  in (,) rendEnums $ T.unlines $
     [ formatHaddockSec $ toHsType iface
     , formatTopLevelComment desc
     , if iface `elem` ["wl_display", "wl_registry"]
       then "" else "data " <> toHsType iface
     ] ++ listener ++ storable ++ callbacks ++ requests ++ ifacePtr ++ addListen

-- | Render a 'SolvedProtocol'.
renderProtocol :: Role -> SolvedProtocol -> [(Text, Text)]
renderProtocol r (SolvedProtocol _ _ ifaces _ enumMap) = map (renderInterface r enumMap) ifaces

roleRender :: Role -> Interface -> RoleRender
roleRender Client (Interface iface _ _ evs reqs _ _) =
  RoleRender Client evs reqs "_" "_listener"  "Listener"
  [ "Ptr () -- ^ user data"
  , "Ptr "  <> toHsType iface <> " -- ^ The interface '" <> toHsType iface <> "'"
  ] $ "Ptr " <> toHsType iface <> " " <> formatArgComment ("The pointer to the interface '" <> toHsType iface <> "'.")
roleRender Server (Interface _ _ _ evs reqs _ _) =
  RoleRender Server reqs evs "_send_" "_interface" "Interface"
  [ "Ptr WlClient"
  , "Ptr WlResource"
  ] "Ptr WlResource"
