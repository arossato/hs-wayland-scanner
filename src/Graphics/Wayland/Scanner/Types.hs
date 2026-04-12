{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wayland.Scanner.Types
-- Copyright   :  (c) Andrea Rossato 2026
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the AST of the Wayland Message Definition
-- Language.
------------------------------------------------------------------------
module Graphics.Wayland.Scanner.Types where

import Data.Map  (Map)
import Data.Set  (Set)
import Data.Text (Text)

data HwsConfig = HwsConfig
  { genPrefix   :: String
  , hsNameSpace :: String
  , protoRole   :: Role
  , protocols   :: [FilePath]
  , cbitsPrefix :: FilePath
  , srcPrefix   :: FilePath
  } deriving (Show, Read)

data Role = Client | Server deriving (Show, Read, Eq)

type Name = Text

data Protocol = Protocol
  { protoName  :: Name
  , protoDesc  :: Text
  , interfaces :: [Interface]
  } deriving Show

data Interface = Interface
  { ifaceName     :: Name
  , ifaceProtocol :: Name
  , ifaceDesc     :: Name
  , ifaceEvents   :: [Message]
  , ifaceReqs     :: [Message]
  , ifaceEnums    :: [EnumDecl]
  , ifaceVersion  :: Int
  } deriving Show

data Message = Message
  { msgName  :: Name
  , msgDesc  :: Text
  , msgArgs  :: [Arg]
  , msgSince :: Int
  } deriving Show

data ObjectType
  = Typed   Text Name
  | Untyped Text
  deriving (Eq, Show)

data Arg
  = ArgNewId  ObjectType
  | ArgObject ObjectType Bool -- Bool is for allow-null
  | ArgValue  Text WlType
  | ArgArray  Text
  deriving (Eq, Show)

data WlType
  = TInt  (Maybe Name) -- possible enum
  | TUint (Maybe Name) -- possible enum
  | TString Bool       -- Bool is for allow-null
  | TFixed
  | TFd
  deriving (Eq, Show)

data EnumDecl = EnumDecl
  { enumName     :: Name
  , enumDesc     :: Text
  , enumBitfield :: Bool
  , enumValues   :: [EnumEntry]
  } deriving Show

data EnumEntry = EnumEntry
  { entryName   :: Name
  , entryDesc   :: Text
  } deriving Show

data EType = EInt | EUint deriving (Eq, Ord, Show)

type EnumMap = Map Name (Set EType)

data SolvedProtocol = SolvedProtocol
  { solvedProtoName    :: Name
  , solvedProtoDesc    :: Text
  , solvedInterfaces   :: [Interface]
  , solvedDependencies :: [Name]
  , solvedEnums        :: EnumMap
  } deriving Show

-- | In the Wayland Message Definition Language events are messages
-- sent by the server to the client, while requests are messages sent
-- by the client to the server. From the server|client perspective,
-- and the Haskell implementation, the distinction is between received
-- messaged (managed through callbacks implemented via foreign
-- wrappers) and messages to be sent (implemented via foreign
-- functions). So, events are received messages for a client and sent
-- messages for the server, and vice versa. 'RoleRender' is to encode
-- this distinction when rendering the 'SolvedProtocol'.
data RoleRender = RoleRender
  { rrRole           :: Role
  , rrReceiveMsgs    :: [Message]  -- received messages: Client = events  / Server requests
  , rrSendMsgs       :: [Message]  -- messages to send:  Client = requests / Server events
  , rrRequestNameSep :: Text       -- "_" / "_send_"
  , rrStructSuf      :: Text       -- "_listener" / "_interface"
  , rrStructTypeSuf  :: Text       -- "Listener" /  "Interface"
  , rrDefaultCBArgs  :: [Text]     -- Callback default args
  , rrDefaultSmsgArg :: Text       -- Default arg for function sending msgs
  }

-- | A "new_id" is a return type in requests. We retrieve it if needed
-- and remove it from the argument list.
splitArgs :: [Arg] -> (Maybe ObjectType, [Arg])
splitArgs args =
  case [t | ArgNewId t <- args] of
    []  -> (Nothing, args)
    [t] -> (Just  t, filter (not . isNewId) args)
    _   -> error "multiple new_id arguments (unexpected in Wayland)"

isNewId :: Arg -> Bool
isNewId (ArgNewId _) = True
isNewId _            = False

notValue :: Arg -> Bool
notValue (ArgValue _ _) = False
notValue _              = True
