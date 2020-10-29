{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module XvsO.Web.Api
  ( GameApi
  , GameRoutes(..)
  
  , MoveData(..)
  , ResultOrMoveData

  , gameApi
  
  , decodeResultOrMoveData
  , encodeResultOrMoveData
  , decodePosition
  , encodePosition
  , encodeBool
  , decodeBool
  ) where


import Servant.WebSockets.API (WebSocketApp)
import GHC.Generics (Generic)
import Servant.API.Generic ((:-), ToServantApi, genericApi)
import Servant ((:>), Proxy(..))
import Servant.WebSockets.Client ()
import Servant.WebSockets.Server ()
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (ToJSON, FromJSON, decode, encode)

import XvsO.Classic.Game

-- | API of the game
type GameApi = ToServantApi GameRoutes

-- | Wrapped API of the game
gameApi :: Proxy GameApi
gameApi = genericApi (Proxy @GameRoutes)

-- | Game routes
newtype GameRoutes routes = GameRoutes
  { gameHost :: routes :-
      "game" :> WebSocketApp
  } deriving stock (Generic)

-- | Data of the move. See 'XvsO.Classic.Model.Player'
data MoveData = MoveData
  { mdStep :: Int
  , mdValue :: XorO
  , mdBoard :: ClassicBoard
  } deriving (Generic)

instance ToJSON MoveData
instance FromJSON MoveData

-- | Result of the game or current step info
type ResultOrMoveData = Either GameResult MoveData

-- | Decode ResultOrMoveData
decodeResultOrMoveData :: ByteString -> Maybe ResultOrMoveData
decodeResultOrMoveData = decode

-- | Encode ResultOrMoveData
encodeResultOrMoveData :: ResultOrMoveData -> ByteString
encodeResultOrMoveData = encode

-- | Decode Position
decodePosition :: ByteString -> Maybe Position
decodePosition = decode

-- | Encode Position
encodePosition :: Position -> ByteString
encodePosition = encode

-- | Decode Bool
encodeBool :: Bool -> ByteString
encodeBool = encode

-- | Encode Bool
decodeBool :: ByteString -> Maybe Bool
decodeBool = decode
