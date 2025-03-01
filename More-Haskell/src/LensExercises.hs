{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-
From https://www.williamyaoh.com/posts/2019-04-25-lens-exercises.html
-}

import Data.Functor (($>))
import Data.Text (Text)
import Control.Lens hiding ((^.), (.~), (%~))

data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [Text]
  }
  deriving (Show)

makeLenses ''User
makeLenses ''UserInfo

user1 = User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }

infixr 4 .~
(.~) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
lens .~ newValue = runIdentity . lens (const $ Identity newValue)

infixr 4 %~
(%~) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
lens %~ f = runIdentity . lens (Identity . f)

infixl 8 ^.
(^.) :: s -> ((a -> Const a b) -> s -> Const a t) -> a
s ^. lens = getConst $ lens Const s

name' :: Functor f => (Text -> f Text) -> User -> f User
name' fn s = fn (_name s) $> s