{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

module Person
    ( Person (..)
    ) where

import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import Language.Haskell.TH.Syntax (Lift)

data Person
    = Person
    { name   :: String
    , age    :: Int
    , spouse :: Maybe Person
    } deriving (Lift, Show)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \o -> Person
        <$> o .: "name"
        <*> o .: "age"
        <*> o .: "spouse"
