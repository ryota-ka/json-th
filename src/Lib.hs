{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( json
    , loadJSONFile
    ) where

import Data.Aeson
    ( eitherDecode
    , FromJSON
    , fromJSON
    , Result (Error, Success)
    , Value
    )
import Language.Haskell.TH
    ( Exp
    , Q
    , runIO
    )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (QuasiQuoter, quoteExp)
    )
import Language.Haskell.TH.Syntax
    ( Lift
    )

import qualified Data.ByteString.Lazy.Char8 as C8

json :: QuasiQuoter
json = QuasiQuoter { quoteExp = buildJSONExp . parseExp }

parseExp :: String -> Value
parseExp str =
    let result = eitherDecode (C8.pack str) in
    case result of
        Left err -> error err
        Right json -> json

buildJSONExp :: Value -> Q Exp
buildJSONExp value = [e| value |]

loadJSONFile :: forall a. (FromJSON a, Lift a) => FilePath -> Q Exp
loadJSONFile filename = do
    str <- runIO $ readFile filename
    let json = parseExp str
    case fromJSON @a json of
        Success x -> [e| x |]
        Error err -> error err
