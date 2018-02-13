{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib (json, loadJSONFile)
import Person (Person)

person :: Person
person = $(loadJSONFile @Person "person.json")

main :: IO ()
main = do
    print [json|
        {
            "name": "ryota-ka",
            "age": 24,
            "spouse": null
        }
    |]
    print person
