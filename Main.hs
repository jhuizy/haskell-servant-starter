{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/health" $ text "OK"

add :: Int -> Int
add n = n

multiply :: Functor f => f Int -> f Int
multiply l = fmap add l
