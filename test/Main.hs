{-# LANGUAGE OverloadedStrings #-}

module Main where

import Budget.User
import Budget.Model
import Test.Hspec

instance MonadUserService IO where
  getUser username = return $ Just $ User "username" "password"
  putUser username password = return $ Just username

main :: IO ()
main = hspec test_validatePassword

test_validatePassword :: Spec
test_validatePassword = describe "validatePassword" $ do
  context "with valid password" $ do
    it "should return true" $ do
      result <- validatePassword "username" "password"
      result `shouldBe` True
  context "with invalid password" $ do
    it "should return false" $ do
      result <- validatePassword "username" "bad_password"
      result `shouldBe` False
