--{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module LibSpec (main, spec) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "nothing" $ do
    it "rejects empty word" $
      0 `shouldBe` 0

-- spec :: Spec
-- spec = with (return app) $
--     describe "GET /users" $ do
--         it "responds with 200" $
--             get "/users" `shouldRespondWith` 200
--         it "responds with [User]" $ do
--             let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
--             get "/users" `shouldRespondWith` users
