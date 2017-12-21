--{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ApiSpec (main, spec) where

import Api (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $
    describe "GET /games" $
        it "responds with 200" $
            get "/games" `shouldRespondWith` 200
        -- it "responds with [User]" $ do
        --     let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
        --     get "/users" `shouldRespondWith` users
