{-# LANGUAGE OverloadedStrings #-}

module EmailBuilderSpec where

import           Test.Hspec
import           EmailBuilder
import           Network.Wai.Parse              ( FileInfo
                                                    ( FileInfo
                                                    , fileName
                                                    , fileContentType
                                                    , fileContent
                                                    )
                                                )
import           Network.Mail.Mime              ( partType
                                                , partEncoding
                                                , partContent
                                                , partHeaders
                                                )


spec :: Spec
spec = do
    describe "addPartToBody" $ do
        it "adds plain text part" $ do
            let plainPartData = FileInfo { fileName        = "message.txt"
                                         , fileContentType = "text/plain"
                                         , fileContent     = "Message Content"
                                         }
                part = head $ addPartToBody plainPartData []
            partType part `shouldBe` "text/plain; charset=utf-8"
            partContent part `shouldBe` "Message Content"
        it "adds html part" $ do
            let htmlPartData = FileInfo { fileName        = "message.html"
                                        , fileContentType = "text/html"
                                        , fileContent     = "Message Content"
                                        }
                part = head $ addPartToBody htmlPartData []
            partType part `shouldBe` "text/html; charset=utf-8"
            partContent part `shouldBe` "Message Content"

    describe "appendAttachment" $ it "appends attachment" $ do
        let filePartData = FileInfo { fileName        = "README.md"
                                    , fileContentType = "text/markdown"
                                    , fileContent     = "# Markdown text"
                                    }
            (contentType, name, content) =
                head $ appendAttachment filePartData []
        contentType `shouldBe` "text/markdown"
        name `shouldBe` "README.md"
        content `shouldBe` "# Markdown text"
