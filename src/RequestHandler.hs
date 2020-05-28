{-# LANGUAGE OverloadedStrings #-}

module RequestHandler where

import           Network.HaskellNet.Auth        ( AuthType(LOGIN) )
import           Network.HaskellNet.SMTP        ( authenticate
                                                , sendMimeMail2
                                                , closeSMTP
                                                )
import           Network.HaskellNet.SMTP.SSL    ( Settings(sslPort)
                                                , connectSMTPSTARTTLSWithSettings
                                                , defaultSettingsSMTPSTARTTLS
                                                )
import           Network.HTTP.Types.Header      ( hContentType )
import           Network.HTTP.Types.Status      ( Status
                                                , status200
                                                , status400
                                                , status500
                                                )
import           Network.Mail.Mime              ( addAttachmentsBS )
import           Network.Wai                    ( Application
                                                , Response
                                                , responseLBS
                                                )
import           Network.Wai.Parse              ( parseRequestBody
                                                , lbsBackEnd
                                                )
import           Data.ByteString.Lazy.UTF8      ( fromString )

import           EmailBuilder                   ( mkMail
                                                , attachments
                                                )

-- | Формирует ответ сервера
response :: Status -> String -> Response
response status text =
    responseLBS status [(hContentType, "text/plain")] $ fromString text

-- | Обработчик запроса
handler :: Application
handler request respond = do
    mailData <- (snd <$>) <$> (snd <$> parseRequestBody lbsBackEnd request)
    let maybeEmail = mkMail mailData
    case maybeEmail of
        Nothing  -> respond $ response status400 "Bad request"
        Just eml -> do
            let email = addAttachmentsBS (attachments $ tail mailData) eml
            smtpConnection <- connectSMTPSTARTTLSWithSettings
                "smtp.gmail.com"
                defaultSettingsSMTPSTARTTLS { sslPort = 587 }
            authenticated <- authenticate LOGIN
                                          "no-reply@allfuneral.com"
                                          "DronKronTron567#$"
                                          smtpConnection
            if authenticated
                then do
                    sendMimeMail2 email smtpConnection
                    closeSMTP smtpConnection
                    respond $ response status200 "Ok"
                else respond $ response status500 "SMTP Authentication error"
