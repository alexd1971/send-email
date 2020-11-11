{-# LANGUAGE OverloadedStrings #-}

module RequestHandler where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.UTF8      ( fromString )
import           Network.HaskellNet.Auth        ( AuthType(LOGIN) )
import           Network.HaskellNet.SMTP        ( authenticate
                                                , closeSMTP
                                                , sendMimeMail2
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
import           Network.Mail.Mime              ( Mail
                                                , addAttachmentsBS
                                                )
import           Network.Wai                    ( Application
                                                , Request
                                                , Response
                                                , ResponseReceived
                                                , responseLBS
                                                , remoteHost
                                                )
import           Network.Wai.Parse              ( lbsBackEnd
                                                , parseRequestBody
                                                )

import           Config
import           EmailBuilder                   ( attachments
                                                , mkMail
                                                , emailSender
                                                , emailRcpt
                                                , emailSubject
                                                )
import           Logs

-- | Формирует ответ сервера
--
-- Параметры:
-- `status :: Status` - HTTP-статус ответа
-- `text :: String` - текст ответа
mkResponse :: Status -> String -> Response
mkResponse status text =
  responseLBS status [(hContentType, "text/plain")] $ fromString text

type Responder = Response -> IO ResponseReceived

-- | Отправляет сообщение
sendEmail :: Mail -> ConfigReader (Either String ())
sendEmail email = do
  config <- ask
  let smtpServer   = server config
      smtpLogin    = login config
      smtpPassword = password config
      port         = fromInteger $ tlsPort config
  lift
    .  logInfo
    $  "Connecting to "
    <> smtpServer
    <> " on port "
    <> show port
    <> "..."
  smtpConnection <- lift
    (connectSMTPSTARTTLSWithSettings
      smtpServer
      defaultSettingsSMTPSTARTTLS { sslPort = port }
    )
  lift . logInfo $ "...connected"
  lift . logInfo $ "Authentication..."
  authenticated <- lift
    $ authenticate LOGIN smtpLogin smtpPassword smtpConnection
  if authenticated
    then do
      lift . logInfo $ "...success"
      lift . logInfo $ "Sending email..."
      lift . sendMimeMail2 email $ smtpConnection
      lift . logInfo $ "...success"
      lift . logInfo $ "Closing connection..."
      lift . closeSMTP $ smtpConnection
      lift . logInfo $ "...closed"
      return $ Right ()
    else do
      lift . logError $ "...fail"
      return $ Left "SMTP Authentication error"

-- | Обрабатывает запрос к серверу
--
-- * Формирует сообщение на основе данных из тела запроса
-- * Отправляет сообщение
-- * Формирует и возвращает ответ клиенту по результатам отправки сообщения
handleRequest :: Request -> Responder -> ConfigReader ResponseReceived
handleRequest request responder = do
  lift . logInfo $ "Recieved request from " <> (show . remoteHost) request
  mailData <-
    (snd <$>) <$> (snd <$> (lift (parseRequestBody lbsBackEnd request)))
  case mkMail mailData of
    Nothing -> do
      lift . logError $ "Bad request"
      lift . responder $ mkResponse status400 "Bad request"
    Just e -> do
      let email   = addAttachmentsBS (attachments $ tail mailData) e
          sender  = emailSender email
          rcpt    = emailRcpt email
          subject = emailSubject email
      lift
        .  logInfo
        $  "Sending \""
        <> subject
        <> "\" from "
        <> sender
        <> " to "
        <> rcpt
      result <- sendEmail email
      case result of
        Right () -> do
          lift
            .  logInfo
            $  "Email from "
            <> sender
            <> " to "
            <> rcpt
            <> " sent successfully"
          lift . responder $ mkResponse status200 "Ok"
        Left message -> do
          lift . logError $ message
          lift . responder $ mkResponse status500 message

-- | Создает обработчик запросов сервера
mkApplication :: ConfigReader Application
mkApplication = do
  config <- ask
  return $ \request responder ->
    runReaderT (handleRequest request responder) $ config
