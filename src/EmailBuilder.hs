{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module EmailBuilder where

import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , withObject
                                                , (.!=)
                                                , decode
                                                , (.:)
                                                , (.:?)
                                                )
import           Network.Wai.Parse              ( FileInfo
                                                , fileContentType
                                                , fileName
                                                , fileContent
                                                )
import           Data.ByteString.Lazy.UTF8      ( ByteString )
import           Network.Mail.Mime              ( Alternatives
                                                , Address(Address)
                                                , Mail
                                                    ( Mail
                                                    , mailFrom
                                                    , mailTo
                                                    , mailCc
                                                    , mailBcc
                                                    , mailHeaders
                                                    , mailParts
                                                    )
                                                , plainPart
                                                , htmlPart
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Lazy.Encoding       as TLazy
                                                ( decodeUtf8 )
import           Data.Text.Encoding            as T
                                                ( decodeUtf8 )
import           Text.Regex
import           GHC.Generics                   ( Generic )
import           Data.List                      ( isPrefixOf )
import           Data.List.Safe                as Safe
import           Control.Applicative
import           Data.Maybe                     ( fromMaybe
                                                , fromJust
                                                , isJust
                                                )
import           Data.ByteString.UTF8           ( toString )

-- | Данные сообщения, полученные из MultiPart HTTP-запроса
-- Представляют собой список частей запроса
type EmailData = [PartData]

-- | Часть Multipart HTTP-запроса
type PartData = FileInfo ByteString

-- | Тело сообщения
-- Включает в себя text/plain и text/html варианты сообщения
type EmailBody = Alternatives

-- | Данные вложения
type Attachment = (Text, Text, ByteString)

-- | Заголовки сообщения
data EmailHeaders = EmailHeaders { from    ::  String
                         , to      ::  [String]
                         , cc      ::  [String]
                         , bcc     ::  [String]
                         , subject ::  String
                         } deriving Generic

-- Создаем возможность декодирования заголовков из JSON
instance FromJSON EmailHeaders where
    parseJSON = withObject "MailData" $ \obj -> do
        from    <- obj .: "from"
        to      <- obj .: "to"
        cc      <- obj .:? "cc" .!= []
        bcc     <- obj .:? "bcc" .!= []
        subject <- obj .: "subject"
        return $ EmailHeaders { .. }

-- | Создает тело письма на основе данных из HTTP-запроса
mkMailBody :: EmailData -> EmailBody
mkMailBody = foldr addPartToBody []

-- | Добавляет часть к телу сообщения
addPartToBody :: PartData -> EmailBody -> EmailBody
addPartToBody partData body
    | isPlainPartData partData
    = plainPart (TLazy.decodeUtf8 $ fileContent partData) : body
    | isHtmlPartData partData
    = htmlPart (TLazy.decodeUtf8 $ fileContent partData) : body
    | otherwise
    = body

-- | Добавляет данные вложения в список
appendAttachment :: PartData -> [Attachment] -> [Attachment]
appendAttachment partData as
    | isPlainPartData partData
    = as
    | isHtmlPartData partData
    = as
    | otherwise
    = ( T.decodeUtf8 $ fileContentType partData
      , T.decodeUtf8 $ fileName partData
      , fileContent partData
      )
        : as

-- | Возвращает список данных вложений, полученных из HTTP-запроса
attachments :: EmailData -> [Attachment]
attachments = foldr appendAttachment []

-- | Проверяет, содержит ли часть HTTP-запроса данные текстового варианта
-- сообщения
isPlainPartData :: PartData -> Bool
isPlainPartData fileInfo =
    "text/plain" `isPrefixOf` toString (fileContentType fileInfo)

-- | Проверяет, содержит ли часть HTTP-запроса данные HTML-варианта
-- сообщения
isHtmlPartData :: PartData -> Bool
isHtmlPartData fileInfo =
    "text/html" `isPrefixOf` toString (fileContentType fileInfo)

-- | Регулярное выражения для выделения из строки имени и электроного адреса
-- Строка должна иметь вид: "User Name<user@email.com>"
emailRegex :: Regex
emailRegex = mkRegex "^(.*)?<(.+)>$"

-- | Создает адрес из строковых значений имени и электронного адреса
addressFromParts :: (String, String) -> Address
addressFromParts (name, email) =
    let maybeName = if null name then Nothing else Just $ pack name
    in  Address maybeName $ pack email

-- | Создает адрес из строки адреса
addressFromString :: String -> Maybe Address
addressFromString string =
    addressFromParts
        .   (\[name, email] -> (name, email))
        <$> matchRegex emailRegex string
        <|> pure (Address Nothing $ pack string)

-- | Создает электронное сообщение на основе данных HTTP-запроса
-- Результирующее сообщение не содержит вложений. Их нужно добавлять отдельно.
mkMail :: EmailData -> Maybe Mail
mkMail (headersData : bodyData) =
    let maybeHeaders = (decode $ fileContent headersData) :: Maybe EmailHeaders
    in
        if isJust maybeHeaders
            then
                let headers = fromJust maybeHeaders
                in
                    Just Mail
                        { mailFrom = fromJust . addressFromString $ from headers
                        , mailTo      = map (fromJust . addressFromString)
                                            $ to headers
                        , mailCc      = map (fromJust . addressFromString)
                                            $ cc headers
                        , mailBcc     = map (fromJust . addressFromString)
                                            $ bcc headers
                        , mailHeaders = [("subject", pack $ subject headers)]
                        , mailParts   = [mkMailBody bodyData]
                        }
            else Nothing
