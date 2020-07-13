{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module EmailBuilder where

import           Data.Aeson                     ( FromJSON
                                                , decode
                                                , parseJSON
                                                , withObject
                                                , (.!=)
                                                , (.:)
                                                , (.:?)
                                                )
import           Data.ByteString.Lazy.UTF8      ( ByteString )
import           Data.ByteString.UTF8           ( toString )
import           Data.List                      ( isPrefixOf )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Encoding            as T
                                                ( decodeUtf8 )
import           Data.Text.Lazy.Encoding       as TLazy
                                                ( decodeUtf8 )
import           GHC.Generics                   ( Generic )
import           Network.Mail.Mime              ( Address(Address)
                                                , Alternatives
                                                , Mail(Mail)
                                                , htmlPart
                                                , plainPart
                                                )
import           Network.Wai.Parse              ( FileInfo
                                                , fileContent
                                                , fileContentType
                                                , fileName
                                                )
import           Text.Regex

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
data EmailHeaders = EmailHeaders
  { from    :: String
  , to      :: [String]
  , cc      :: [String]
  , bcc     :: [String]
  , subject :: String
  }
  deriving Generic

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

list2Tuple :: [a] -> Maybe (a, a)
list2Tuple list = case list of
  [x, y] -> Just (x, y)
  _      -> Nothing

-- | Создает адрес из строки адреса
addressFromString :: String -> Maybe Address
addressFromString string = do
  addressParts <- matchRegex emailRegex string >>= list2Tuple
  return $ addressFromParts addressParts

-- | Создает электронное сообщение на основе данных HTTP-запроса
-- Результирующее сообщение не содержит вложений. Их нужно добавлять отдельно.
mkMail :: EmailData -> Maybe Mail
mkMail emailData = case emailData of
  []                       -> Nothing
  (headersData : bodyData) -> do
    headers  <- (decode $ fileContent headersData)
    mailFrom <- addressFromString $ from headers
    mailTo   <- mapM addressFromString $ to headers
    mailCc   <- mapM addressFromString $ cc headers
    mailBcc  <- mapM addressFromString $ bcc headers
    return $ Mail mailFrom
                  mailTo
                  mailCc
                  mailBcc
                  [("subject", pack $ subject headers)]
                  [mkMailBody bodyData]
