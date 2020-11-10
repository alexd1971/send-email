-- |

module Logs where

import           System.IO                      ( stdout )
import           System.Log                     ( Priority(INFO) )
import           System.Log.Logger              ( setHandlers
                                                , setLevel
                                                , updateGlobalLogger
                                                , rootLoggerName
                                                , infoM
                                                , errorM
                                                )
import           System.Log.Handler             ( setFormatter )
import           System.Log.Formatter           ( simpleLogFormatter )
import           System.Log.Handler.Simple      ( streamHandler )

setupLogger :: IO ()
setupLogger = do
  handler' <- streamHandler stdout INFO
  let handler = setFormatter handler' $ simpleLogFormatter "$time $prio\t$msg"
  updateGlobalLogger rootLoggerName (setLevel INFO . setHandlers [handler])

logInfo :: String -> IO ()
logInfo = infoM rootLoggerName

logError :: String -> IO ()
logError = errorM rootLoggerName
