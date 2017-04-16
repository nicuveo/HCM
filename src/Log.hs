-- module

module Log (logInfo, logError) where



-- imports

import           Control.Monad.IO.Class



-- exported functions

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn . ("info:  " ++)

logError :: MonadIO m => String -> m ()
logError = liftIO . putStrLn . ("error: " ++)
