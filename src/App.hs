module App 
  ( module Control.Monad.Reader,
    AppConfig (AppConfig), App, MonadApp, askWorld, runApp, toIO, liftSTM,
    getState, mutateState, askApp, asksApp, initConfig
  ) where


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Monoid
import qualified Katip       as K

import Utils


data AppConfig vola const = AppConfig 
  { storage :: TVar vola
  , logNamespace :: K.Namespace
  , logContext :: K.LogContexts
  , logEnv :: K.LogEnv
  , config :: const
  }


newtype App vola const a = App 
  { unApp :: ReaderT (AppConfig vola const) IO a }
  deriving (Applicative, Functor, Monad, MonadIO,
            MonadReader (AppConfig vola const))


instance K.Katip (App v c) where
  getLogEnv = asks logEnv
  localLogEnv f (App m) =
    App (local (\s -> s { logEnv = f (logEnv s)}) m)


instance K.KatipContext (App v c) where
  getKatipContext = asks logContext
  localKatipContext f (App m) =
    App (local (\s -> s { logContext = f (logContext s)}) m)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (App m) =
    App (local (\s -> s { logNamespace = f (logNamespace s)}) m)


type MonadApp m v c = (MonadReader (AppConfig v c) m, MonadIO m)


runApp :: AppConfig v c -> App v c a -> IO a
runApp c k = runReaderT (unApp k) c


askWorld :: MonadReader (AppConfig v c) m => m (AppConfig v c)
askWorld = ask


toIO :: App v c a -> App v c (IO a)
toIO k = askWorld `for` \c -> runApp c k


liftSTM :: MonadApp m v c => (TVar v -> STM v') -> m v'
liftSTM fn = do
  storage <- asks storage
  liftIO $ atomically (fn storage)


getState :: MonadApp m v c => m v
getState = liftSTM readTVar


mutateState :: MonadApp m v c => (v -> v) -> m ()
mutateState fn = liftSTM $ \storage -> do
  st <- readTVar storage
  writeTVar storage (fn st)


askApp :: MonadApp m v c => m c
askApp = asks config


asksApp :: MonadApp m v c => (c -> b) -> m b
asksApp sel = asks config `for` sel


initConfig :: K.LogEnv -> TVar v -> c -> AppConfig v c
initConfig le st cfg = AppConfig { logEnv = le
                                 , logContext = mempty
                                 , logNamespace = Data.Monoid.mempty
                                 , storage = st
                                 , config = cfg 
                                 }
