module App 
  ( AppConfig (AppConfig), App, askWorld, runApp, toIO, liftSTM,
    getState, mutateState, askApp, asksApp, initConfig
  ) where


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Monoid
import qualified Katip       as K

import Utils


data AppConfig a b = AppConfig 
  { storage :: TVar a
  , logNamespace :: K.Namespace
  , logContext :: K.LogContexts
  , logEnv :: K.LogEnv
  , config :: b
  }

newtype App t0 t1 a = App { unApp :: ReaderT (AppConfig t0 t1) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (AppConfig t0 t1))

instance K.Katip (App t0 t1) where
  getLogEnv = asks logEnv
  localLogEnv f (App m) =
    App (local (\s -> s { logEnv = f (logEnv s)}) m)

instance K.KatipContext (App t0 t1) where
  getKatipContext = asks logContext
  localKatipContext f (App m) =
    App (local (\s -> s { logContext = f (logContext s)}) m)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (App m) =
    App (local (\s -> s { logNamespace = f (logNamespace s)}) m)


runApp :: AppConfig t0 t1 -> App t0 t1 a -> IO a
runApp c k = runReaderT (unApp k) c


askWorld :: App t0 t1 (AppConfig t0 t1)
askWorld = ask


toIO :: App t0 t1 a -> App t0 t1 (IO a)
toIO k = askWorld `for` \c -> runApp c k


liftSTM :: (TVar t0 -> STM a) -> App t0 t1 a
liftSTM fn = do
  storage <- asks storage
  liftIO $ atomically (fn storage)


getState :: App t0 t1 t0
getState = liftSTM readTVar


mutateState :: (t0 -> t0) -> App t0 t1 ()
mutateState fn = liftSTM $ \storage -> do
  st <- readTVar storage
  writeTVar storage (fn st)


askApp :: App t0 t1 t1
askApp = asks config


asksApp :: (t1 -> t2) -> App t0 t1 t2
asksApp sel = asks config `for` sel


initConfig :: K.LogEnv -> TVar a -> b -> AppConfig a b
initConfig le st cfg = AppConfig { logEnv = le
                                 , logContext = mempty
                                 , logNamespace = Data.Monoid.mempty
                                 , storage = st
                                 , config = cfg 
                                 }
