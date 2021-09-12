{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Monad.AppM where

import App.Env (Env)

import Control.Monad.Reader (ReaderT(ReaderT))
newtype AppM m a
  = AppM
  { unApp :: ReaderT Env m a
  }
  deriving (Functor, Applicative, Monad)

