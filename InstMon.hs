{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module InstMon where

import Language.Haskell.TH
import Control.Monad
import Control.Applicative

decInstFunctor :: Name -> Q [Dec]
decInstFunctor name = [d| instance Functor $(conT name) where
					fmap f ma = ma >>= (\a -> return (f a))|]

decInstApplicative :: Name -> Q [Dec]
decInstApplicative name = [d| instance Applicative $(conT name) where|]

decInstMonad :: Name -> Q [Dec]
decInstMonad name = [d| instance Monad $(conT name) where|]
