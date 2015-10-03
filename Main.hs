{-# LANGUAGE TemplateHaskell #-}

import InstMon

data MyMonad a = MyMonad a

decInstMonad ''MyMonad
decInstFunctor ''MyMonad
