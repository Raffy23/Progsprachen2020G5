{- |
Module      :  Utils
Description :  provides some utilities functions for the Application
License     :  MIT
-}

module Utils where

import Control.Monad (foldM)

foldM' :: (Foldable t, Monad m) => (b -> a -> m b) -> t a -> b -> m b
foldM' a c b = foldM a b c

apply :: (b -> c) -> b -> c
apply f b = f b

apply' :: b -> (b -> c) -> c
apply' b f = f b

apply2' :: a -> b -> (a -> b -> c) -> c
apply2' a b f = f a b