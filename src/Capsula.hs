{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --fazendo a magia acontecer
module Capsula where

import Import
import Prelude

--usado pra retirar o primeiro elemento de um selectList restrito
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

class Vaziozar a where
    vazio :: a
instance (Num a) => Vaziozar a where
    vazio = 0
instance Vaziozar Text where
    vazio = ""

--desemcapsula um Just campo do selectList
desemcapsula :: (Vaziozar a) => Maybe a -> a
desemcapsula (Just a) = a
desemcapsula Nothing = vazio

--desemcapsula uma Just key do selectList
desemcapsula2 :: (Maybe (b,c)) -> b
desemcapsula2 (Just (b,c)) = b

------Antes de class
--desemcapsula :: Maybe Text -> Text
--desemcapsula (Just a) = a
--desemcapsula Nothing = ""

--desemcapsula2 :: Maybe Int -> Int
--desemcapsula2 (Just a) = a
--desemcapsula2 Nothing = 0
