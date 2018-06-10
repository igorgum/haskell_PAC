{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Julius
import Text.Lucius

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        addScript $ (StaticR js_jquery_js)
        toWidget $[whamlet| Hello world |]
