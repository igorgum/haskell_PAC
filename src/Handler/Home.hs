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
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                return ""
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Home"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/home.julius")
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/header.hamlet")
        $(whamletFile "templates/haskellchan.hamlet")
        $(whamletFile "templates/footer.hamlet")
