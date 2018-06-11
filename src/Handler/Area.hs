{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Area where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius

-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b
formArea :: Form Area
formArea = renderDivs $ Area
        <$> areq textField "Nome: " Nothing
        <*> areq textField "Link do mapa: " Nothing
        <*> areq intField "Ordem de apresentação: " Nothing

getAreaR :: Handler Html
getAreaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
    (widget,enctype) <- generateFormPost formArea
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Area"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
        <br>
        <main>
         <div class="row">
          <div class="col s6 offset-s3 valign">
            <div class="card blue-grey darken-1">
              <div class="card-content white-text">
                <span class="card-title">Cadastro de Area</span>
                  <form action=@{AreaR} method=post enctype=#{enctype}>
                    ^{widget}
                    <button class="btn waves-effect waves-light" type="submit" name="action">Cadastrar
                      <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")


postAreaR :: Handler Html
postAreaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formArea
    case res of
        FormSuccess area -> do
            areaid <- runDB $ insert area
            defaultLayout $ do
                setTitle "ⓅⒶⒸ - Area"
                addStylesheet $ (StaticR css_materialize_css)
                addScript $ (StaticR js_jquery_js)
                addScript $ (StaticR js_materialize_js)
                toWidget $(juliusFile "templates/admin.julius")
                toWidget $(luciusFile "templates/admin.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                 <main>
                    Area #{fromSqlKey areaid} inserida com sucesso!
                |]
                $(whamletFile "templates/footer.hamlet")
        _ -> redirect HomeR


getListaAreaR :: Handler Html
getListaAreaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    areas <- runDB $ selectList [] [Asc AreaOrdem]
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Area"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
                <main>
                    <table>
                        <thead>
                            <tr>
                                <th>
                                    Areas
                                <th>
                        <tbody>
                            $forall (Entity aid area) <- areas
                                <tr>
                                 <li class="divider"></li>
                                    <td>
                                        <a href=@{AreaPerfilR aid}>
                                            #{areaNome area}
                                    <td>
                                        <form action=@{AreaPerfilR aid} method=post>
                                            <input class="btn waves-effect waves-light" type="submit" value="Apagar">
        |]
        $(whamletFile "templates/footer.hamlet")
