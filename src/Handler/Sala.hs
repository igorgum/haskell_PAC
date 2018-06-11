{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Sala where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius
import Prelude
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b

getSalaR :: Handler Html
getSalaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
    arduinos <- runDB $ selectList [] [Asc ArduinoName]
    areas <- runDB $ selectList [] [Asc AreaOrdem]
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Sala"
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
                <span class="card-title">Cadastro de Sala</span>
                  <form action=@{SalaR} method=post>
                   <div class="input-field">
                     <label class="active white-text" for="sala_nome">Nome da Sala</label>
                     <input value="" name="sala_nome" id="sala_nome" type="text" class="validate">

                    <br>
                     <div class="input-field">
                      <select name="id" >
                        <option value="" disabled selected>Qual Arduino?</option>
                        $forall (Entity arid arduino) <- arduinos
                          <option value="#{fromSqlKey $ arid}">#{arduinoName arduino}</option>
                      <label class="white-text">Arduino</label>


                     <div class="input-field">
                       <select id="areaDesignada" name="areaDesignada">
                         <option value="" disabled selected>Qual Area?</option>
                         $forall (Entity areaid restoarea) <- areas
                           <option value="#{fromSqlKey $ areaid}">#{areaNome restoarea}</option>
                       <label class="white-text">Area designada</label>

                    <button class="btn waves-effect waves-light" type="submit" name="action">Cadastrar
                      <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")


postSalaR :: Handler Html
postSalaR = do
        maybeId <- lookupSession "ID"
        idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
        nome <- runInputPost $ ireq textField "sala_nome"
        id <- runInputPost $ ireq intField "id"
        area <- runInputPost $ ireq intField "areaDesignada"
        let posx = (Just 0)
        let posy = (Just 0)
        let size = (Just 1)
        sid <- runDB $ insert $ Sala nome (toSqlKey id) (toSqlKey area) posx posy size
        defaultLayout $ do
                setTitle "ⓅⒶⒸ - Sala"
                addStylesheet $ (StaticR css_materialize_css)
                addScript $ (StaticR js_jquery_js)
                addScript $ (StaticR js_materialize_js)
                toWidget $(juliusFile "templates/admin.julius")
                toWidget $(luciusFile "templates/admin.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                 <main>
                    Sala #{nome} inserida com sucesso!
                |]
                $(whamletFile "templates/footer.hamlet")
