{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pessoa where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius
import Prelude
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Network.HTTP.Simple


-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b

getPessoaR :: Handler Html
getPessoaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
    arduinos <- runDB $ selectList [] [Asc ArduinoName]
    cardid <- httpLBS "http://187.21.121.25:8081/card"
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Pessoa"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_jquery_validate_js)
        addScript $ (StaticR js_additional_methods_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/pessoa.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
        <br>
        <main>
         <div class="row">
          <div class="col s6 offset-s3 valign">
            <div class="card blue-grey darken-1">
              <div class="card-content white-text">
                <span class="card-title">Cadastro de Pessoa</span>
                  <form action=@{PessoaR} id="pessoaForm" name="pessoaForm" novalidate="novalidate" method=post>
                   <div class="input-field">
                     <input value="" name="pessoa_nome" id="pessoa_nome" type="text" class="validate">
                     <label class="active white-text" for="pessoa_nome">Nome da Pessoa
                   <div class="input-field">
                     <input value="" name="pessoa_cpf" id="pessoa_cpf" type="text" class="validate" onkeyup="">
                     <label class="active white-text" for="pessoa_cpf">CPF
                   <label class="white-text">Arduino para Escanear o Cartão
                   <br>
                   <select id="arduinoIp" name="arduinoIp">
                    <option value="" disabled selected>Qual Arduino?
                    $forall (Entity arid arduino) <- arduinos
                      <option value="#{arduinoIp arduino}">#{arduinoName arduino}
                   <div class="input-field">
                     <input class="white-text" value="CartãoId" id="cartaoID" name="cartaoID" type="text" class="validate" readonly="readonly">
                     <label class="white-text" for="cartaoID">Cartão da pessoa
                     <button class="btn waves-effect waves-light" id="UpdateID" type="button" onclick="updateID();">Atualizar RFID
                     <br>
                     <br>
                   <button class="btn waves-effect waves-light" type="submit" name="action">Cadastrar
                    <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")


postPessoaR :: Handler Html
postPessoaR = do
        maybeId <- lookupSession "ID"
        idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
        nome <- runInputPost $ ireq textField "pessoa_nome"
        cpf <- runInputPost $ ireq textField "pessoa_cpf"
        cartaoID <- runInputPost $ ireq textField "cartaoID"
        pid <- runDB $ insert $ Pessoa nome cartaoID cpf
        defaultLayout $ do
                setTitle "ⓅⒶⒸ - Pessoa"
                addStylesheet $ (StaticR css_materialize_css)
                addScript $ (StaticR js_jquery_js)
                addScript $ (StaticR js_materialize_js)
                toWidget $(juliusFile "templates/admin.julius")
                toWidget $(luciusFile "templates/admin.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                 <main>
                    Pessoa #{nome} inserida com sucesso!
                |]
                $(whamletFile "templates/footer.hamlet")


getPessoaPerfilR :: PessoaId -> Handler Html
getPessoaPerfilR pid = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    pessoa <- runDB $ get404 pid
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Pessoa"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
         <main>
          <br>
           <br>
            <div class="row">
              <div class="col s6 offset-s3 valign">
               <div class="card blue-grey darken-1">
                <div class="card-content white-text">
                 <span class="card-title">PESSOA</span>
                 <br>
                     <p> Nome da Pessoa : #{pessoaNome pessoa}
                     <p> CPF da Pessoa : #{pessoaCpf pessoa}
                     <p> Cartão da Pessoa : #{pessoaCartao pessoa}
                <br>
                <div class="card-action">
                  <form action=@{EditPessoaR}  method=post>
                   <input type="hidden" id="pid" name="pid" value=#{fromSqlKey pid}>
                   <button class="btn waves-effect waves-light" type="submit" name="action">Editar
                     <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")

postEditPessoaR :: Handler Html
postEditPessoaR = do
      maybeId <- lookupSession "ID"
      idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
      pid  <- runInputPost $ ireq hiddenField "pid"
      pessoa <- runDB $ selectList [SalaId Database.Persist.Postgresql.==. pid] []
      arduinos <- runDB $ selectList [] [Asc ArduinoName]
      defaultLayout $ do
        setTitle "ⓅⒶⒸ - Pessoa"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_jquery_validate_js)
        addScript $ (StaticR js_additional_methods_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/pessoa.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
        <main>
         <div class="row">
          <div class="col s6 offset-s3 valign">
            <div class="card blue-grey darken-1">
              <div class="card-content white-text">
                <span class="card-title">Cadastro de Pessoa</span>
                  <form action=@{AltPessoaR} id="pessoaForm" name="pessoaForm" novalidate="novalidate" method=post>
                   <div class="input-field">
                     <input value="" name="pessoa_cpf" id="pessoa_cpf" type="text" class="validate" onkeyup="">
                     <label class="active white-text" for="pessoa_cpf">CPF
                   <label class="white-text">Arduino para Escanear o Cartão
                   <br>
                   <select id="arduinoIp" name="arduinoIp">
                    <option value="" disabled selected>Qual Arduino?
                    $forall (Entity arid arduino) <- arduinos
                      <option value="#{arduinoIp arduino}">#{arduinoName arduino}
                   <div class="input-field">
                     <input class="white-text" value="CartãoId" id="cartaoID" name="cartaoID" type="text" class="validate" readonly="readonly">
                     <label class="white-text" for="cartaoID">Cartão da pessoa
                     <button class="btn waves-effect waves-light" id="UpdateID" type="button" onclick="updateID();">Atualizar RFID
                     <br>
                     <br>
                   <input type="hidden" id="pid" name="pid" value=#{fromSqlKey pid}>
                   <button class="btn waves-effect waves-light" type="submit" name="action">Cadastrar
                    <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")

postAltPessoaR :: Handler Html
postAltPessoaR = do
   maybeId <- lookupSession "ID"
   idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
   cpf <- runInputPost $ ireq textField "pessoa_cpf"
   cartaoID <- runInputPost $ ireq textField "cartaoID"
   pid <- runInputPost $ ireq hiddenField "pid"
   runDB $ Database.Persist.Postgresql.update pid [PessoaCpf Database.Persist.Postgresql.=. cpf]
   runDB $ Database.Persist.Postgresql.update pid [PessoaCartao Database.Persist.Postgresql.=. cartaoID]
   defaultLayout $ do
           setTitle "ⓅⒶⒸ - Pessoa"
           addStylesheet $ (StaticR css_materialize_css)
           addScript $ (StaticR js_jquery_js)
           addScript $ (StaticR js_materialize_js)
           toWidget $(juliusFile "templates/admin.julius")
           toWidget $(luciusFile "templates/admin.lucius")
           $(whamletFile "templates/header.hamlet")
           [whamlet|
            <main>
               Pessoa Atualizada com Sucesso
           |]
           $(whamletFile "templates/footer.hamlet")

postPessoaPerfilR :: PessoaId -> Handler Html
postPessoaPerfilR pid = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    runDB $ Database.Persist.Postgresql.delete pid
    redirect ListaPessoaR

getListaPessoaR :: Handler Html
getListaPessoaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    pessoas <- runDB $ selectList [] [Asc PessoaNome]
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Pessoa"
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
                                    Pessoas
                                <th>
                        <tbody>
                            $forall (Entity pid pessoa) <- pessoas
                                <tr>
                                 <li class="divider"></li>
                                    <td>
                                        <a href=@{PessoaPerfilR pid}>
                                            #{pessoaNome pessoa}
                                    <td>
                                        <form action=@{PessoaPerfilR pid} method=post>
                                            <input class="btn waves-effect waves-light" type="submit" value="Apagar">
        |]
        $(whamletFile "templates/footer.hamlet")
