{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius

-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b
formAdmin :: Form Admin
formAdmin = renderDivs $ Admin
        <$> areq textField "Usuario: " Nothing
        <*> areq passwordField "Senha: " Nothing

getAdminR :: Handler Html
getAdminR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
    (widget,enctype) <- generateFormPost formAdmin
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Admin"
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
                <span class="card-title">Cadastro de Admin</span>
                  <form action=@{AdminR} method=post enctype=#{enctype}>
                    ^{widget}
                    <button class="btn waves-effect waves-light" type="submit" name="action">Cadastrar
                      <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")


postAdminR :: Handler Html
postAdminR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formAdmin
    case res of
        FormSuccess admin -> do
            aid <- runDB $ insert admin
            defaultLayout $ do
                setTitle "ⓅⒶⒸ - Admin"
                addStylesheet $ (StaticR css_materialize_css)
                addScript $ (StaticR js_jquery_js)
                addScript $ (StaticR js_materialize_js)
                toWidget $(juliusFile "templates/admin.julius")
                toWidget $(luciusFile "templates/admin.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                 <main>
                    Admin #{fromSqlKey aid} inserido com sucesso!
                |]
                $(whamletFile "templates/footer.hamlet")
        _ -> redirect HomeR


getADMPerfilR :: AdminId -> Handler Html
getADMPerfilR aid = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    admin <- runDB $ get404 aid
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Admin"
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
                 <span class="card-title">ADMIN</span>
                 <br>
                 <p> Admin: #{adminLogin admin}
                 <br>
                 $if (idText == (adminLogin admin))
                     <p>Senha: #{adminPass admin}
                <br>
                <div class="card-action">
                 $if (idText == (adminLogin admin))
                  <form action=@{EditAdminR}  method=post>
                   <input type="hidden" id="aid" name="aid" value=#{fromSqlKey aid}>
                   <button class="btn waves-effect waves-light" type="submit" name="action">Editar
                     <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")


postEditAdminR :: Handler Html
postEditAdminR = do
     maybeId <- lookupSession "ID"
     idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
     aid <- runInputPost $ ireq hiddenField "aid"
     admin <- runDB $ selectList [AdminId ==. aid] []
     defaultLayout $ do
        setTitle "ⓅⒶⒸ - Admin"
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
               <form action=@{AltAdminR}  method=post>
                <div class="card blue-grey darken-1">
                 <div class="card-content white-text">
                  <span class="card-title">ADMIN</span>
                  <br>
                  <p> Troca de Senha
                  <br>
                  <div class="input-field">
                   <input type="hidden" id="aid" name="aid" value=#{fromSqlKey aid}>
                   <input id="senha" name="senha" type="text" class="validate">
                   <label class="white-text" for="senha">Senha</label>
                 <br>
                 <div class="card-action">
                   <button class="btn waves-effect waves-light" type="submit" name="action">Editar
                     <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")



postAltAdminR :: Handler Html
postAltAdminR = do
   maybeId <- lookupSession "ID"
   idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
   aid <- runInputPost $ ireq hiddenField "aid"
   senha <- runInputPost $ ireq textField "senha"
   admin <- runDB $ selectList [AdminId ==. aid] []
   runDB $ update aid [AdminPass =. senha]
   defaultLayout $ do
           setTitle "ⓅⒶⒸ - Admin"
           addStylesheet $ (StaticR css_materialize_css)
           addScript $ (StaticR js_jquery_js)
           addScript $ (StaticR js_materialize_js)
           toWidget $(juliusFile "templates/admin.julius")
           toWidget $(luciusFile "templates/admin.lucius")
           $(whamletFile "templates/header.hamlet")
           [whamlet|
            <main>
               Senha Atualizada com Sucesso
           |]
           $(whamletFile "templates/footer.hamlet")



postADMPerfilR :: AdminId -> Handler Html
postADMPerfilR aid = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    runDB $ delete aid
    redirect ListaAdminR

getListaAdminR :: Handler Html
getListaAdminR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    admins <- runDB $ selectList [] [Asc AdminLogin]
    qtadmins <- runDB $ count ([] :: [Filter Admin])
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Admin"
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
                                    Admins
                                <th>
                        <tbody>
                            $forall (Entity aid admin) <- admins
                                <tr>
                                 <li class="divider"></li>
                                    <td>
                                        <a href=@{ADMPerfilR aid}>
                                            #{adminLogin admin}
                                    <td>
                                      $if (qtadmins >= 2)
                                        <form action=@{ADMPerfilR aid} method=post>
                                            <input class="btn waves-effect waves-light" type="submit" value="Apagar">
                                    <li class="divider"></li>
        |]
        $(whamletFile "templates/footer.hamlet")
