{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Radar where

import Import
import Prelude
import Database.Persist.Postgresql
import Network.HTTP.Types.Status
import Text.Cassius
import Text.Lucius
import Text.Julius
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Capsula



getRadarR :: Handler Html
getRadarR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ ->
                    return ""
    areaUm <- runDB $ selectList [AreaOrdem ==. 1] [Asc AreaOrdem]
    pegaareaUm <- case (safeHead(areaUm)) of
                    Just (Entity _ resto) -> do redirect (RadarIndiceR 1 ("false"))
                    _ -> return Nothing
    --ack <- return $ show "a"
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Radar"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/home.julius")
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
          <main>
            <h1>
              Radar não conseguiu encontrar uma área para mostrar
        |]
        $(whamletFile "templates/footer.hamlet")

getRadarIndiceR :: Int -> Text -> Handler Html
getRadarIndiceR ordemcampo automatico = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ ->
                    return ""
    --funcoes pra verificar, redirecionar & atribuir ordemcampo recebido no get
    areaMax <- runDB $ selectList [] [Desc AreaOrdem]
    redirecionaareaMax <- case (safeHead(areaMax)) of
                    Just (Entity _ resto) -> if (((areaOrdem resto) < ordemcampo) || (ordemcampo < 1)) then redirect (RadarIndiceR 1 automatico) else return Nothing
                    _ -> do redirect (RadarIndiceR 1 automatico)

    pegaareaMaxOrdem <- case (safeHead(areaMax)) of
                    Just (Entity _ resto) -> do return $ Just $ areaOrdem resto
                    _ -> do return Nothing

    --pegando 2 campos de Area individualmente
    arealistinha <- runDB $ selectList [AreaOrdem ==. ordemcampo] [Asc AreaOrdem]
    --uso no footer
    pegaareaNome <- case (safeHead(arealistinha)) of
                    Just (Entity _ resto) -> do return $ Just (areaNome resto)
                    _ -> do return Nothing
    --uso no cassius
    pegaareaMapa <- case (safeHead(arealistinha)) of
                    Just (Entity _ resto) -> do return $ Just (areaMapa resto)
                    _ -> do return Nothing

    --usado para pegar o id da area da sala recebida na url
    pegaareaId <- case (safeHead(arealistinha)) of
                    Just (Entity key resto) -> do return $ Just  (key, resto)
                    _ -> do return Nothing

    debug <- do return $ desemcapsula2 $ pegaareaId

    salaLista <- runDB
                $ E.select
                $ E.from $ \(area `E.InnerJoin` sala `E.InnerJoin` arduino) -> do
                    E.on $ arduino ^. ArduinoId E.==. sala ^. SalaArid
                    E.on $ area ^. AreaId E.==. sala ^. SalaArea
                    E.where_ (sala ^. SalaArea E.==. E.val (desemcapsula2 $ pegaareaId))
                    --desemcapsula2 incrivelmente faz o trabalho de toSqlKey
                    return
                        ( sala ^. SalaId
                        , sala ^. SalaArea
                        , sala ^. SalaNome
                        , sala ^. SalaPosx
                        , sala ^. SalaPosy
                        , arduino ^. ArduinoIp
                        )




    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Radar"
        addStylesheet $ (StaticR css_materialize_css)
        --addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/home.julius")
        toWidget $(juliusFile "templates/radar.julius")
        toWidget $(cassiusFile "templates/radar.cassius")
        $(whamletFile "templates/header.hamlet")
        $(whamletFile "templates/radar.hamlet")
        {-
        toWidget $[whamlet|
          Debug:
          <br>
          Id da area atual: #{fromSqlKey $ debug}
          <br>
          $forall (E.Value salaid, E.Value salaarea, E.Value salanome, E.Value salaposicx, E.Value salaposicy, E.Value arip) <- salaLista
              #{fromSqlKey $ salaid} #{fromSqlKey $ salaarea} #{salanome} #{desemcapsula salaposicx} #{desemcapsula salaposicy}
              <br> ip #{arip}
        |]
        -}
        $(whamletFile "templates/footerRadar.hamlet")
