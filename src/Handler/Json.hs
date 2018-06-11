{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Json where

import Import
import Prelude
import Database.Persist.Postgresql
import Network.HTTP.Types.Status
import Capsula



getJsonTrocaCartaoPorNomeR :: Text -> Handler Value
getJsonTrocaCartaoPorNomeR cartaorecebido = do
    --caraLista <- runDB $ selectList [PessoaCartao ==. (readMaybe $ show $ cartaorecebido)] [Asc PessoaNome]
    caraNome <- runDB $ getBy $ UniqueCartao cartaorecebido
    pegacaraNome <- case caraNome of
                    Just (Entity _ resto) -> do return $ Just (pessoaNome resto)
                    _ -> do return Nothing

    sendStatusJSON ok200 (object["nomedocara" .= pegacaraNome])
--sendStatusJSON noContent204 (object[])
