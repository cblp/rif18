{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Suspicious where

import           Import

import           Database.Persist.Sql (rawSql)

getSuspiciousR :: Handler Html
getSuspiciousR = do
    users <- map entityVal <$> runDB selectSuspiciousUsers
    defaultLayout $ do
        setTitle "Suspicious users"
        $(widgetFile "suspicious")

selectSuspiciousUsers :: MonadIO m => SqlPersistT m [Entity User]
selectSuspiciousUsers = rawSql
    "select * from user where exists (                      \
    \   select message from comment                         \
    \   where user_id = user.id and message like \"%<%\"    \
    \);"
    []
