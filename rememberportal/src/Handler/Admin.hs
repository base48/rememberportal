{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
    (newLevelWidget, newLevelEnctype) <- generateFormPost $ levelForm Nothing
    getAdminHelper newLevelWidget newLevelEnctype

postLevelNewR :: Handler Html
postLevelNewR = do
    ((result, widget), enctype) <- runFormPost $ levelForm Nothing
    case result of
        FormMissing -> do
            addMessage "danger" $ toHtml ("Form missing" :: Text)
            redirect $ AdminR
        FormFailure fs -> do
            forM_ fs (addMessage "danger" . toHtml)
            getAdminHelper widget enctype
        FormSuccess nl -> do
            _ <- runDB $ insert nl
            addMessage "success" "Membership level created"
            redirect $ AdminR

getLevelEditR :: LevelId -> Handler Html
getLevelEditR lid = do
    levelData <- runDB $ get404 lid
    (widget, enctype) <- generateFormPost $ levelForm $ Just levelData
    getLevelHelper lid widget enctype

postLevelEditR :: LevelId -> Handler Html
postLevelEditR lid = do
    ((result, widget), enctype) <- runFormPost $ levelForm Nothing -- FIXME Just levelData
    case result of
        FormMissing -> do
            addMessage "danger" $ toHtml ("Form missing" :: Text)
            redirect $ LevelEditR lid
        FormFailure fs -> do
            forM_ fs (addMessage "danger" . toHtml)
            getLevelHelper lid widget enctype
        FormSuccess nl -> do
            -- let withKey = nu { userVerkey = Just verkey, userEmail = email }
            runDB $ replace lid nl
            addMessage "success" "Membership level updated"
            redirect $ AdminR

levelForm :: Maybe Level -> Form Level
levelForm ml = renderBootstrap2 $ Level
    <$> areq identField "Name" (levelName <$> ml) -- FIXME nonempty
    <*> (realToFrac <$> (areq amountField "Amount" (fromRational <$> levelAmount <$> ml))) -- FIXME rounding errors 321.4
    <*> areq checkBoxField "Active" (Just $ defaultTrue $ levelActive <$> ml)
    <*> areq checkBoxField "User-selectable" (levelUserSelectable <$> ml)
  where
    identField = strField 1 30 (const True) "Name"
    amountField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Double
    amountField = check validateAmount doubleField
    validateAmount :: Double -> Either Text Double
    validateAmount x
      | x < 0     = Left "Must not be negative"
      | otherwise = Right x
    defaultTrue x = fromMaybe True x

getAdminHelper :: Widget -> Enctype -> Handler Html
getAdminHelper newLevelWidget newLevelEnctype = do
    --levels <- runDB $ selectList [] [Asc LevelName]
    levels <- runDB $ do
        lvls <- selectList [] [Asc LevelName]
        forM lvls (\(Entity lid l) -> do
            c <- count [UserLevel ==. Just lid]
            return (lid, l, c))
    currency <- getYesod >>= return . appCurrency . appSettings
    defaultLayout $ do
        setTitle . toHtml $ ("Administration" :: Text)
        $(widgetFile "admin")

getLevelHelper :: LevelId -> Widget -> Enctype -> Handler Html
getLevelHelper lid widget enctype = do
    defaultLayout $ do
        setTitle . toHtml $ ("Edit membership level" :: Text)
        $(widgetFile "level-edit")
