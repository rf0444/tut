{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root where

import Import
import qualified Data.Text as T
import Control.Arrow

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle "tut homepage"
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
    $(widgetFile "homepage")

data UserForm = UserForm {
  userFormSex :: Sex
, userFormAge :: Int
, userFormMemo :: Textarea
} deriving Show

userForm :: Maybe User -> Form UserForm
userForm u = renderDivs $ UserForm
  <$> areq sexField "sex" (userSex <$> u)
  <*> areq intField "age" (userAge <$> u)
  <*> areq textareaField "memo" (textarea . userMemo <$> u)
 where
  sexField = selectField sexSelect
  sexSelect :: [(Text, Sex)]
  sexSelect = map (T.pack . show &&& id) sexs
  textarea text = Textarea { unTextarea = text }

sexs :: [Sex]
sexs = [minBound..maxBound]

getHomeR :: UserId -> Handler RepHtml
getHomeR uid = do
  (self, friends) <- runDB $ do
    me <- get404 uid
    fs <- selectList [UserId !=. uid] [Asc UserIdent]
    return (me, fs)
  --let sexIs s = s == userSex self
  -- TODO: form デザイン調整
  ((_, form), _) <- generateFormPost $ userForm $ Just self
  defaultLayout $ do
    setTitle "user home"
    $(widgetFile "userhome")

postHomeR :: UserId -> Handler RepHtml
postHomeR uid = do
  ((result, _), _) <- runFormPost $ userForm Nothing
  case result of
    FormSuccess form -> do
      _ <- runDB $ do
	    update uid [UserSex =. userFormSex form, UserAge =. userFormAge form, UserMemo =. (unTextarea . userFormMemo) form]
      getHomeR uid
    -- TODO: えー・・・
    FormMissing -> do
      _ <- runDB $ do
	    update uid [UserMemo =. "missing"]
      getHomeR uid
    FormFailure texts -> do
      _ <- runDB $ do
	    update uid [UserMemo =. T.unlines texts]
      getHomeR uid

