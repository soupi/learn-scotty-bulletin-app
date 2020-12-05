{-# language OverloadedStrings #-}

module Bulletin where

import qualified Web.Scotty as S
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as C
import qualified Data.Map as M
import qualified Network.HTTP.Types as HTTP
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)

-----------
-- Types --
-----------

data Post
  = Post
    { pTime :: C.UTCTime
    , pAuthor :: TL.Text
    , pTitle :: TL.Text
    , pContent :: TL.Text
    }

type Posts = M.Map Integer Post

data MyState
  = MyState
    { msId :: Integer
    , msPosts :: Posts
    }

------------------------
-- Runner and Routing --
------------------------

main :: IO ()
main = do
  posts <- makeDummyPosts
  mystateVar <- STM.newTVarIO MyState{msId = 1, msPosts = posts}
  S.scotty 3000 (myApp mystateVar)

myApp :: STM.TVar MyState -> S.ScottyM ()
myApp mystateVar = do
  -- Our main page, which will display all of the bulletins
  S.get "/" $ do
    posts <- liftIO $ msPosts <$> STM.readTVarIO mystateVar
    S.text $ TL.unlines $ map ppPost $ M.elems posts

  -- A page for a specific post
  S.get "/post/:id" $ do
    pid <- S.param "id"
    posts <- liftIO $ msPosts <$> STM.readTVarIO mystateVar
    case M.lookup pid posts of
      Just post ->
        S.text $ ppPost post

      Nothing -> do
        S.status HTTP.notFound404
        S.text "404 Not Found."

  -- A page for creating a new post
  S.get "/new" $
    error "not yet implemented"

  -- A request to submit a new page
  S.post "/new" $
    error "not yet implemented"

  -- A request to delete a specific post
  S.post "/post/:id/delete" $ do
    pid <- S.param "id"
    exists <- liftIO $ STM.atomically $ do
      mystate <- STM.readTVar mystateVar
      case M.lookup pid (msPosts mystate) of
        Just{} -> do
          STM.writeTVar
            mystateVar
            ( mystate
              { msPosts = M.delete pid (msPosts mystate)
              }
            )
          pure True

        Nothing ->
          pure False
    if exists
      then
        S.redirect "/"

      else do
        S.status HTTP.notFound404
        S.text "404 Not Found."



newPost :: Post -> STM.TVar MyState -> IO ()
newPost post mystateVar = do
  STM.atomically $ do
    mystate <- STM.readTVar mystateVar
    STM.writeTVar
      mystateVar
      ( mystate
        { msId = msId mystate + 1
        , msPosts = M.insert (msId mystate) post (msPosts mystate)
        }
      )

-----------
-- Utils --
-----------

makeDummyPosts :: IO Posts
makeDummyPosts = do
  time <- C.getCurrentTime
  pure $
    M.singleton
      0
      ( Post
        { pTime = time
        , pTitle = "Dummy title"
        , pAuthor = "Dummy author"
        , pContent = "bla bla bla..."
        }
      )

ppPost :: Post -> TL.Text
ppPost post =
  let
    header =
      TL.unwords
        [ "[" <> TL.pack (show (pTime post)) <> "]"
        , pTitle post
        , "by"
        , pAuthor post
        ]
    seperator =
      TL.replicate (TL.length header) "-"
  in
    TL.unlines
      [ seperator
      , header
      , seperator
      , pContent post
      , seperator
      ]
