{-# language OverloadedStrings #-}

module Bulletin where

import qualified Web.Scotty as S
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as C
import qualified Data.Map as M
import qualified Network.HTTP.Types as HTTP

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

------------------------
-- Runner and Routing --
------------------------

main :: IO ()
main = do
  dummyPosts <- makeDummyPosts
  S.scotty 3000 (myApp dummyPosts)

myApp :: Posts -> S.ScottyM ()
myApp posts = do
  -- Our main page, which will display all of the bulletins
  S.get "/" $
    S.text $ TL.unlines $ map ppPost $ M.elems posts

  -- A page for a specific post
  S.get "/post/:id" $ do
    pid <- S.param "id"
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
  S.post "/post/:id/delete" $
    error "not yet implemented"


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
