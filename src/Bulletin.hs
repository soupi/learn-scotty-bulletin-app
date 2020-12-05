{-# language OverloadedStrings #-}

module Bulletin where

import qualified Web.Scotty as S
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as C
import qualified Data.Map as M
import qualified Network.HTTP.Types as HTTP
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Lucid as H

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
    S.html $
      H.renderText $
        template
          "Bulletin board - posts"
          (postsHtml posts)

  -- A page for a specific post
  S.get "/post/:id" $ do
    pid <- S.param "id"
    posts <- liftIO $ msPosts <$> STM.readTVarIO mystateVar
    case M.lookup pid posts of
      Just post ->
        S.html $
          H.renderText $
            template
              ("Bulletin board - post " <> TL.pack (show pid))
              (postHtml pid post)

      Nothing -> do
        S.status HTTP.notFound404
        S.html $
          H.renderText $
            template
              ("Bulletin board - post " <> TL.pack (show pid) <> " not found.")
              "404 Post not found."

  -- A page for creating a new post
  S.get "/new" $
    S.html $
      H.renderText $
        template
          ("Bulletin board - add new post")
          newPostHtml

  -- A request to submit a new page
  S.post "/new" $ do
    title <- S.param "title"
    author <- S.param "author"
    content <- S.param "content"
    time <- liftIO C.getCurrentTime
    pid <- liftIO $ newPost
      ( Post
        { pTime = time
        , pAuthor = author
        , pTitle = title
        , pContent = content
        }
      )
      mystateVar
    S.redirect ("/post/" <> TL.pack (show pid))

  -- A request to submit a new page
  S.post "/new" $ do
    title <- S.param "title"
    author <- S.param "author"
    content <- S.param "content"
    time <- liftIO C.getCurrentTime
    pid <- liftIO $ newPost
      ( Post
        { pTime = time
        , pAuthor = author
        , pTitle = title
        , pContent = content
        }
      )
      mystateVar
    S.redirect ("/post/" <> TL.pack (show pid))

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



newPost :: Post -> STM.TVar MyState -> IO Integer
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
    pure (msId mystate)

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

----------
-- HTML --
----------

type Html = H.Html ()

template :: TL.Text -> Html -> Html
template title content =
  H.doctypehtml_ $ do
    H.head_ $ do
      H.meta_ [ H.charset_ "utf-8" ]
      H.title_ (H.toHtml title)
      H.link_ [ H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/style.css"  ]
    H.body_ $ do
      H.div_ [ H.class_ "main" ] $ do
        H.h1_ [ H.class_ "logo" ] $
          H.a_ [H.href_ "/"] "Bulletin Board"
        content

postsHtml :: Posts -> Html
postsHtml posts = do
  H.p_ [ H.class_ "new-button" ] $
    H.a_ [H.href_ "/new"] "New Post"
  mapM_ (uncurry postHtml) $ reverse $ M.toList posts

postHtml :: Integer -> Post -> Html
postHtml pid post = do
  H.div_ [ H.class_ "post" ] $ do
    H.div_ [ H.class_ "post-header" ] $ do
      H.h2_ [ H.class_ "post-title" ] $
        H.a_
          [H.href_ (TL.toStrict $ "/post/" <> TL.pack (show pid))]
          (H.toHtml $ pTitle post)

      H.span_ $ do
        H.p_ [ H.class_ "post-time" ] $ H.toHtml (TL.pack (show (pTime post)))
        H.p_ [ H.class_ "post-author" ] $ H.toHtml (pAuthor post)

    H.div_ [H.class_ "post-content"] $ do
      H.toHtml (pContent post)

    H.form_
      [ H.method_ "post"
      , H.action_ (TL.toStrict $ "/post/" <> TL.pack (show pid) <> "/delete")
      , H.onsubmit_ "return confirm('Are you sure?')"
      , H.class_ "delete-post"
      ]
      ( do
        H.input_ [H.type_ "submit", H.value_ "Delete", H.class_ "deletebtn"]
      )

newPostHtml :: Html
newPostHtml = do
  H.form_
    [ H.method_ "post"
    , H.action_ "/new"
    , H.class_ "new-post"
    ]
    ( do
      H.p_ $ H.input_ [H.type_ "text", H.name_ "title", H.placeholder_ "Title..."]
      H.p_ $ H.input_ [H.type_ "text", H.name_ "author", H.placeholder_ "Author..."]
      H.p_ $ H.textarea_ [H.name_ "content", H.placeholder_ "Content..."] ""
      H.p_ $ H.input_ [H.type_ "submit", H.value_ "Submit", H.class_ "submit-button"]
    )

