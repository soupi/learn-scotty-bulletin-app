{-# language OverloadedStrings #-}

module Bulletin where

import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Web.Twain as Twain
import qualified Data.Text as T
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
  putStrLn "Starting server at port 3000"
  Warp.run 3000 (myApp mystateVar)

myApp :: STM.TVar MyState -> Twain.Application
myApp mystateVar = foldr ($)
  (Twain.notFound $ Twain.send $ Twain.text "404 Not found.")

  -- Our main page, which will display all of the bulletins
  [ Twain.get "/" $ do
    posts <- liftIO $ msPosts <$> STM.readTVarIO mystateVar
    Twain.send $
      Twain.html $ H.renderBS $
        template
          "Bulletin board - posts"
          (postsHtml posts)

  -- A page for a specific post
  , Twain.get "/post/:id" $ do
    pid <- Twain.param "id"
    posts <- liftIO $ msPosts <$> STM.readTVarIO mystateVar
    case M.lookup pid posts of
      Just post ->
        Twain.send $
          Twain.html $ H.renderBS $
            template
              ("Bulletin board - post " <> TL.pack (show pid))
              (postHtml pid post)

      Nothing -> do
        Twain.send $
          Twain.raw HTTP.status404 [("Content-Type", "text/html; charset=utf-8")] $
            H.renderBS $
              template
                ("Bulletin board - post " <> TL.pack (show pid) <> " not found.")
                "404 Post not found."

  -- A page for creating a new post
  , Twain.get "/new" $
    Twain.send $
      Twain.html $ H.renderBS $
        template
          ("Bulletin board - add new post")
          newPostHtml

  -- A request to submit a new page
  , Twain.post "/new" $ do
    title <- Twain.param "title"
    author <- Twain.param "author"
    content <- Twain.param "content"
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
    Twain.send $ Twain.redirect302 ("/post/" <> T.pack (show pid))

  -- A request to delete a specific post
  , Twain.post "/post/:id/delete" $ do
    pid <- Twain.param "id"
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
        Twain.send $ Twain.redirect302 "/"

      else do
        Twain.send $
          Twain.raw HTTP.status404 [("Content-Type", "text/html; charset=utf-8")] $
            "404 Not Found."

  -- css styling
  , Twain.get "/style.css" $
    Twain.send $
      Twain.css ".main { width: 900px; margin: auto; }"
  ]


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
