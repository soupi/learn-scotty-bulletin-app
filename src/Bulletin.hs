{-# language OverloadedStrings #-}

module Bulletin where

import qualified Web.Scotty as S

main :: IO ()
main = do
  S.scotty 3000 myApp

myApp :: S.ScottyM ()
myApp = do
  -- Our main page, which will display all of the bulletins
  S.get "/" $
    S.text "not yet implemented"

  -- A page for a specific post
  S.get "/post/:id" $
    error "not yet implemented"

  -- A page for creating a new post
  S.get "/new" $
    error "not yet implemented"

  -- A request to submit a new page
  S.post "/new" $
    error "not yet implemented"

  -- A request to delete a specific post
  S.post "/post/:id/delete" $
    error "not yet implemented"
