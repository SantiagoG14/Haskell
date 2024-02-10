{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Lucid
  ( Attribute,
    Html,
    ToHtml (toHtml),
    action_,
    body_,
    button_,
    class_,
    crossorigin_,
    div_,
    form_,
    h1_,
    html_,
    input_,
    integrity_,
    method_,
    name_,
    p_,
    renderText,
    script_,
    src_,
    type_,
  )
import Lucid.Base (makeAttribute)
import Lucid.Html5 (head_)
import Web.Scotty (get, html, param, post, redirect, scotty)

mdiv :: Text -> Html ()
mdiv t = div_ $ h1_ (toHtml t)

main :: IO ()
main = do
  urlR <- newIORef (1 :: Int, mempty :: Map Int Text)
  server urlR

hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "hx-post"

hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "hx-target"

hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"

server :: IORef (Int, Map Int Text) -> IO ()
server urlR = do
  scotty 3000 $ do
    get "/:word" $ do
      word <- param "word"
      html . renderText $ mdiv word

    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlR
      html . renderText $
        html_ $ do
          head_ $ do
            script_ [src_ "https://unpkg.com/htmx.org@1.9.10", integrity_ "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC", crossorigin_ "annonymous"] ("" :: Html ())
          body_ $ do
            h1_ "Welcome to this page"
            form_ [method_ "post", action_ "/"] $ do
              input_ [type_ "text", name_ "url"]
              input_ [type_ "submit"]
              button_ [hxPost_ "/clicked"] "click me"
            div_ [class_ "flex"] $ do
              mapM_
                ( \(i, url) -> div_ [] $ do
                    p_ (toHtml (show i))
                    p_ (toHtml url)
                )
                (M.toList urls)

    post "/" $ do
      url <- param "url"
      liftIO $
        modifyIORef urlR $
          \(i, urls) -> (i + 1, M.insert i url urls)
      redirect "/"
