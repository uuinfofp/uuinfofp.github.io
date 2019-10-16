{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module FpExtraWeb where

import Web.Scotty
import Data.Text.Lazy (pack)
import Control.Monad (replicateM_, forM_)
import Lucid
import Control.Concurrent.STM
import Control.Monad.IO.Class

main :: IO ()
-- main = scotty 8000 hallo
-- main = scotty 8000 rasaas2
main = do lst <- newTVarIO []
          scotty 8000 (todo lst)

hallo :: ScottyM ()
hallo = do
  get "/hallo" $ do
    html $ "<h1>Hallo!</h1>"
  get "/hallo/:naam" $ do
    naam <- param "naam"
    html $ "<h1>Hallo, " <> naam <> "!</h1>"

rasaas :: ScottyM ()
rasaas =
  get "/replicate/:n/:s" $ do
    n <- param "n"
    s <- param "s"
    let lst = mconcat (replicate n $ "<li>" <> pack s <> "</li>")
    html $ "<ul>" <> lst <> "</ul>"

rasaas2 :: ScottyM ()
rasaas2 =
  get "/replicate/:n/:s" $ do
    n <- param "n"
    (s :: String) <- param "s"
    html $ renderText $ do
      html_ $ do
        head_ $ title_ "Replicate a string"
        body_ $
          ul_ $ replicateM_ n $
            li_ (toHtml s)

todo :: TVar [String] -> ScottyM ()
todo vr = do
  get "/show" htmlLst
  get "/add/:thing" $ do
    (t :: String) <- param "thing"
    liftIO $ atomically $ modifyTVar vr (t :)
    htmlLst
  where
    htmlLst = do
      lst <- liftIO $ readTVarIO vr
      html $ renderText $ do
        h3_ "Your to-do list"
        ul_ $ forM_ lst (li_ . toHtml)