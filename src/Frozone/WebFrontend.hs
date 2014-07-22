{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Frozone.WebFrontend where

import Data.Monoid
import Text.Blaze.Html5
import Web.Spock (blaze)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

basicPage pageContent =
    H.html $
    do H.head pageHead
       H.body pageBody
    where
      pageHead =
          do H.meta ! A.charset "utf-8"
             H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
             H.title "Frozone"
             H.link ! A.href "/css/site.css" ! A.rel "stylesheet"
             H.link ! A.href "/css/bootstrap.min.css" ! A.rel "stylesheet"
             H.link ! A.href "/css/bootstrap-theme.min.css" ! A.rel "stylesheet"
             H.link ! A.href "/css/font-awesome.min.css" ! A.rel "stylesheet"
             H.link ! A.href "/css/codemirror.css" ! A.rel "stylesheet"
             H.link ! A.href "/css/mergely.css" ! A.rel "stylesheet"
      pageBody =
          do H.div ! A.class_ "container" $
              do H.div ! A.class_ "header" $ H.a ! A.href "/#/home" $ H.h3 ! A.class_ "text-muted" $ "Frozone"
                 H.div ! A.id "page-content" ! A.style "margin-bottom: 20px;" $ pageContent
                 H.div ! A.class_ "footer" $
                  H.p "Powered by Frozone"
             H.script ! A.src "/js/lib/jquery.min.js" $ mempty
             H.script ! A.src "/js/lib/bootstrap.min.js" $ mempty
             H.script ! A.src "/js/lib/react-with-addons.min.js" $ mempty
             H.script ! A.src "/js/lib/signals.min.js" $ mempty
             H.script ! A.src "/js/lib/hasher.min.js" $ mempty
             H.script ! A.src "/js/lib/crossroads.min.js" $ mempty
             H.script ! A.src "/js/lib/codemirror.min.js" $ mempty
             H.script ! A.src "/js/lib/mergely.min.js" $ mempty
             H.script ! A.src "/js/out/frozone.js" $ mempty

indexPage =
    blaze $ basicPage $ H.div ! A.style "text-align:center;" $
    H.img ! A.src "/img/loading-bar.gif"
