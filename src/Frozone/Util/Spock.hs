module Frozone.Util.Spock where

import Web.Spock
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

blaze :: Html -> SpockAction conn sess st ()
blaze htmlVal =
    html $ renderHtml htmlVal
