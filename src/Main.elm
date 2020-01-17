module Main exposing (main)

import Browser
import Model exposing (Msg(..))
import Update
import View


main : Program Model.Flags Model.Model Model.Msg
main =
    Browser.application
        { view = View.view
        , init = Update.init
        , update = Update.update
        , subscriptions = Update.subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
