module Main exposing (main)

import Browser
import Model
import Update
import View


main : Program Model.Flags Model.Model Update.Msg
main =
    Browser.element
        { view = View.view
        , init = Update.init
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
