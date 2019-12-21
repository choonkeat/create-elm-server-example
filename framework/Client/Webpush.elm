port module Client.Webpush exposing (..)

{-| <https://developer.mozilla.org/en-US/docs/Web/API/Push_API>
-}

import Json.Decode
import Json.Encode


port onWebpushRegistered : (Json.Encode.Value -> msg) -> Sub msg


port registerWebpush : String -> Cmd msg
