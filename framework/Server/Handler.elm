module Server.Handler exposing (..)

{-| Apps use this package to handle HTTP requests.
-}

import Native.Handler
import Server.HTTP exposing (Body, Headers, Method, Request, StatusCode)


handleRequest : (Request -> msg) -> Sub msg
handleRequest =
    Native.Handler.handleRequest


writeResponse : StatusCode -> Body -> Headers -> Request -> Cmd msg
writeResponse =
    Native.Handler.writeResponse


writeResponseStartBackgroundJob : StatusCode -> Body -> Headers -> Request -> Cmd msg
writeResponseStartBackgroundJob =
    Native.Handler.writeResponseStartBackgroundJob


endBackgroundJob : Request -> Cmd msg
endBackgroundJob =
    Native.Handler.endBackgroundJob
