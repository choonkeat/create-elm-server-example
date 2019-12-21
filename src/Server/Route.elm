module Server.Route exposing (..)

{- This is its own module because `Url.Parser.*` functions are best used unqualified,
   but short function names clash easily with short variables
-}

import Url.Parser exposing (..)
import Url.Parser.Query as Query


type Endpoint
    = Status (Maybe String)


router : Url.Parser.Parser (Endpoint -> Endpoint) Endpoint
router =
    oneOf
        [ map Status (s "api" </> s "status" <?> Query.string "t")
        ]
