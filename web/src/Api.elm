module Api exposing (loadActivityInfo, loadActivityTree)

import Http
import Models.Activity exposing (ActivityInfo, ActivityTree, activityInfoDecoder, activityTreeDecoder)


loadActivityInfo : (Result Http.Error ActivityInfo -> msg) -> String -> String -> Cmd msg
loadActivityInfo toMsg dbName activityId =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId
        , expect = Http.expectJson toMsg activityInfoDecoder
        }


loadActivityTree : (Result Http.Error ActivityTree -> msg) -> String -> String -> Cmd msg
loadActivityTree toMsg dbName activityId =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId ++ "/tree"
        , expect = Http.expectJson toMsg activityTreeDecoder
        }
