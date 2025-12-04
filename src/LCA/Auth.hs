{-# LANGUAGE OverloadedStrings #-}

module LCA.Auth
    ( basicAuthMiddleware
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64
import Network.Wai
import Network.HTTP.Types.Status (status401)
import Network.HTTP.Types.Header (hWWWAuthenticate, hAuthorization)

-- | WAI middleware for HTTP Basic Authentication
-- If the password matches, the request proceeds
-- If not, returns 401 Unauthorized with WWW-Authenticate header
basicAuthMiddleware :: ByteString -> Middleware
basicAuthMiddleware expectedPassword app req respond =
    case lookup hAuthorization (requestHeaders req) of
        Just authHeader ->
            case parseBasicAuth authHeader of
                Just providedPassword
                    | providedPassword == expectedPassword ->
                        app req respond
                _ ->
                    respond unauthorized
        Nothing ->
            respond unauthorized
  where
    unauthorized =
        responseLBS
            status401
            [(hWWWAuthenticate, "Basic realm=\"fpLCA\"")]
            "Unauthorized"

-- | Parse HTTP Basic Auth header
-- Format: "Basic base64(username:password)"
-- We ignore the username and only check the password
parseBasicAuth :: ByteString -> Maybe ByteString
parseBasicAuth header =
    case BS.stripPrefix "Basic " header of
        Just encoded ->
            case B64.decode encoded of
                Right decoded ->
                    -- Format is "username:password", we take everything after the colon
                    case C8.elemIndex ':' decoded of
                        Just idx -> Just $ BS.drop (idx + 1) decoded
                        Nothing -> Just decoded  -- No colon, treat whole thing as password
                Left _ -> Nothing
        Nothing -> Nothing
