module YandexGeocoder 
        (
          getGeoInfo

        , APIKey

        , GeoInfo
        , giRequest
        , giSuggested
        , giObjects

        , GeoObject
        , goName
        , goPos

        , Position(..)
        ) where


import Control.Monad (mapM)

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody, urlEncodeVars)
import Text.JSON

import Text.ParserCombinators.Parsec (Parser, parse, space)
import Text.Parsec.Numbers (parseExtFloat)


type APIKey = String

data GeoInfo = GeoInfo
    { giRequest   :: String
    , giSuggested :: String
    , giObjects   :: [GeoObject]
    } deriving (Eq, Show)

data Position = Position
    { latitude  :: Double
    , longitude :: Double
    } deriving (Eq, Show)

data GeoObject = GeoObject
    { goName :: String
    , goPos  :: Position 
    } deriving (Eq, Show)


toResult :: Either String a -> Result a
toResult (Left s)  = Error s
toResult (Right a) = Ok a


posParser :: Parser Position
posParser = do
    lat <- parseExtFloat
    space
    long <- parseExtFloat
    return $ Position lat long


readPos :: String -> Result Position
readPos s = case parse posParser "" s of
              (Left s)  -> Error $ show s
              (Right p) -> Ok p


buildObjectFrom :: JSObject JSValue -> Result GeoObject
buildObjectFrom obj = do
    object  <- valFromObj "GeoObject" obj
    name    <- valFromObj "name"      object

    point   <- valFromObj "Point"     object
    pos     <- valFromObj "pos"       point >>= readPos

    return $ GeoObject { goName = name, goPos = pos }
 

buildObjectsFrom :: JSValue -> Result [GeoObject]
buildObjectsFrom (JSArray jss) = mapM buildObjectFrom' jss
    where buildObjectFrom' (JSObject obj) = buildObjectFrom obj
          buildObjectFrom' _              = Error "Not a JSON object"
buildObjectsFrom _ = Error "Object is not a JSON array"


buildInfoFrom :: String -> Result GeoInfo
buildInfoFrom jsondata = do
    json         <- decode jsondata

    response     <- valFromObj "response"                 json
    header       <- valFromObj "GeoObjectCollection"      response
    metadata     <- valFromObj "metaDataProperty"         header
    respMetadata <- valFromObj "GeocoderResponseMetaData" metadata

    request      <- valFromObj "request"                  respMetadata

    results      <- valFromObj "featureMember"            header
    objects      <- buildObjectsFrom results

    return $ GeoInfo { giRequest   = request
                     , giSuggested = ""
                     , giObjects   = objects }
     

getGeoInfo :: APIKey -> String -> IO (Either String GeoInfo) 
getGeoInfo apikey location = do
    response <- simpleHTTP $ getRequest $ mkYndRq
    jsondata <- getResponseBody response
    return $ resultToEither $ buildInfoFrom $ decodeString jsondata
 
  where mkYndRq  = "http://geocode-maps.yandex.ru/1.x/?" ++ rqParams

        rqParams = urlEncodeVars [ ("geocode", encodeString location)
                                 , ("key"    , apikey)
                                 , ("format" , "json")
                                 ]
