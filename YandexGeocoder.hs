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
import Control.Exception (SomeException, handle, evaluate)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody, urlEncodeVars)
import Text.JSON

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


readPos :: String -> Position
readPos s = Position { latitude = lat, longitude = long }
    where (lat:long:_) = map read $ words s 


buildObjectFrom :: JSObject JSValue -> Result GeoObject
buildObjectFrom obj = do
   object <- valFromObj "GeoObject" obj
   name   <- valFromObj "name"      object

   point  <- valFromObj "Point"     object
   pos    <- valFromObj "pos"       point 
   
   return $ GeoObject { goName = name
                      , goPos  = readPos $ fromJSString pos }
 

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

    result <- handle handler $ evaluate $ buildInfoFrom jsondata
    return $ fromResult $ result
 
  where mkYndRq  = "http://geocode-maps.yandex.ru/1.x/?" ++ rqParams 
        rqParams = urlEncodeVars [ ("geocode", location)
                                 , ("key"    , apikey  )
                                 , ("format" , "json"  )
                                 ]

        fromResult (Error e) = Left e
        fromResult (Ok a)    = Right a

        handler :: SomeException -> IO (Result a)
        handler _ = return $ Error "exception occured during parsing" 
