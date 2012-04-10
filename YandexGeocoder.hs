module YandexGeocoder 
        (
          getGeoInfo

        , APIKey
        , GeoInfo(..)
        , GeoObject(..)
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


data Bounds = Bounds
    { lowerCorner :: Position
    , upperCorner :: Position
    } deriving (Eq, Show)


data GeoObject = GeoObject
    { goName        :: String
    , goPos         :: Position

    , goAddressLine :: String
    , goCountry     :: String
    , goCountryCode :: String

    , goBounds      :: Bounds
    } deriving (Eq, Show)


toResult :: Either String a -> Result a
toResult (Left s)  = Error s
toResult (Right a) = Ok a


(@@) :: JSON a => String -> JSObject JSValue -> Result a
s @@ o = valFromObj s o


(@#) :: JSON a => Result a -> a -> Result a
value @# defvalue = Ok $ case value of
    (Ok a)    -> a
    (Error _) -> defvalue


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
    object  <- "GeoObject"  @@ obj
    name    <- "name"       @@ object
    pos     <- readPos =<< (@@) "pos"  =<< "Point"            @@ object

    bounds  <- (@@) "Envelope"         =<< "boundedBy"        @@ object
    meta    <- (@@) "GeocoderMetaData" =<< "metaDataProperty" @@ object
    addr    <- (@@) "Country"          =<< "AddressDetails"   @@ meta
    
    address <- "text"            @@ meta
    country <- "CountryName"     @@ addr
    ccode   <- "CountryNameCode" @@ addr

    lcorn   <- readPos =<< "lowerCorner" @@ bounds
    ucorn   <- readPos =<< "upperCorner" @@ bounds
    

    return $ GeoObject { goName        = name
                       , goPos         = pos 

                       , goAddressLine = address
                       , goCountry     = country
                       , goCountryCode = ccode

                       , goBounds      = Bounds lcorn ucorn
                       }
 

buildObjectsFrom :: JSValue -> Result [GeoObject]
buildObjectsFrom (JSArray jss) = mapM buildObjectFrom' jss
    where buildObjectFrom' (JSObject obj) = buildObjectFrom obj
          buildObjectFrom' _              = Error "Not a JSON object"
buildObjectsFrom _ = Error "Object is not a JSON array"


buildInfoFrom :: String -> Result GeoInfo
buildInfoFrom json = do
    header  <- (@@) "GeoObjectCollection" =<< (@@) "response" =<< decode json
    meta    <- (@@) "GeocoderResponseMetaData" =<< "metaDataProperty" @@ header    
    
    request <- "request"       @@ meta
    suggest <- "suggest"       @@ meta   @# ""
    results <- "featureMember" @@ header
    objects <- buildObjectsFrom results

    return $ GeoInfo { giRequest   = request
                     , giSuggested = suggest
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
