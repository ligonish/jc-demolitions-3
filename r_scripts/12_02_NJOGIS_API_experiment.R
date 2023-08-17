# Friday 12/2/2022
# https://newjersey.maps.arcgis.com/home/item.html?id=dba4fc758a8f425f8fa83b8634b1b95c
# https://njogis-newjersey.opendata.arcgis.com/datasets/newjersey::addresspoints/explore?location=40.716309%2C-74.041331%2C18.98
# https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer
# https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates
# https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/geocodeAddresses
# https://developers.arcgis.com/rest/services-reference/enterprise/get-started-with-the-services-directory.htm

# Let's try accessing NJGIS geocoding service, which requires accessing ArcGIS REST.
# Following instructions in this esri blog post from 2019: https://community.esri.com/t5/gis-blog/accessing-arcgis-rest-services-using-r/ba-p/898451

library(tidyverse)
library(httr)
library(sf)
library(janitor)

# His version -----------------------------

url <- parse_url("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services")
url$path <- paste(url$path, "USA_Railroads_1/FeatureServer/0/query", sep = "/")
url$query <- list(where = "STATE = 'FL'",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)

Florida_Railroads <- st_read(request)

# My version ------------------------------------------

# Fields (using us as example):

# STATE = 'NJ'
# COUNTY = '882278'
# INC_MUNI = '885264'
# ADD_NUMBER = '168'
# ST_NAME = 'Grand'
# ST_POSTYP = 'Street'
# PRIMENAME = 'Grand Street'
# FULLADDR = '168 Grand Street'
# MSAGCOMM = 'JERSEY CITY'
# POST_COMM = 'Jersey City'
# POST_CODE = '07302'
# POST_CODE4 = '4433'
# LONG_ = '-74.041'
# LAT = '40.716'

# using ArcGIS REST Tasks/NJ_Geocode via https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer
# try this query: https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates?Address=168+Grand&Address2=&Address3=&Neighborhood=&City=Jersey+City&Subregion=&Region=&Postal=&PostalExt=&CountryCode=&SingleLine=&outFields=&maxLocations=&matchOutOfRange=true&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&outSR=&magicKey=&f=html

url <- parse_url("https://geo.nj.gov")
url$path <- paste(url$path, "arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates", sep = "/")
url$query <- list(Address = "168+Grand",
                  Address2 = "",
                  Address3 = "",
                  Neighborhood = "",
                  City = "Jersey+City",
                  Subregion = "",
                  Region = "",
                  Postal = "",
                  PostalExt = "",
                  CountryCode = "",
                  SingleLine = "",
                  outFields = "",
                  maxLocations = "",
                  matchOutOfRange = "",
                  langCode = "",
                  locationType = "",
                  sourceCountry = "",
                  category = "",
                  location = "",
                  distance = "",
                  searchExtent = "",
                  outSR = "",
                  magicKey = "",
                  f = "pjson")
request <- build_url(url)

test_168 <- st_read(request)

# here's what the site uses
# https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates?Address=168+Grand&Address2=&Address3=&Neighborhood=&City=Jersey+City&Subregion=&Region=&Postal=&PostalExt=&CountryCode=&SingleLine=&outFields=&maxLocations=&matchOutOfRange=true&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&outSR=&magicKey=&f=pjson
# https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates?Address=168%20Grand&City=Jersey%20City&outFields=%2A&returnGeometry=true&f=geojson
# here's what I generated

# Trying to figure it out in reverse:
parse_url("https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates?Address=168+Grand&Address2=&Address3=&Neighborhood=&City=Jersey+City&Subregion=&Region=&Postal=&PostalExt=&CountryCode=&SingleLine=&outFields=&maxLocations=&matchOutOfRange=true&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&outSR=&magicKey=&f=pjson")

# Hmm. So the above build_url, stored as "request", works in like a Chrome bar but not here.

test_parse <- GET("https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates?Address=168+Grand&Address2=&Address3=&Neighborhood=&City=Jersey+City&Subregion=&Region=&Postal=&PostalExt=&CountryCode=&SingleLine=&outFields=&maxLocations=&matchOutOfRange=true&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&outSR=&magicKey=&f=html")

# Maybe try whatever's going on here instead?

library(geojsonsf)
library(urltools)  # https://cran.r-project.org/web/packages/urltools/vignettes/urltools.html

url_encode("https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates?Address=168+Grand&Address2=&Address3=&Neighborhood=&City=Jersey+City&Subregion=&Region=&Postal=&PostalExt=&CountryCode=&SingleLine=&outFields=&maxLocations=&matchOutOfRange=true&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&outSR=&magicKey=&f=pjson")

request_2 <- "https://geo.nj.gov/arcgis/rest/services/Tasks/NJ_Geocode/GeocodeServer/findAddressCandidates?Address=168+Grand&Address2=&Address3=&Neighborhood=&City=Jersey+City&Subregion=&Region=&Postal=&PostalExt=&CountryCode=&SingleLine=&outFields=&maxLocations=&matchOutOfRange=true&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&outSR=&magicKey=&f=geojson"

test_168 <- geojson_sf(request_2)

house <- geojson_sf()

# ughhhhhhhhh I don't think this is worth the amt of time I'm dumping here to learn it;
# let's just move to trying the set I downloaded.
# ... nope, that's a proprietary ESRI .gdb file.
# where the fuck is a simple tabular version of this data?
# ok, trying this even though it said not to: https://njogis-newjersey.opendata.arcgis.com/datasets/newjersey::address-points-for-nj-hosted-3424/about?layer=0
# also this seems much more promising? https://njogis-newjersey.opendata.arcgis.com/datasets/newjersey::address-points-for-nj-hosted-3424/api?layer=0
# via https://njogis-newjersey.opendata.arcgis.com/datasets/newjersey::address-points-for-nj-hosted-3424/about?layer=0

# addresses & lat/long for all JC parcels via API, I think:
# https://services2.arcgis.com/XVOqAjTOJ5P6ngMu/arcgis/rest/services/AddressPoints/FeatureServer/0/query?where=INC_MUNI%20%3D%20'885264'&outFields=ADD_NUMBER,ST_NAME,ST_POSTYP,PRIMENAME,FULLADDR,LST_NAME,LST_PNAME,MSAGCOMM,POST_COMM,POST_CODE,PLACE_TYPE,LONG_,LAT&outSR=4326&f=json

# and for my house specifically, as a test:
# https://services2.arcgis.com/XVOqAjTOJ5P6ngMu/arcgis/rest/services/AddressPoints/FeatureServer/0/query?where=INC_MUNI%20%3D%20'885264'%20AND%20FULLADDR%20%3D%20'168%20GRAND%20STREET'&outFields=EFFECTIVE,EXPIRE,SITE_NGUID,STATE,COUNTY,ADD_NUMBER,ST_NAME,ST_POSTYP,PRIMENAME,FULLADDR,LST_NAME,LST_PNAME,MSAGCOMM,POST_COMM,POST_CODE,PLACE_TYPE,LONG_,LAT,INC_MUNI,DATEUPDATE&outSR=4326&f=json
test_168 <- st_read("https://services2.arcgis.com/XVOqAjTOJ5P6ngMu/arcgis/rest/services/AddressPoints/FeatureServer/0/query?where=INC_MUNI%20%3D%20'885264'%20AND%20FULLADDR%20%3D%20'168%20GRAND%20STREET'&outFields=EFFECTIVE,EXPIRE,SITE_NGUID,STATE,COUNTY,ADD_NUMBER,ST_NAME,ST_POSTYP,PRIMENAME,FULLADDR,LST_NAME,LST_PNAME,MSAGCOMM,POST_COMM,POST_CODE,PLACE_TYPE,LONG_,LAT,INC_MUNI,DATEUPDATE&outSR=4326&f=json")
# ahhhh this worked! ok will there be too many parcels over the query limit if I do all of JC?

test_jc <- st_read("https://services2.arcgis.com/XVOqAjTOJ5P6ngMu/arcgis/rest/services/AddressPoints/FeatureServer/0/query?where=INC_MUNI%20%3D%20'885264'&outFields=ADD_NUMBER,ST_NAME,ST_POSTYP,PRIMENAME,FULLADDR,LST_NAME,LST_PNAME,MSAGCOMM,POST_COMM,POST_CODE,PLACE_TYPE,LONG_,LAT&outSR=4326&f=json")

test_jc <- clean_names(test_jc)

write_csv(test_jc, "data_raw/jc_parcel_addresses_coords_12_02_2022.csv")
# see metadata for these here: https://services2.arcgis.com/XVOqAjTOJ5P6ngMu/arcgis/rest/services/AddressPoints/FeatureServer/0
# Spatial Reference: 102711 (3424)
# aka EPSG:3424
