# ACMT
Automatic Context Measurement Tool -- work in progress

To get started, do the following:

* Copy the ACMT Repository Locally
* Request a Census API key from https://api.census.gov/data/key_signup.html
* Add the Census API key you get to the code in RefreshAPIKey.R
* Download US Counties shapefile (cb_2017_us_county_500k) from Census TIGER files: https://www2.census.gov/geo/tiger/GENZ2017/shp/
    * Place this in the ACMT directory
* Download the US State Planes shapefile (USA_State_Plane_Zones_NAD83) from ESRI's Open Data Portal: http://edu-esriroedu.opendata.arcgis.com/datasets/23178a639bdc4d658816b3ea8ee6c3ae_0
    * Place this in the ACMT directory
* Download the 2011 National Land Cover Database from MLRC: https://s3-us-west-2.amazonaws.com/mrlc/NLCD_2011_Land_Cover_L48_20190424.zip
    * Place the zip file in the ACMT directory, unzip it, and make sure the land_cover variable in ACMT.R refers to the path to the .img file in the unzipped folder
* Run the code in ACMTPlayground.R. If it throws errors, let me know and we'll figure it out together
