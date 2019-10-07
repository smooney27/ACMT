# ACMT
Automatic Context Measurement Tool -- work in progress

To get started, do the following:

0) Copy the ACMT Repository Locally
1) Request a Census API key from https://api.census.gov/data/key_signup.html
2) Add the Census API key you get to the code in RefreshAPIKey.R
3) Download US Counties shapefile (cb_2017_us_county_500k) from Census TIGER files: https://www2.census.gov/geo/tiger/GENZ2017/shp/
3a) Place this in the ACMT directory
4) Download the US State Planes shapefile (USA_State_Plane_Zones_NAD83) from ESRI's Open Data Portal: http://edu-esriroedu.opendata.arcgis.com/datasets/23178a639bdc4d658816b3ea8ee6c3ae_0
4a) Place this in the ACMT directory
5) Run the code in ACMTPlayground.R.  If you eventually see something that looks like:

                        |                      names |      values
1                       |             men_proportion | 5.004026e-01
2                       |   males_under_5_proportion | 5.770891e-02
3                       |    males_5_to_9_proportion | 2.238256e-02


then you're in good shape.  If not, let me know and we'll figure it out together
