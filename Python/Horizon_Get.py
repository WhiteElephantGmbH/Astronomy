import requests
import sys

if len(sys.argv) != 6:
    print('expected parameters: Target   Site_Coord                        Start_Time Stop_Time  Step_Size')
    print('        for example: 90000305 8.60986388888,47.70550277777,0.54 2022-02-14 2022-02-21 1%20HOURS')
    sys.exit(-1)
url = "https://ssd.jpl.nasa.gov/api/horizons.api"
url += "?MAKE_EPHEM=YES"
url += "&COMMAND='"
url += sys.argv[1] #<target>
url += "'&EPHEM_TYPE=OBSERVER"
url += "&CENTER='coord@399'"
url += "&COORD_TYPE=GEODETIC"
url += "&SITE_COORD='"
url += sys.argv[2] #8.60986388888,47.70550277777,0.54
url += "'&START_TIME='"
url += sys.argv[3] #2022-02-10
url += "'&STOP_TIME='"
url += sys.argv[4] #2022-02-11
url += "'&STEP_SIZE='"
url += sys.argv[5] #1%20HOURS
url += "'&QUANTITIES='1'"
url += "&REF_SYSTEM=ICRF"
url += "&CAL_FORMAT=BOTH"
url += "&TIME_DIGITS=MINUTES"
url += "&ANG_FORMAT=DEG"
url += "&APPARENT=AIRLESS"
url += "&RANGE_UNITS=AU"
url += "&SUPPRESS_RANGE_RATE=NO"
url += "&ELEV_CUT='0'"
url += "&SKIP_DAYLT=NO"
url += "&SOLAR_ELONG='0,180'"
url += "&EXTRA_PREC=NO"
url += "&R_T_S_ONLY=NO"
url += "&CSV_FORMAT=YES"
url += "&OBJ_DATA=NO"
response = requests.get(url)
print (response.text)
sys.exit(0)

