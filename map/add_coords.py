import csv
import sys
import json
import urllib
import urllib2
import pprint

KEY = "VAHxsfzb9Iz9GOpwJJd9RhGPns8c0lls"

args = sys.argv
if len(args)<3:
    print("usage : "+args[0]+" <classif> <out>")

classif = open(args[1], 'r')
raw_json = classif.read()
data = json.loads(raw_json)
classif.close()

for i in data:
    print(i)
    response = urllib2.urlopen('http://www.mapquestapi.com/geocoding/v1/address?key='+KEY+"&location="+urllib.quote_plus(data[i]["city"]+ ",France"))
    api_raw_result = response.read()
    result = json.loads(api_raw_result)
    data[i]["pos"] = result["results"][0]["locations"][0]["latLng"]

print(data)

with open(args[2], 'w') as fp:
    json.dump(data, fp)
