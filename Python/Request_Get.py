import requests
import sys

if len(sys.argv) != 2:
    print('error')
    sys.exit(-1)
url = sys.argv[1]
response = requests.get(url)
print (response.text)
sys.exit(0)

