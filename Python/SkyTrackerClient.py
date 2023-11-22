import requests
import sys

url = "http://127.0.0.1:9000/information"
response = requests.get(url)
print (response.text)
sys.exit(0)

