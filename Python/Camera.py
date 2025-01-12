import os
import time
import array
import numpy as np
import astropy.io.fits as fits
from alpaca import discovery
from alpaca import management
from alpaca.camera import *
from alpaca.exceptions import *

svrs = discovery.search_ipv4()
print(svrs)
for svr in svrs:
    print(f"At {svr}")
    print (f"  V{management.apiversions(svr)} server")
    print (f"  {management.description(svr)['ServerName']}")
    devs = management.configureddevices(svr)
    for dev in devs:
        print(f"    {dev['DeviceType']}[{dev['DeviceNumber']}]: {dev['DeviceName']}")

C = Camera('localhost:32323',0)
C.Connected = True
print (C.Name)
C.BinX = 1
C.BinY = 1
# Assure full frame after binning change
C.StartX = 0
C.StartY = 0
C.NumX = C.CameraXSize // C.BinX    # Watch it, this needs to be an int (typ)
C.NumY = C.CameraYSize // C.BinY
#
# Acquire a light image, wait while printing % complete
#
C.StartExposure(2.0, True)
while not C.ImageReady:
    time.sleep(0.5)
    print(f'{C.PercentCompleted}% complete')
print('finished')
#
# OK image acquired, grab the image array and the metadata
#
img = C.ImageArray
imginfo = C.ImageArrayInfo
if imginfo.ImageElementType == ImageArrayElementTypes.Int32:
    if C.MaxADU <= 65535:
        imgDataType = np.uint16 # Required for BZERO & BSCALE to be written
    else:
        imgDataType = np.int32
elif imginfo.ImageElementType == ImageArrayElementTypes.Double:
    imgDataType = np.float64
#
# Make a numpy array of he correct shape for astropy.io.fits
#
if imginfo.Rank == 2:
    nda = np.array(img, dtype=imgDataType).transpose()
else:
    nda = np.array(img, dtype=imgDataType).transpose(2,1,0)
#
# Create the FITS header and common FITS fields
#
hdr = fits.Header()
hdr['COMMENT'] = 'FITS (Flexible Image Transport System) format defined in Astronomy and'
hdr['COMMENT'] = 'Astrophysics Supplement Series v44/p363, v44/p371, v73/p359, v73/p365.'
hdr['COMMENT'] = 'Contact the NASA Science Office of Standards and Technology for the'
hdr['COMMENT'] = 'FITS Definition document #100 and other FITS information.'
if imgDataType ==  np.uint16:
    hdr['BZERO'] = 32768.0
    hdr['BSCALE'] = 1.0
hdr['EXPOSURE'] = C.LastExposureDuration
hdr['EXPTIME'] = C.LastExposureDuration
hdr['DATE-OBS'] = C.LastExposureStartTime
hdr['TIMESYS'] = 'UTC'
hdr['XBINNING'] = C.BinX
hdr['YBINNING'] = C.BinY
hdr['INSTRUME'] = C.SensorName
try:
    hdr['GAIN'] = C.Gain
except:
    pass
try:
    hdr['OFFSET'] = C.Offset
    if type(C.Offset == int):
        hdr['PEDESTAL'] = C.Offset
except:
    pass
hdr['HISTORY'] = 'Created using Python alpyca-client library'
#
# Create the final FITS from the numpy array and FITS info
#
hdu = fits.PrimaryHDU(nda, header=hdr)

img_file = f"{os.getenv('USERPROFILE')}/Desktop/test.fts"
hdu.writeto(img_file, overwrite=True)

C.Connected = False

print("Booyah! Your FITS image is ready.")
