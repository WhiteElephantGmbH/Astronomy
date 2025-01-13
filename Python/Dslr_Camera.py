import win32com.client
from astropy.io import fits

# if you don't know what your driver is called, use the ASCOM Chooser
#x = win32com.client.Dispatch("ASCOM.Utilities.Chooser")
#x.DeviceType = 'Camera'
#driver = x.Choose(None)
#print (driver)

# otherwise, just use it
driver = "ASCOM.DSLR.Camera"

camera = win32com.client.Dispatch(driver)
camera.connected = True
camera.CoolerOn = True

n = camera.Name
print ('camera =',n)

xp = camera.PixelSizeX
print ('pixel size x =',xp)
yp = camera.PixelSizeX
print ('pixel size y =',yp)

x = camera.CameraXSize
print ('camera size x =',x)
y = camera.CameraYSize
print ('camera size y =',y)

s = camera.HasShutter
print ('camera shutter =',s)

openshutter = True # False will take a dark frame
exptime = 3
camera.StartExposure(exptime,openshutter)
image = camera.ImageArray

hdu = fits.PrimaryHDU(image)
hdu.writeto('test.fits',overwrite=True)

camera.connected = False

# see more camera methods/properties here:
# https://ascom-standards.org/Help/Developer/html/T_ASCOM_DriverAccess_Camera.htm
