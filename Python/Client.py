##########
# Client #
##########

import socket
import sys

PORT      = 11001
TELESCOPE = 'APO'

host = '192.168.110.129'
while True:
  target = input ('Target>')
  if target == '':
    info = TELESCOPE
  elif target == 'x':
    info = ''
  else:
    info = TELESCOPE + ':' + target
  count = len (info)
  header = bytearray ([count % 256, count // 256])
  data = header + bytearray (info, 'utf-8')

  while True:
    try: 
      s = socket.socket (socket.AF_INET, socket.SOCK_STREAM)
      s.connect ((host, PORT))
      break
    except socket.error as error:
      print('!!! ' + error.args[1])
    except:
      print('!!! unknown error')
    s.close
    host = input ('Server IP address>')
    if host == 'x':
      sys.exit()

  s.sendall (data)

  if info == '':
    print ('Exit')
    break  
  reply = s.recv(count + 2)
  if reply == data:
    print ('ok')
  else:
    print ('failed')
