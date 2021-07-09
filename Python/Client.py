##########
# Client #
##########

import socket

HOST      = '127.0.0.1'
PORT      = 11001
TELESCOPE = 'APO'

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

  s = socket.socket (socket.AF_INET, socket.SOCK_STREAM)
  s.connect ((HOST, PORT))
  s.sendall (data)

  if info == '':
    print ('Exit')
    break  
  reply = s.recv(count + 2)
  if reply == data:
    print ('ok')
  else:
    print ('failed')
