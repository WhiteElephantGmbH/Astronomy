##########
# Client #
##########

import socket

HOST      = '127.0.0.1'
PORT      = 11001
TELESCOPE = 'APO'

while True:
  target = raw_input('Target>')
  if target == '':
    info = TELESCOPE
  else:
    info = TELESCOPE + ':' + target
  count = len(info)
  header = bytearray([count % 256, count // 256])
  data = header + bytearray(info, 'Utf8')

  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((HOST, PORT))
  s.sendall(data)
  reply = s.recv(count)
  s.close
  if reply == info:
    print('ok')
  else:
    print('failed')
