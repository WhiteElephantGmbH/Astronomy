##########
# Server #
##########

import socket

HOST = ''
PORT = 11001
print("Server Port = " + str(PORT))

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
print 'Socket created'

#Bind socket to a local host and port
try:
  s.bind((HOST, PORT))
except socket.error as msg:
  print 'Bind failed. Error Code : ' + str(msg[0]) + ' Message ' + msg[1]
  sys.exit()
  
#Start listening on socket
s.listen(1)
print 'Socket now listening'

#Handle clients
while True:
  connection, address = s.accept()
  print('Connected by', address)
  
  header = bytearray(connection.recv(2))
  count = header[0] + 256 * header[1]
  print (count)

  data = connection.recv(count)
  if data:
    items = data.decode("utf-8").split(":")
    telescope = items[0]
    if len(items) == 2:
      target = items[1]
      print(telescope + "> " + target)
    else:
      print telescope + "> no target"
  connection.sendall(data)
  connection.close
  print 'Socket closed'
