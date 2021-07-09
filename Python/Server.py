##########
# Server #
##########

import socket
import sys

HOST = 'localhost'
PORT = 11001
print ('Server Port = ' + str(PORT))

server = socket.socket (socket.AF_INET, socket.SOCK_STREAM)
print ('Socket created')

#Bind socket to a local host and port
try:
  server.bind ((HOST, PORT))
except socket.error as error:
  print('!!! ' + error.args[1])
  sys.exit()
  
#Start listening on socket
server.listen (1)
print ('Socket now listening')

#Handle clients
while True:
  connection, address = server.accept()
  print ('Connected by', address)
  
  header = bytearray(connection.recv(2))
  count = header[0] + 256 * header[1]
  if count == 0:
    connection.close
    print ('Connection closed')
    server.close
    print ('Server closed')
    break
  print ('Character count = ', count)

  data = connection.recv (count)
  if data:
    items = data.decode ('utf-8').split(':')
    telescope = items[0]
    if len(items) == 2:
      target = items[1]
      print (telescope + '> ' + target)
    else:
      print (telescope + '> no target')
  connection.sendall (header + data)
  connection.close
  print ('Connection closed')
