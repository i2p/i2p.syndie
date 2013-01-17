import socket
import re

page1 = 'page1.html'
image1 = 'image1.gif'


addr = ('localhost', 10111)

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(addr)

i = s.makefile()



def cmd(c):
   l = []
   s.send(c)
   r = i.readline()
   while (r != '* Command execution complete.\n'):
      l.append(r)
      r = i.readline()
   status = int(re.match('\* Status\: ([\-\d]+)\\n', i.readline()).group(1))
   i.readline() # dispose of location line
   return (status, l)


cmd('menu post\n')
cmd('channels\n')
cmd('create --channel 0\n')
cmd('addpage --in ' + page1 + ' --type text/html\n')
cmd('addattachment --in ' + image1 + ' --type image/gif\n')
cmd('set --subject \"MOSFET is my hero\"\n')
cmd('execute\n')

i.close()
s.close()
