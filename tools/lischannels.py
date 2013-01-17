import socket
import re


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



cmd('menu read\n')
nchannels = re.match('(\d+)', cmd('channels\n')[1][0]).group(1)

l = cmd('next --lines ' + nchannels + '\n')[1]
l.pop(0)
l.pop()

for line in l:
   print line

i.close()
s.close()

