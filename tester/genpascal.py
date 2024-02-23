""" generates instructions to read/write frames based on cp_plus.log
 (other captures might not work since it interprets a checksum error as
  a read frame)
"""  
import sys
frames=[]
def myhex(v):
  return '$'+hex(v)[2:]
  
count=0  
oldtime=0
for line in sys.stdin:
   count=count+1;
   tokens=line.split()
   if tokens[0]=='Time':
     continue
   pid=int(tokens[1],16)
   if pid==0:
     continue
   newtime=float(tokens[0].replace(',','.')) 
   if oldtime!=0: 
     print('sleep(',int((newtime-oldtime)*1000),');');
   print('writeln(\'line:\', ',count,');')
   oldtime=newtime
   pid=pid & 0x3f  
   if tokens[3]=='Checksum':
     print('ReadFrame(',myhex(pid),',lindata,8);')
   else:
     print('setlength(lindata,8); ', end='')
     for index in range(8):
       print('byte(lindata[',index+1,']):=','$'+tokens[index+2],';',end='')
     print('WriteFrame(',myhex(pid),',lindata);')       
   