import os
import sys
import numpy
import string
import csv

fingmat = dict()
#print(os.path.dirname(os.path.abspath(__file__)))
#filenames = ["659347_log_Sentences_1446629136_matched.txt"]
filenames = os.listdir(str(os.path.dirname(os.path.abspath(__file__))+'\logfiles\sent'))
for filename in filenames:
  i=0
  file = open(str(os.path.dirname(os.path.abspath(__file__))) + '\\logfiles\\sent\\' + filename, 'r')
  for line in file.readlines():
    i+=1
    #print(i+1)
    tokens = line.split('\t')
    #print(tokens)
    #print(filename)
    if (tokens[5] != 'iki'):
      if not (tokens[2] in fingmat):
        fingmat[tokens[2]] = (0.0,1.0, len(tokens[8]))
      else:
          #print(fingmat[(tokens[16], tokens[11])])
        #print(fingmat)
        fingmat[tokens[2]] = (fingmat[tokens[2]][0] + (1.0 if (tokens[7]=="BackSpace" or tokens[7] == "Delete") else 0.0), fingmat[tokens[2]][1] + 1.0, fingmat[tokens[2]][2] + (len(tokens[8]) if (tokens[2]!=last_part or tokens[3]!=last_sent) else 0) )
    else:
      print(tokens)
    last_sent = tokens[3]
    last_part = tokens[2]
print('finished')

with open('hwtperf.csv', 'w') as wf:
  writer = csv.writer(wf)
  writer.writerow(("Part_id", "sentence_id", "ECPC", "KSPC"))
  for k in fingmat:
    writer.writerow((k, float(fingmat[k][0]/fingmat[k][2]), float(fingmat[k][1]/fingmat[k][2])))