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
      if ((int(tokens[5])>0) &  (tokens[11]!='') & (tokens[-1]!='') & (tokens[-3]!='') & all(ii in string.printable for ii in tokens[11]) & all(ii in string.printable for ii in tokens[-3])):
        if not ((tokens[2], tokens[-1], tokens[11], tokens[-3]) in fingmat):
          fingmat[(tokens[2], tokens[-1], tokens[11], tokens[-3])] = [(int(tokens[5]))] #,float(tokens[9]))
        else:
          #print(fingmat[(tokens[16], tokens[11])])
          fingmat[(tokens[2], tokens[-1], tokens[11], tokens[-3])].append(int(tokens[5]))
    else:
      print(tokens)
print('finished')

with open('summary.csv', 'w') as wf:
  writer = csv.writer(wf)
  writer.writerow(("(Part_id, first_finger, second_finger, bigram)","mean_iki", "sd_iki", "No of observations"))
  for k in fingmat:
    #print(string.printable)
    for p in fingmat[k]:
        writer.writerow((k, p))
    #if ((len(fingmat[k][0])>4) & (numpy.std(fingmat[k][0]) < numpy.mean(fingmat[k][0]) )):
    #writer.writerow((k, numpy.mean(fingmat[k][0]), numpy.std(fingmat[k][0]), len(fingmat[k][0]), fingmat[k][1]))