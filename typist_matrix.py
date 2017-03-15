import os
import sys
import numpy
import string
import csv

fingmat = dict()
#print(os.path.dirname(os.path.abspath(__file__)))
#filenames = ["659347_log_Sentences_1446629136_matched.txt"]
filenames = os.listdir(str(os.path.dirname(os.path.abspath(__file__))+'\logfiles\sent'))

flatten = lambda l: [item for sublist in l for item in sublist]
letters = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
digits = [i for i in range(1,11)]
fing = dict({"Thumb":1, "Index":2, "Middle":3, "Ring":4, "Little":5})

for filename in filenames:
  i=0
  file = open(str(os.path.dirname(os.path.abspath(__file__))) + '\\logfiles\\sent\\' + filename, 'r')
  for line in file.readlines():
      i+=1
      #print(i+1)
      tokens = line.split('\t')
      tokens = [t.strip() for t in tokens]
      #print(tokens)
      #print(filename)
      #if (tokens[5] != 'iki'):
      if (tokens[6] in letters and all(ii in string.printable for ii in tokens[11])):
        print(tokens[2], tokens[0], tokens[6], letters.index(tokens[6]), tokens[12])
        if not (tokens[2] in fingmat):
          fingmat[tokens[2]]  = [[0 for i in digits] for l in letters]
          fingmat[tokens[2]][letters.index(tokens[6])][int(tokens[11][0]=="R")*5+fing[tokens[11][2:]]] = 1
        else:
          #print(fingmat[(tokens[], tokens[11])])
          
          fingmat[tokens[2]][letters.index(tokens[6])][int(tokens[11][0]=="R")*5+fing[tokens[11][2:]]-1] += 1
      #else:
      #print(tokens)
print('finished')

with open('typingmatrix.txt', 'w') as wf:
  writer = csv.writer(wf)
  
  fields = flatten([[c+str(i) for i in digits] for c in letters])
  writer.writerow(("Participant_id",fields))
  for k in fingmat:
    #print(string.printable)
    #for p in fingmat[k]:
    writer.writerow((k, flatten([[j/sum(i) if sum(i)>0 else 0 for j in i] for i in fingmat[k]])))
    #if ((len(fingmat[k])>4) & (numpy.std(fingmat[k]) < numpy.mean(fingmat[k]) )):
    #writer.writerow((k, numpy.mean(fingmat[k]), numpy.std(fingmat[k]), len(fingmat[k])))