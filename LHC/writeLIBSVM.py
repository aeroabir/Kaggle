' Read the csv file and write output in LIBSVM format'
' @ Abir, 5th June, 2014'

import os, csv, random, sys

os.chdir("D:/247NETWORKBACKUP/BackupOn21May/Kaggle")
# Read the static predictor names and columns

outvar = 'resp'
sample_rate = 0.1
f_train = open("traindata_SVM.csv")
read_train = csv.reader(f_train)
header = read_train.next()
outvar_column = header.index(outvar)

# print outvar_column

fw = open("SVM_train.dat", 'w')

nline = 0
numN = 0
numY = 0
signal = list()
noise  = list()
for row in read_train:
	
	y = row[outvar_column]
	if (int(y)==0):
		numN += 1
		noise.append(row)
	else: 
		numY += 1
		signal.append(row)
	nline += 1

	row2 = row
	# row2.remove(y)
	elems = map(float, row2[0:outvar_column])
	op_line = [str(y)]
	for idx, elem in enumerate(elems):
		if elem != 0:
			pos = idx+1
			op_line.append(str(pos)+":"+str(elem))
	# print op_line		
	fw.write(" ".join(op_line)+"\n")

# print(map(len, signal))
# sys.exit('Here ------------')
print 'Written {0} lines for train set with {1} signal and {2} background class'.format(nline, numY, numN)
f_train.close()
fw.close()

# Sample randomly from the cart and non-cart journeys
random.seed(100)
signalRows  = sorted(random.sample(range(numY), int(numY*sample_rate)))
noiseRows   = sorted(random.sample(range(numN), int(numN*sample_rate)))

# print numY, len(signalRows), len(signal)
# print signal[483], len(signal[483])

# Get sampled data for both signal and noise
findx = 0
cindx = 0
writeIndex1 = 0
sampledSignal = list()
for lines in signal:
	if(cindx < len(signalRows)):
		if (findx == signalRows[cindx]):
			# print cindx, signalRows[cindx], findx, writeIndex1
			cindx = cindx + 1
			sampledSignal.append(lines) 
			writeIndex1 = writeIndex1 + 1
		else: 
			pass
	findx = findx + 1
print 'Sampled {0} lines of signal data'.format(writeIndex1)

findx = 0
cindx = 0
writeIndex1 = 0
sampledNoise = list()
for lines in noise:
	if(cindx < len(noiseRows)):
		if (findx == noiseRows[cindx]):
			# print cindx, signalRows[cindx], findx, writeIndex1
			cindx = cindx + 1
			sampledNoise.append(lines) 
			writeIndex1 = writeIndex1 + 1
		else: 
			pass
	findx = findx + 1
print 'Sampled {0} lines of noise data'.format(writeIndex1)

# print sampledSignal[0:2]

op_line = list()
fw1 = open('training_sampled.dat', 'w')
for index in range(len(sampledSignal)):
	elems2 = map(float, sampledSignal[index])
	y = int(elems2[outvar_column])
	op_line = [str(y)]
	for idx, elem in enumerate(elems2[0:outvar_column]):
		if elem != 0:
			pos = idx+1
			op_line.append(str(pos)+":"+str(elem))
	# print op_line		
	fw1.write(" ".join(op_line)+"\n")

for index in range(len(sampledNoise)):
	elems2 = map(float, sampledNoise[index])
	y = int(elems2[outvar_column])
	op_line = [str(y)]
	for idx, elem in enumerate(elems2[0:outvar_column]):
		if elem != 0:
			pos = idx+1
			op_line.append(str(pos)+":"+str(elem))
	# print op_line		
	fw1.write(" ".join(op_line)+"\n")

fw1.close()

# f_test = open("testdata_SVM.csv")
# read_test = csv.reader(f_test)
# header = read_test.next()
# outfile = "SVM_test.dat"
# fw = open(outfile, 'w')
# nline = 0
# for row in read_test:
# 	elems = map(float, row)
# 	op_line = [str(1)]
# 	for idx, elem in enumerate(elems):
# 		if elem != 0:
# 			pos = idx+1
# 			op_line.append(str(pos)+":"+str(elem))
# 	fw.write(" ".join(op_line)+"\n")
# 	nline += 1

# print 'Written {0} lines for test set in {1}'.format(nline, outfile)
# f_test.close()
# fw.close()