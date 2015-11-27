# MultiBoost
# os.chdir("D:/247NETWORKBACKUP/BackupOn21May/Kaggle")
import random, string, math, csv, sys
import numpy as np

def DataToArff(xs,labels,weights,header,title,fileName):
    outFile = open(fileName + ".arff","w")
    outFile.write("@RELATION " + title + "\n\n")
    #header
    for feature in header:
        outFile.write("@ATTRIBUTE " + feature + " NUMERIC\n")
    outFile.write("@ATTRIBUTE classSignal NUMERIC\n")
    outFile.write("@ATTRIBUTE classBackground NUMERIC\n")
    outFile.write("\n@DATA\n")
    for x,label,weight in zip(xs,labels,weights):
        for xj in x:
            outFile.write(str(xj) + ",")
        if label == 's':
            outFile.write(str(weight) + "," + str(-weight) + "\n")
        else:
            outFile.write(str(-weight) + "," + str(weight) + "\n")
    outFile.close()


def AMS(s,b):
    assert s >= 0
    assert b >= 0
    bReg = 10.
    return math.sqrt(2 * ((s + b + bReg) * 
                          math.log(1 + s / (b + bReg)) - s))

runtype = 'test' # 'train', 'validation', 'test'

all = list(csv.reader(open("training.csv","rb")))
header = np.array(all[0][1:-2])

xs = np.array([map(float, row[1:-2]) for row in all[1:]])
(numPoints, numFeatures) = xs.shape

sSelector = np.array([row[-1] == 's' for row in all[1:]])
bSelector = np.array([row[-1] == 'b' for row in all[1:]])

weights = np.array([float(row[-2]) for row in all[1:]])
labels = np.array([row[-1] for row in all[1:]])
sumWeights = np.sum(weights)
sumSWeights = np.sum(weights[sSelector])
sumBWeights = np.sum(weights[bSelector])

if (runtype == 'train'):
    randomPermutation = random.sample(range(len(xs)), len(xs))
    np.savetxt("randomPermutation.csv",randomPermutation,fmt='%d',delimiter=',')
else: 
    randomPermutation = np.array(map(int,np.array(list(csv.reader(open("randomPermutation.csv","rb"), delimiter=','))).flatten()))

numPointsTrain = int(numPoints*0.6)
numPointsValidation = numPoints - numPointsTrain

xsTrain = xs[randomPermutation[:numPointsTrain]]
xsValidation = xs[randomPermutation[numPointsTrain:]]

sSelectorTrain = sSelector[randomPermutation[:numPointsTrain]]
bSelectorTrain = bSelector[randomPermutation[:numPointsTrain]]
sSelectorValidation = sSelector[randomPermutation[numPointsTrain:]]
bSelectorValidation = bSelector[randomPermutation[numPointsTrain:]]

weightsTrain = weights[randomPermutation[:numPointsTrain]]
weightsValidation = weights[randomPermutation[numPointsTrain:]]

labelsTrain = labels[randomPermutation[:numPointsTrain]]
labelsValidation = labels[randomPermutation[numPointsTrain:]]

sumWeightsTrain = np.sum(weightsTrain)
sumSWeightsTrain = np.sum(weightsTrain[sSelectorTrain])
sumBWeightsTrain = np.sum(weightsTrain[bSelectorTrain])

if (runtype == 'train'):
    DataToArff(xsTrain,labelsTrain,weightsTrain,header,"HiggsML_challenge_train","training")
    DataToArff(xsValidation,labelsValidation,weightsValidation,header,"HiggsML_challenge_validation","validation")
    sys.exit("Now build the model using MLBoost ... ")

# Now build the model
# Run: ..\..\MultiBoost-1.2.02\MultiBoost-Build\multiboost --configfile config.txt

# After building the model
# Run: ..\MultiBoost-1.2.02\MultiBoost-Build\multiboost --configfile configScoresValidation.txt

if (runtype == 'validation' or runtype == 'test'):
    validationScoresText = list(csv.reader(open("scoresValidation.txt","rb"), delimiter=','))
    validationScores = np.array([float(score[0]) for score in validationScoresText])
    tIIs = validationScores.argsort()

    wFactor = 1.* numPoints / numPointsValidation

    s = np.sum(weightsValidation[sSelectorValidation])
    b = np.sum(weightsValidation[bSelectorValidation])
    amss = np.empty([len(tIIs)])
    amsMax = 0
    threshold = 0.0
    for tI in range(len(tIIs)):
    # don't forget to renormalize the weights to the same sum 
    # as in the complete training set
        amss[tI] = AMS(max(0,s * wFactor),max(0,b * wFactor))
        # careful with small regions, they fluctuate a lot
        if tI < 0.9 * len(tIIs) and amss[tI] > amsMax:
            amsMax = amss[tI]
            threshold = validationScores[tIIs[tI]]
            #print tI,threshold
        if sSelectorValidation[tIIs[tI]]:
            s -= weightsValidation[tIIs[tI]]
        else:
            b -= weightsValidation[tIIs[tI]]

    print 'Threshold value is {0}'.format(threshold)  # 1.52292

    testText = list(csv.reader(open("test.csv","rb"), delimiter=','))
    testIds = np.array([int(row[0]) for row in testText[1:]])
    xsTest = np.array([map(float, row[1:]) for row in testText[1:]])
    weightsTest = np.repeat(1.0,len(testText)-1)
    labelsTest = np.repeat('s',len(testText)-1)
    if (runtype == 'validation'):
        DataToArff(xsTest,labelsTest,weightsTest,header,"HiggsML_challenge_test","test")
        sys.exit("Wrote test files, now run MultiBoost again ... ")

# Run: ..\MultiBoost-1.2.02\MultiBoost-Build\multiboost --configfile configScoresTest.txt
# threshold = raw_input("Please enter the threshold value: ")
# print "you entered", threshold

if (runtype == 'test'):
    testScoresText = list(csv.reader(open("scoresTest_2.txt", "rb"),delimiter=','))
    testScores = np.array([float(score[0]) for score in testScoresText])

    testInversePermutation = testScores.argsort()

    testPermutation = list(testInversePermutation)
    for tI,tII in zip(range(len(testInversePermutation)),
                      testInversePermutation):
        testPermutation[tII] = tI

    submission = np.array([[str(testIds[tI]),str(testPermutation[tI]+1),
                           's' if testScores[tI] >= threshold else 'b'] 
                for tI in range(len(testIds))])

    submission = np.append([['EventId','RankOrder','Class']],
                            submission, axis=0)

    np.savetxt("submission_mb2.csv",submission,fmt='%s',delimiter=',')



# Submission - 2: baselearnertype SingleStumpLearner 8, stronglearner AdaBoostMH
