library(RWeka)
library(RWekajars)
library(party)
library(caTools)
library(epiDisplay)
library(caret)
library(OneR)
library(e1071)


cancerdata<-read.csv("WDBC.csv", header =TRUE) # Read the file
dim(cancerdata)

## [1] 569  12
head(cancerdata)

##   PatientID radius texture perimeter   area smoothness compactness concavity
## 1    842302  17.99   10.38    122.80 1001.0    0.11840     0.27760    0.3001
## 2    842517  20.57   17.77    132.90 1326.0    0.08474     0.07864    0.0869
## 3  84300903  19.69   21.25    130.00 1203.0    0.10960     0.15990    0.1974
## 4  84348301  11.42   20.38     77.58  386.1    0.14250     0.28390    0.2414
## 5  84358402  20.29   14.34    135.10 1297.0    0.10030     0.13280    0.1980
## 6    843786  12.45   15.70     82.57  477.1    0.12780     0.17000    0.1578

##   concavePoints symmetry fractalDimension Diagnosis
## 1       0.14710   0.2419          0.07871         M
## 2       0.07017   0.1812          0.05667         M
## 3       0.12790   0.2069          0.05999         M
## 4       0.10520   0.2597          0.09744         M
## 5       0.10430   0.1809          0.05883         M
## 6       0.08089   0.2087          0.07613         M

#set.seed(32)
ntrain <- sample(nrow(cancerdata),round(nrow(cancerdata)*.60), replace=FALSE) #Training data

cancerdata.train <- cancerdata[ntrain,] 
dim(cancerdata.train)
## [1] 341  12

head(cancerdata.train)
#PatientID radius texture perimeter   area smoothness compactness concavity concavePoints symmetry fractalDimension Diagnosis
#P338    897630  18.77   21.43    122.90 1092.0    0.09116     0.14020   0.10600       0.06090   0.1953          0.06083         M
#P565    926424  21.56   22.39    142.00 1479.0    0.11100     0.11590   0.24390       0.13890   0.1726          0.05623         M
#P434    908445  18.82   21.97    123.70 1110.0    0.10180     0.13890   0.15940       0.08744   0.1943          0.06132         M
#P201    877501  12.23   19.56     78.54  461.0    0.09586     0.08087   0.04187       0.04107   0.1979          0.06013         B
#P354   9010018  15.08   25.74     98.00  716.6    0.10240     0.09769   0.12350       0.06553   0.1647          0.06464         M
#P357   9010259  13.05   18.59     85.09  512.0    0.10820     0.13040   0.09603       0.05603   0.2035          0.06501         B



cancerdata.test<-cancerdata[ntrain,]  # Testing data
#cancerdata.test<-cancerdata[-ntrain,]  # Testing data
dim(cancerdata.test)
## [1] 228  12

majorityClass<-(table(cancerdata.train$Diagnosis)) # determine majority class

frequencyTable <- tab1(study$Diagnosis, sort.group = "increasing", cum.percent = TRUE)
frequencyTable

myClass<-names(which(majorityClass==max(majorityClass))) #Obtain the majority class

testDiagnosis<-cancerdata.test$Diagnosis # Grab the class of the test data

zeroR<-function(x,s1,s2) {    # simple function to substitute 
  # all the classes to majority class 
  gsub(s1,s2,x,fixed=TRUE)
}

#Perform substitution
testDiagnosisPred<-unlist(lapply(testDiagnosis, zeroR,s1 ="M", s2="B")) 
table(testDiagnosisPred)
## testDiagnosisPred
##   B 
## 228

majorityClass # confusion matrix
## 
##   B   M 
## 213 128

# another seed's confusion matrix
## B   M 
## 211 130 

#?confusionMatrix
#reference	
#a factor of classes to be used as the true results

#levels(testDiagnosisPred)
#levels(cancerdata.train$Diagnosis)
#levels(testDiagnosis)

#dim(testDiagnosisPred)
#dim(cancerdata.train$Diagnosis)
#dim(testDiagnosis)

#head(testDiagnosisPred)
#head(cancerdata.train$Diagnosis)
#head(testDiagnosis)

#str(testDiagnosisPred)
#str(cancerdata.train$Diagnosis)
#str(testDiagnosis)

confusionMatrix(as.factor(testDiagnosisPred), as.factor(testDiagnosis), positive="B") 


#--------------------------------------------------
#     One R Model
#--------------------------------------------------


set.seed(48)
randsamp <- sample(nrow(cancerdata), 150, replace=FALSE)
#randsamp

cancersamp <- cancerdata[randsamp,]
#cancersamp

#?sample.split

spl = sample.split(cancersamp$Diagnosis, SplitRatio = 0.7)
#spl

dataTrain = subset(cancersamp, spl==TRUE)
dataTest = subset(cancersamp, spl==FALSE)

oneRModel <- OneR(as.factor(Diagnosis)~., cancersamp)
summary(oneRModel)
dataTest.pred <- predict(oneRModel, newdata = dataTest)
table(dataTest$Diagnosis, dataTest.pred) #ope this gives a bad confusion matrix
#just ignore this 

#OneR(formula, data, subset, na.action,
#     control = Weka_control(), options = NULL)

#OneR's parameter's deduced from the J48 function
#J48(formula, data, subset, na.action,
#    control = Weka_control(), options = NULL)

# This is the 50 test 50 train ratio model from homework 3
#m5050 <- J48(as.factor(study.Diagnosis)~., dataTrain)

confusionMatrix(dataTest.pred, as.factor(dataTest$Diagnosis), positive="B") 

