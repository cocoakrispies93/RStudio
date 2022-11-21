library(RWeka)
library(RWekajars)
library(party)
library(caTools)

# Read data file
getwd()
study <- read.csv("WDBC.csv")
study
#study <- lapply(study, as.factor)

str(study)
summary(study)

#according to the study, the best predictive data was based on mean texture,
#worst smoothness, and worst area
cancerdata <- data.frame(study$Diagnosis, study$texture, study$smoothness, study$area)
cancerdata

str(cancerdata)
summary(cancerdata)

dim(cancerdata)
head(cancerdata)

#?sample()

#a) 50 train: 50 test
#b) 75 train: 25 test
#c) 25 train: 75 test
#d) 85 train: 15 test





#---------------------------------
#       50 Train : 50 Test
#---------------------------------

set.seed(1)
randsamp <- sample(nrow(cancerdata), 150, replace=FALSE)
randsamp

cancersamp <- cancerdata[randsamp,]
cancersamp

?sample.split

spl = sample.split(cancersamp$study.Diagnosis, SplitRatio = 0.5)
spl

dataTrain = subset(cancersamp, spl==TRUE)
dataTest = subset(cancersamp, spl==FALSE)

#cancersamp$study.Diagnosis <- as.factor(cancersamp$study.Diagnosis)
#m5050 <- J48(cancersamp$study.Diagnosis~., dataTrain) 

#cancersamp$study.Diagnosis <- as.factor(cancersamp$study.Diagnosis)

m5050 <- J48(as.factor(study.Diagnosis)~., dataTrain)

#cancersamp$study.Diagnosis <- as.factor(cancersamp$study.Diagnosis)
#m5050 <- J48(study.Diagnosis~., dataTrain)

#m5050 <- J48(as.factor(study.Diagnosis)~., dataTrain) //this one was good

#m5050 <- J48(as.factor(cancersamp$study.Diagnosis)~., dataTrain) 

summary(m5050)

## visualization the model
## use partykit package
if(require("partykit", quietly = TRUE)) plot(m5050)

dataTest.pred <- predict(m5050, newdata = dataTest)
table(dataTest$study.Diagnosis, dataTest.pred)




#---------------------------------
#       75 Train : 25 Test
#---------------------------------

set.seed(2)
randsamp <- sample(nrow(cancerdata), 150, replace=FALSE)
randsamp

cancersamp <- cancerdata[randsamp,]
cancersamp

?sample.split

spl = sample.split(cancersamp$study.Diagnosis, SplitRatio = 0.75)
spl

dataTrain = subset(cancersamp, spl==TRUE)
dataTest = subset(cancersamp, spl==FALSE)

#cancersamp$study.Diagnosis <- as.factor(cancersamp$study.Diagnosis)

m7525 <- J48(as.factor(study.Diagnosis)~., dataTrain) 

#m7525 <- J48(as.factor(cancersamp$study.Diagnosis)~., dataTrain) 

#m5050 <- J48(cancersamp$study.Diagnosis~., dataTrain) 

summary(m7525)

## visualization the model
## use partykit package
if(require("partykit", quietly = TRUE)) plot(m7525)

dataTest.pred <- predict(m7525, newdata = dataTest)
table(dataTest$study.Diagnosis, dataTest.pred)




#---------------------------------
#       25 Train : 75 Test
#---------------------------------

set.seed(3)
randsamp <- sample(nrow(cancerdata), 150, replace=FALSE)
randsamp

cancersamp <- cancerdata[randsamp,]
cancersamp

?sample.split

spl = sample.split(cancersamp$study.Diagnosis, SplitRatio = 0.25)
spl

dataTrain = subset(cancersamp, spl==TRUE)
dataTest = subset(cancersamp, spl==FALSE)

#cancersamp$study.Diagnosis <- as.factor(cancersamp$study.Diagnosis)

m2575 <- J48(as.factor(study.Diagnosis)~., dataTrain)

#m2575 <- J48(as.factor(cancersamp$study.Diagnosis)~., dataTrain) 

#m5050 <- J48(cancersamp$study.Diagnosis~., dataTrain) 

summary(m2575)

## visualization the model
## use partykit package
if(require("partykit", quietly = TRUE)) plot(m2575)

dataTest.pred <- predict(m2575, newdata = dataTest)
table(dataTest$study.Diagnosis, dataTest.pred)




#---------------------------------
#       85 Train : 15 Test
#---------------------------------

set.seed(3)
randsamp <- sample(nrow(cancerdata), 150, replace=FALSE)
randsamp

cancersamp <- cancerdata[randsamp,]
cancersamp

?sample.split

spl = sample.split(cancersamp$study.Diagnosis, SplitRatio = 0.85)
spl

dataTrain = subset(cancersamp, spl==TRUE)
dataTest = subset(cancersamp, spl==FALSE)

#cancersamp$study.Diagnosis <- as.factor(cancersamp$study.Diagnosis)

m8515 <- J48(as.factor(study.Diagnosis)~., dataTrain)

#m8515 <- J48(as.factor(cancersamp$study.Diagnosis)~., dataTrain) 

#m5050 <- J48(cancersamp$study.Diagnosis~., dataTrain) 

summary(m8515)

## visualization the model
## use partykit package
if(require("partykit", quietly = TRUE)) plot(m8515)

dataTest.pred <- predict(m8515, newdata = dataTest)
table(dataTest$study.Diagnosis, dataTest.pred)



