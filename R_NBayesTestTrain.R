library(e1071)
library(ROCR)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape)

# Read study file
getwd()
bank <- read.csv("attemptCSV.csv")
head(bank)

theme_set(theme_gray())

set.seed(100)
samp <- sample(nrow(bank), 150, replace=FALSE)


#---------------------------------
#       75 Train : 25 Test
#---------------------------------

spl = sample.split(samp$y, SplitRatio = 0.75)

train = subset(samp, spl==TRUE)
test = subset(-samp, spl==FALSE)



## GOOD ONE
# split into train/test data
xTrain <- train[,-150]
xTrain
yTrain <- train$y
yTrain
xTest <- test[,-150]
yTest <- test$y

# fit naive bayes model with default params
model <- naiveBayes(xTrain, yTrain)

# look at confusion matrix
table(predict(model, xTest), yTest)

# plot ROC curve
pred <- prediction(probs[, "yes"], yTest)
perf_nb <- performance(pred, measure='tpr', x.measure='fpr')
plot(perf_nb)

performance(pred, 'auc')

# plot histogram of predicted probabilities
probs <- predict(model, xTest, type="raw")
qplot(x=probs[, "yes"], geom="histogram")

head(probs)
head(yTest)

# plot ROC curve
pred <- prediction(probs, yTest)
perf_lr <- performance(pred, measure='tpr', x.measure='fpr')
plot(perf_lr)

# plot ROC for each method
roc_nb <- data.frame(fpr=unlist(perf_nb@x.values), tpr=unlist(perf_nb@y.values))
roc_nb$method <- "naive bayes"
roc_lr <- data.frame(fpr=unlist(perf_lr@x.values), tpr=unlist(perf_lr@y.values))
roc_lr$method <- "logistic regression"
rbind(roc_nb, roc_lr) %>%
        ggplot(data=., aes(x=fpr, y=tpr, linetype=method, color=method)) + 
        geom_line() +
        geom_abline(a=1, b=0, linetype=2) +
        scale_x_continuous(labels=percent, lim=c(0,1)) +
        scale_y_continuous(labels=percent, lim=c(0,1)) +
        theme(legend.position=c(0.8,0.2), legend.title=element_blank())

#---------------------------------
#       50 Train : 50 Test
#---------------------------------

spl = sample.split(samp$y, SplitRatio = 0.5)

train = subset(samp, spl==TRUE)
test = subset(-samp, spl==FALSE)


## GOOD ONE
# split into train/test data
xTrain <- train[,-150]
xTrain
yTrain <- train$y
yTrain
xTest <- test[,-150]
yTest <- test$y

# fit naive bayes model with default params
model <- naiveBayes(xTrain, yTrain)

# look at confusion matrix
table(predict(model, xTest), yTest)

# plot ROC curve
pred <- prediction(probs[, "yes"], yTest)
perf_nb <- performance(pred, measure='tpr', x.measure='fpr')
plot(perf_nb)

performance(pred, 'auc')

# plot histogram of predicted probabilities
probs <- predict(model, xTest, type="raw")
qplot(x=probs[, "yes"], geom="histogram")

head(probs)
head(yTest)

# plot ROC curve
pred <- prediction(probs, yTest)
perf_lr <- performance(pred, measure='tpr', x.measure='fpr')
plot(perf_lr)

# plot ROC for each method
roc_nb <- data.frame(fpr=unlist(perf_nb@x.values), tpr=unlist(perf_nb@y.values))
roc_nb$method <- "naive bayes"
roc_lr <- data.frame(fpr=unlist(perf_lr@x.values), tpr=unlist(perf_lr@y.values))
roc_lr$method <- "logistic regression"
rbind(roc_nb, roc_lr) %>%
        ggplot(data=., aes(x=fpr, y=tpr, linetype=method, color=method)) + 
        geom_line() +
        geom_abline(a=1, b=0, linetype=2) +
        scale_x_continuous(labels=percent, lim=c(0,1)) +
        scale_y_continuous(labels=percent, lim=c(0,1)) +
        theme(legend.position=c(0.8,0.2), legend.title=element_blank())

#---------------------------------
#       25 Train : 75 Test
#---------------------------------

spl = sample.split(samp$y, SplitRatio = 0.25)

train = subset(samp, spl==TRUE)
test = subset(-samp, spl==FALSE)


## GOOD ONE
# split into train/test data
xTrain <- train[,-150]
xTrain
yTrain <- train$y
yTrain
xTest <- test[,-150]
yTest <- test$y

# fit naive bayes model with default params
model <- naiveBayes(xTrain, yTrain)

# look at confusion matrix
table(predict(model, xTest), yTest)

# plot ROC curve
pred <- prediction(probs[, "yes"], yTest)
perf_nb <- performance(pred, measure='tpr', x.measure='fpr')
plot(perf_nb)

performance(pred, 'auc')

# plot histogram of predicted probabilities
probs <- predict(model, xTest, type="raw")
qplot(x=probs[, "yes"], geom="histogram")

head(probs)
head(yTest)

# plot ROC curve
pred <- prediction(probs, yTest)
perf_lr <- performance(pred, measure='tpr', x.measure='fpr')
plot(perf_lr)

# plot ROC for each method
roc_nb <- data.frame(fpr=unlist(perf_nb@x.values), tpr=unlist(perf_nb@y.values))
roc_nb$method <- "naive bayes"
roc_lr <- data.frame(fpr=unlist(perf_lr@x.values), tpr=unlist(perf_lr@y.values))
roc_lr$method <- "logistic regression"
rbind(roc_nb, roc_lr) %>%
        ggplot(data=., aes(x=fpr, y=tpr, linetype=method, color=method)) + 
        geom_line() +
        geom_abline(a=1, b=0, linetype=2) +
        scale_x_continuous(labels=percent, lim=c(0,1)) +
        scale_y_continuous(labels=percent, lim=c(0,1)) +
        theme(legend.position=c(0.8,0.2), legend.title=element_blank())











