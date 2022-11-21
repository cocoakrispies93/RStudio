library(rpart)
library(rpart.plot)
library(caTools)

setwd("C:/Users/penng/OneDrive/Documents/IUSB/FA21/Data Mining/WK8")
bank_data<-read.csv("bank.csv", header=T)
head(bank_data)

set.seed(25)
#samp <- sample(nrow(bank_data),nrow(bank_data)*0.70, replace=FALSE)
samp <- sample(nrow(bank_data),nrow(bank_data)*0.70)
#head(samp)
#class(samp)

#train <- hr_data[sample_ind,]
#test <- hr_data[-sample_ind,]
#head(train)
#class(train)
#head(test)
#class(test)


#============================
#   80% Train : 20% Test
#============================

spl = sample.split(samp, SplitRatio = 0.80)

subtrue = subset(samp, spl==TRUE)
subfalse = subset(-samp, spl==FALSE)

train <- bank_data[subtrue,]
test <- bank_data[-subfalse,]

head(train)
class(train)
head(test)
class(test)

#train <- bank_data[samp,]
#test <- bank_data[-samp,]

# Base Model and Information Model
bankmodel <- rpart(y ~ ., data = train, 
                   method = "class", control = rpart.control(cp = 0))
info.model <- rpart(y~., data = train, parms=list(split="information"))

#summary(bankmodel)
#summary(info.model)

# Plot the info.model Decision Tree
prp(info.model, main ="Info Model Decision Tree", 
    box.col=c("Pink", "Green")[info.model$y],
    varlen=0,faclen=0, type=1,extra=4,under=TRUE)
rpart.plot(info.model , main ="Info Model Decision Tree")

# Plot the bankmodel Decision Tree
prp(bankmodel, main ="Bank Model Decision Tree", 
    box.col=c("Pink", "Green")[bankmodel$y],
    varlen=0,faclen=0, type=1,extra=4,under=TRUE)
rpart.plot(bankmodel, main ="Bank Model Decision Tree")

# Examine the complexity of the plot
printcp(bankmodel)
plotcp(bankmodel, main ="Bank Model Plot CP")

# Examine the complexity of the plot
printcp(info.model)
plotcp(info.model, main ="Info Model Plot CP")

# Compute the accuracy of the pruned bank model tree
test$pred <- predict(bankmodel, test, type = "class")
bankmodel_accuracy <- mean(test$pred == test$y)
bankmodel_accuracy

# Compute the accuracy of the pruned info model tree
test$pred <- predict(info.model, test, type = "class")
info.model_accuracy <- mean(test$pred == test$y)
info.model_accuracy

# Grow a bank model tree with minsplit of 100 and max depth of 8
bankmodel_preprune <- rpart(y ~ ., data = train, method = "class", 
                            control = rpart.control(cp = 0, 
                                                    maxdepth = 8,minsplit = 100))

printcp(bankmodel_preprune)
plotcp(bankmodel_preprune, main ="Bank Model Preprune Plot CP")
rpart.plot(bankmodel_preprune, main ="Bank Model Preprune Rpart Plot")

# Compute the accuracy of the pruned tree
test$pred <- predict(bankmodel_preprune, test, type = "class")
accuracy_preprune <- mean(test$pred == test$y)
accuracy_preprune

# Postpruning
# Prune the bankmodel based on the optimal cp value
bankmodel_postprune021 <- prune(bankmodel, cp = 0.021 )
rpart.plot(bankmodel_postprune021, 
           main ="Bank Model Postprune Rpart Plot CP 0.021")

bankmodel_postprune012 <- prune(bankmodel, cp = 0.012 )
rpart.plot(bankmodel_postprune012, 
           main ="Bank Model Postprune Rpart Plot CP 0.012")

bankmodel_postprune0084 <- prune(bankmodel, cp = 0.0084 )
rpart.plot(bankmodel_postprune0084, 
           main ="Bank Model Postprune Rpart Plot CP 0.0084")

bankmodel_postprune11 <- prune(bankmodel, cp = 0.11 )
rpart.plot(bankmodel_postprune0084, 
           main ="Bank Model Postprune Rpart Plot CP 0.11")

bankmodel_postprune01 <- prune(bankmodel, cp = 0.01 )
rpart.plot(bankmodel_postprune01, 
           main ="Bank Model Postprune Rpart Plot CP 0.01")

# Compute the accuracy of the pruned tree CP = 0.021
test$pred <- predict(bankmodel_postprune021, test, type = "class")
accuracy_postprune021 <- mean(test$pred == test$y)
accuracy_postprune021

# Compute the accuracy of the pruned tree CP = 0.012
test$pred <- predict(bankmodel_postprune012, test, type = "class")
accuracy_postprune012 <- mean(test$pred == test$y)
accuracy_postprune012

# Compute the accuracy of the pruned tree CP = 0.0084
test$pred <- predict(bankmodel_postprune0084, test, type = "class")
accuracy_postprune0084 <- mean(test$pred == test$y)
accuracy_postprune0084

# Compute the accuracy of the pruned tree CP = 0.11
test$pred <- predict(bankmodel_postprune11, test, type = "class")
accuracy_postprune11 <- mean(test$pred == test$y)
accuracy_postprune11

# Compute the accuracy of the pruned tree CP = 0.01
test$pred <- predict(bankmodel_postprune01, test, type = "class")
accuracy_postprune01 <- mean(test$pred == test$y)
accuracy_postprune01

# Display accuracy results
data.frame(bankmodel_accuracy, info.model_accuracy, 
           accuracy_preprune, accuracy_postprune012, 
           accuracy_postprune021, accuracy_postprune0084, 
           accuracy_postprune11, accuracy_postprune01)