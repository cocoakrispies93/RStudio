
#importing libraries
library(readr)
library(writexl)
library(readxl)
library(stringr)
library(ggplot2)
library(dplyr)
#library(lessR)
library(rpart)
library(rpart.plot)
library(caTools)
library(data.tree)

# libraries for plotting the graph
library(ggraph) 
library(igraph)
library(tidyverse)





getwd()
setwd("C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\Data Mining\\Quiz 3")
getwd()

data <- read_excel("trees.xlsx", col_names = TRUE)
dim(data) #dimensions of dataset 
head(data) #brief overview

set.seed(25)
#samp <- sample(nrow(data),nrow(data)*0.70, replace=FALSE)
samp <- sample(nrow(data),nrow(data))
length(samp)

#train <- hr_data[sample_ind,]
#test <- hr_data[-sample_ind,]
#head(train)
#class(train)
#head(test)
#class(test)


#============================
#   80% Train : 20% Test
#============================

spl = sample.split(samp, SplitRatio = 0.50)

subtrue = subset(samp, spl==TRUE) # 50%
subfalse = subset(-samp, spl==FALSE) # 50%

subtrue = subset(samp) 
subfalse = subset(-samp) #reverse

train <- data
test <- data

head(train)
head(test)

#adjust plot margins
par(mar = c(1, 1, 1, 1))

#train <- data[samp,]
#test <- data[-samp,]

# Model and Information Model
model <- rpart(Class ~ ., data = train, 
                   method = "class", control = rpart.control(cp = 0))
info.model <- rpart(Class~., data = train, parms=list(split="information"))

#summary(model)
#summary(info.model)

# Plot the info.model Decision Tree
prp(info.model, main ="Info Model Decision Tree", 
    box.col=c("Pink", "Green")[info.model$Class],
    varlen=0,faclen=0, type=1,extra=4,under=TRUE)
rpart.plot(info.model , main ="Info Model Decision Tree")

# Plot the bankmodel Decision Tree
prp(model, main ="Model Decision Tree", 
    box.col=c("Pink", "Green")[model$Class],
    varlen=0,faclen=0, type=1,extra=4,under=TRUE)
rpart.plot(model, main ="Model Decision Tree")

# Examine the complexity of the plot
printcp(model)
plotcp(model, main ="Model Plot CP")

# Examine the complexity of the plot
printcp(info.model)
plotcp(info.model, main ="Info Model Plot CP")

# Compute the accuracy of the pruned bank model tree
test$pred <- predict(model, test, type = "class")
model_accuracy <- mean(test$pred == test$Class)
model_accuracy

# Compute the accuracy of the pruned info model tree
test$pred <- predict(info.model, test, type = "class")
info.model_accuracy <- mean(test$pred == test$Class)
info.model_accuracy

# Grow a model tree with minsplit of 100 and max depth of 8
model_preprune <- rpart(Class ~ ., data = train, method = "class", 
                            control = rpart.control(cp = 0, 
                                                    maxdepth = 8,minsplit = 10))

printcp(model_preprune)
plotcp(model_preprune, main ="Model Preprune Plot CP")
rpart.plot(model_preprune, main ="Model Preprune Rpart Plot")

# Compute the accuracy of the pruned tree
test$pred <- predict(model_preprune, test, type = "class")
accuracy_preprune <- mean(test$pred == test$Class)
accuracy_preprune

# Postpruning
# Prune the bankmodel based on the optimal cp value
model_postprune021 <- prune(model, cp = 0.021 )
rpart.plot(model_postprune021, 
           main ="Model Postprune Rpart Plot CP 0.021")

model_postprune012 <- prune(model, cp = 0.012 )
rpart.plot(model_postprune012, 
           main ="Model Postprune Rpart Plot CP 0.012")

model_postprune0084 <- prune(model, cp = 0.0084 )
rpart.plot(model_postprune0084, 
           main ="Model Postprune Rpart Plot CP 0.0084")

model_postprune11 <- prune(model, cp = 0.11 )
rpart.plot(model_postprune0084, 
           main ="Model Postprune Rpart Plot CP 0.11")

model_postprune01 <- prune(model, cp = 0.01 )
rpart.plot(model_postprune01, 
           main ="Model Postprune Rpart Plot CP 0.01")

model_postprune5 <- prune(model, cp = 0.5 )
rpart.plot(model_postprune01, 
           main ="Model Postprune Rpart Plot CP 0.5")