library(Rcpp)
library(RSNNS)
library(ggplot2)
library(plotROC)
library(tidyr)
library(cowplot)
#library(reshape2)
#library(ISLR)
#library(rpart)
#library(rpart.plot)
#library(caTools)
#library(random)

setwd("C:/Users/penng/OneDrive/Documents/IUSB/FA21/Data Mining/WK11")
data <- read.csv("WDBC.csv", header=T)
head(data)

data <- data[,2:11]

data <- scale(data)  # normalizes the data
head(data, 10)

numHneurons3 = 3
numHneurons10 = 10
numHneurons20 = 20
numHneurons50 = 50

DecTargets = decodeClassLabels(data[,10])

train.test3 <- splitForTrainingAndTest(data, DecTargets,ratio = 0.50) # split
train.test10 <- splitForTrainingAndTest(data, DecTargets,ratio = 0.50) # split
train.test20 <- splitForTrainingAndTest(data, DecTargets,ratio = 0.50) # split
train.test50 <- splitForTrainingAndTest(data, DecTargets,ratio = 0.50) # split

model3_02 <- mlp(train.test3$inputsTrain, train.test3$targetsTrain,  # build model3
           size = numHneurons3, learnFuncParams = c(0.02),maxit = 10000, 
           inputsTest = train.test3$inputsTest, 
           targetsTest = train.test3$targetsTest)
model3_08 <- mlp(train.test3$inputsTrain, train.test3$targetsTrain,  # build model3
           size = numHneurons3, learnFuncParams = c(0.08),maxit = 10000, 
           inputsTest = train.test3$inputsTest, 
           targetsTest = train.test3$targetsTest)

model10_02 <- mlp(train.test10$inputsTrain, train.test10$targetsTrain, # build model10
           size = numHneurons10, learnFuncParams = c(0.02),maxit = 10000, 
           inputsTest = train.test10$inputsTest, 
           targetsTest = train.test10$targetsTest)

model10_08 <- mlp(train.test10$inputsTrain, train.test10$targetsTrain, # build model10
           size = numHneurons10, learnFuncParams = c(0.08),maxit = 10000, 
           inputsTest = train.test10$inputsTest, 
           targetsTest = train.test10$targetsTest)

model20_02 <- mlp(train.test20$inputsTrain, train.test20$targetsTrain, # build model20
           size = numHneurons20, learnFuncParams = c(0.02),maxit = 10000, 
           inputsTest = train.test20$inputsTest, 
           targetsTest = train.test20$targetsTest)

model20_08 <- mlp(train.test20$inputsTrain, train.test20$targetsTrain, # build model20
           size = numHneurons20, learnFuncParams = c(0.08),maxit = 10000, 
           inputsTest = train.test20$inputsTest, 
           targetsTest = train.test20$targetsTest)

model50_02 <- mlp(train.test50$inputsTrain, train.test50$targetsTrain, # build model50
           size = numHneurons50, learnFuncParams = c(0.02),maxit = 10000, 
           inputsTest = train.test50$inputsTest, 
           targetsTest = train.test50$targetsTest)

model50_08 <- mlp(train.test50$inputsTrain, train.test50$targetsTrain, # build model50
           size = numHneurons50, learnFuncParams = c(0.08),maxit = 10000, 
           inputsTest = train.test50$inputsTest, 
           targetsTest = train.test50$targetsTest)

trainFitTar3_02 <- cbind(fitted.values(model3_02), train.test3$targetsTrain)
predictions = predict(model3_02, train.test3$inputsTest)

trainFitTar3_08 <- cbind(fitted.values(model3_08), train.test3$targetsTrain)
predictions = predict(model3_08, train.test3$inputsTest)

trainFitTar10_02 <- cbind(fitted.values(model10_02), train.test10$targetsTrain)
predictions = predict(model10_02, train.test10$inputsTest)

trainFitTar10_08 <- cbind(fitted.values(model10_08), train.test10$targetsTrain)
predictions = predict(model10_08, train.test10$inputsTest)

trainFitTar20_02 <- cbind(fitted.values(model20_02), train.test20$targetsTrain)
predictions = predict(model20_02, train.test20$inputsTest)

trainFitTar20_08 <- cbind(fitted.values(model20_08), train.test20$targetsTrain)
predictions = predict(model20_08, train.test20$inputsTest)

trainFitTar50_02 <- cbind(fitted.values(model50_02), train.test50$targetsTrain)
predictions = predict(model50_02, train.test50$inputsTest)

trainFitTar50_08 <- cbind(fitted.values(model50_08), train.test50$targetsTrain)
predictions = predict(model50_08, train.test50$inputsTest)

#--------------------------------------
#     GGPlots of the Iterative Error:
#--------------------------------------

test_error_m302 <- model3_02$IterativeTestError
train_error_m302 <- model3_02$IterativeFitError

test_error_m308 <- model3_08$IterativeTestError
train_error_m308 <- model3_08$IterativeFitError



test_error_m1002 <- model10_02$IterativeTestError
train_error_m1002 <- model10_02$IterativeFitError

test_error_m1008 <- model10_08$IterativeTestError
train_error_m1008 <- model10_08$IterativeFitError



test_error_m2002 <- model20_02$IterativeTestError
train_error_m2002 <- model20_02$IterativeFitError

test_error_m2008 <- model20_08$IterativeTestError
train_error_m2008 <- model20_08$IterativeFitError



test_error_m5002 <- model50_02$IterativeTestError
train_error_m5002 <- model50_02$IterativeFitError

test_error_m5008 <- model50_08$IterativeTestError
train_error_m5008 <- model50_08$IterativeFitError



error_df_m3_02 <- data.frame(iter = c(seq_along(test_error_m302),
                                      seq_along(train_error_m302)),
                             Error = c(test_error_m302, train_error_m302), 
                             type = c(rep("test", length(test_error_m302)),
                                      rep("train", length(train_error_m302))
                             ))


error_df_m3_08 <- data.frame(iter = c(seq_along(test_error_m308),
                                      seq_along(train_error_m308)),
                             Error = c(test_error_m308, train_error_m308), 
                             type = c(rep("test", length(test_error_m308)),
                                      rep("train", length(train_error_m308))
                             ))



error_df_m10_02 <- data.frame(iter = c(seq_along(test_error_m1002),
                                      seq_along(train_error_m1002)),
                             Error = c(test_error_m1002, train_error_m1002), 
                             type = c(rep("test", length(test_error_m1002)),
                                      rep("train", length(train_error_m1002))
                             ))


error_df_m10_08 <- data.frame(iter = c(seq_along(test_error_m1008),
                                      seq_along(train_error_m1008)),
                             Error = c(test_error_m1008, train_error_m1008), 
                             type = c(rep("test", length(test_error_m1008)),
                                      rep("train", length(train_error_m1008))
                             ))



error_df_m20_02 <- data.frame(iter = c(seq_along(test_error_m2002),
                                       seq_along(train_error_m2002)),
                              Error = c(test_error_m2002, train_error_m2002), 
                              type = c(rep("test", length(test_error_m2002)),
                                       rep("train", length(train_error_m2002))
                              ))


error_df_m20_08 <- data.frame(iter = c(seq_along(test_error_m2008),
                                       seq_along(train_error_m2008)),
                              Error = c(test_error_m2008, train_error_m2008), 
                              type = c(rep("test", length(test_error_m2008)),
                                       rep("train", length(train_error_m2008))
                              ))



error_df_m50_02 <- data.frame(iter = c(seq_along(test_error_m5002),
                                       seq_along(train_error_m5002)),
                              Error = c(test_error_m5002, train_error_m5002), 
                              type = c(rep("test", length(test_error_m5002)),
                                       rep("train", length(train_error_m5002))
                              ))


error_df_m50_08 <- data.frame(iter = c(seq_along(test_error_m5008),
                                       seq_along(train_error_m5008)),
                              Error = c(test_error_m5008, train_error_m5008), 
                              type = c(rep("test", length(test_error_m5008)),
                                       rep("train", length(train_error_m5008))
                              ))


plot3_02 <- ggplot(error_df_m3_02[c(5000:10000, 15000:20000),], 
       aes(iter, Error, color = type, 
           each = length(test_error_m302))) + geom_line() + ggtitle("Error M3 Neurons 02")

plot3_08 <- ggplot(error_df_m3_08[c(5000:10000, 15000:20000),], 
       aes(iter, Error, color = type, 
           each = length(test_error_m308))) + geom_line() + ggtitle("Error M3 Neurons 08")

plot10_02 <- ggplot(error_df_m10_02[c(5000:10000, 15000:20000),], 
       aes(iter, Error, color = type, 
           each = length(test_error_m1002))) + geom_line() + ggtitle("Error M10 Neurons 02")

plot10_08 <- ggplot(error_df_m10_08[c(5000:10000, 15000:20000),], 
       aes(iter, Error, color = type, 
           each = length(test_error_m1008))) + geom_line() + ggtitle("Error M10 Neurons 08")

plot20_02 <- ggplot(error_df_m20_08[c(5000:10000, 15000:20000),], 
       aes(iter, Error, color = type, 
           each = length(test_error_m2002))) + geom_line() + ggtitle("Error M20 Neurons 02")

plot20_08 <- ggplot(error_df_m20_08[c(5000:10000, 15000:20000),], 
       aes(iter, Error, color = type, 
           each = length(test_error_m2008)))  + geom_line() + ggtitle("Error M20 Neurons 08")

plot50_02 <- ggplot(error_df_m50_02[c(5000:10000, 15000:20000),], 
       aes(iter, Error, color = type, 
           each = length(test_error_m2002))) + geom_line() + ggtitle("Error M50 Neurons 02")

plot50_08 <- ggplot(error_df_m50_08[c(5000:10000, 15000:20000),], 
       aes(iter, Error, color = type, 
           each = length(test_error_m2008)))  + geom_line() + ggtitle("Error M50 Neurons 08")



title_theme1 <- ggdraw() +
draw_label("ANN Models with 02 Learning Rate", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))

title_theme2 <- ggdraw() +
  draw_label("ANN Models with 08 Learning Rate", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


#plot_grid(plot3_02, plot10_02, plot20_02, plot50_02 + rremove("x.text"),
plot_grid(title_theme1, plot3_02, plot10_02, plot20_02, plot50_02, labels = c("N3", "N10", "N20", "N50"), ncol = 1, rel_heights = c(0.1, 0.9))
plot_grid(title_theme2, plot3_08, plot10_08, plot20_08, plot50_08, labels = c("N3", "N10", "N20", "N50"), ncol = 1, rel_heights = c(0.1, 0.9))


roc3_02 <- plotROC(fitted.values(model3_02), train.test3$targetsTrain) # This will always be perfect
dec3_02 <- plotROC(predictions, train.test3$targetsTest) # Make decisions based on the test.
roc3_08 <- plotROC(fitted.values(model3_08), train.test3$targetsTrain) 
dec3_08 <- plotROC(predictions, train.test3$targetsTest) 

roc10_02 <- plotROC(fitted.values(model10_02), train.test10$targetsTrain) 
dec10_02 <- plotROC(predictions, train.test10$targetsTest)
roc10_08 <- plotROC(fitted.values(model10_08), train.test10$targetsTrain) 
dec10_08 <- plotROC(predictions, train.test10$targetsTest)

roc20_02 <- plotROC(fitted.values(model20_02), train.test10$targetsTrain) 
dec20_02 <- plotROC(predictions, train.test10$targetsTest)
roc20_08 <- plotROC(fitted.values(model20_08), train.test10$targetsTrain) 
dec20_08 <- plotROC(predictions, train.test10$targetsTest)

roc50_02 <- plotROC(fitted.values(model50_02), train.test10$targetsTrain) 
dec50_02 <- plotROC(predictions, train.test10$targetsTest)
roc50_08 <- plotROC(fitted.values(model50_08), train.test10$targetsTrain) 
dec50_08 <- plotROC(predictions, train.test10$targetsTest)


title_theme3 <- ggdraw() +
  draw_label("ROC 3 Neurons - Fit Vals vs. Pred", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


title_theme4 <- ggdraw() +
  draw_label("ROC 3 Neurons - Fit Vals vs. Pred", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


title_theme5 <- ggdraw() +
  draw_label("ROC 10 Neurons - Fit Vals vs. Pred", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


title_theme6 <- ggdraw() +
  draw_label("ROC 10 Neurons - Fit Vals vs. Pred", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


title_theme7 <- ggdraw() +
  draw_label("ROC 20 Neurons - Fit Vals vs. Pred", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


title_theme8 <- ggdraw() +
  draw_label("ROC 20 Neurons - Fit Vals vs. Pred", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


title_theme9 <- ggdraw() +
  draw_label("ROC 50 Neurons - Fit Vals vs. Pred", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


title_theme10 <- ggdraw() +
  draw_label("ROC 50 Neurons - Fit Vals vs. Pred", 
             fontfamily = theme_georgia()$text$family, 
             fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


plot_grid(title_theme3, roc3_02, dec3_02, labels = c("02", "08"), ncol = 2, nrow = 1)
plot_grid(title_theme4, roc3_08, dec3_08, labels = c("02", "08"), ncol = 2, nrow = 1)

plot_grid(title_theme5, roc10_02, dec10_02, labels = c("02", "08"), ncol = 2, nrow = 1)
plot_grid(title_theme6, roc10_08, dec10_08, labels = c("02", "08"), ncol = 2, nrow = 1)

plot_grid(title_theme7, roc20_02, dec20_02, labels = c("02", "08"), ncol = 2, nrow = 1)
plot_grid(title_theme8, roc20_08, dec20_08, labels = c("02", "08"), ncol = 2, nrow = 1)

plot_grid(title_theme9, roc50_02, dec50_02, labels = c("02", "08"), ncol = 2, nrow = 1)
plot_grid(title_theme10, roc50_08, dec50_08, labels = c("02", "08"), ncol = 2, nrow = 1)


