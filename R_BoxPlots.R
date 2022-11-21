#---
#title: "Title"
#author: "Whitney May"
#date: "Date"
#output: "output type"
#---



library(datasets)
library(ggplot2)
library(plyr)
library(ggpubr)

data(iris)
summary(iris)

# Z score = (x - m)/sdev 
# vars: sWidth, sLength (Sepal width/length)
# vars: pWidth, pLength
# m = mean of sample, in summary(iris)
# sdev = standard deviation


#-----------------------------------------------
#         Sepal Length - All Species
#-----------------------------------------------

# subsetting of data from a data frame can be done as follows
iris.sepalLgth <-iris$Sepal.Length
iris.sepalLgth

meanSepalLength = mean(iris.sepalLgth)
stdevSepalLength = sd(iris.sepalLgth)
meanSepalLength

zSepalLength = (iris.sepalLgth - meanSepalLength) / stdevSepalLength
zSepalLength


#-----------------------------------------------
#         Sepal Width - All Species
#-----------------------------------------------

# subsetting of data from a data frame can be done as follows
iris.sepalWidth <-iris$Sepal.Width
iris.sepalWidth

meanSepalWidth = mean(iris.sepalWidth)
stdevSepalWidth = sd(iris.sepalWidth)
meanSepalWidth

zSepalWidth = (iris.sepalWidth - meanSepalWidth) / stdevSepalWidth
zSepalWidth



#-----------------------------------------------
#         Petal Length - All Species
#-----------------------------------------------

# subsetting of data from a data frame can be done as follows
iris.petalLgth <-iris$Petal.Length
iris.petalLgth

meanPetalLength = mean(iris.petalLgth)
stdevPetalLength = sd(iris.petalLgth)
meanPetalLength

zPetalLength = (iris.petalLgth - meanPetalLength) / stdevPetalLength
zPetalLength


#-----------------------------------------------
#         Petal Width - All Species
#-----------------------------------------------

# subsetting of data from a data frame can be done as follows
iris.petalWidth <-iris$Petal.Width
iris.petalWidth

meanPetalWidth = mean(iris.petalWidth)
stdevPetalWidth = sd(iris.petalWidth)
meanPetalWidth

zPetalWidth = (iris.petalWidth - meanPetalWidth) / stdevPetalWidth
zPetalWidth










#------------------------------------------------------
#     Sepal Width : Setosa, Versicolor, virginica
#------------------------------------------------------

# Split OG iris data into 
vectorSpecies <- iris$Species



#-------------------
#     DataFrame:
#-------------------

zSepalWidthFrame <- data.frame(vectorSpecies, zSepalWidth)


#-------------------
#     GGPlot:
#-------------------


bpSepalWidth = ggplot(zSepalWidthFrame, aes(x=factor(vectorSpecies), y=zSepalWidth, 
                             fill = vectorSpecies)) + geom_boxplot()  

bpSepalWidth 






#------------------------------------------------------
#     Sepal Length : Setosa, Versicolor, virginica
#------------------------------------------------------

# Split OG iris data into 
vectorSpecies <- iris$Species

#-------------------
#     DataFrame:
#-------------------

zSepalLengthFrame <- data.frame(vectorSpecies, zSepalLength)


#-------------------
#     GGPlot:
#-------------------

bpSepalLength = ggplot(zSepalLengthFrame, aes(x=factor(vectorSpecies), y=zSepalLength, 
                                  fill = vectorSpecies)) + geom_boxplot()  

bpSepalLength 



#*****************************************************************************************



#------------------------------------------------------
#     Petal Width : Setosa, Versicolor, virginica
#------------------------------------------------------

# Split OG iris data into 
vectorSpecies <- iris$Species

#-------------------
#     DataFrame:
#-------------------

zPetalWidthFrame <- data.frame(vectorSpecies, zPetalWidth)

#-------------------
#     GGPlot:
#-------------------

bpPetalWidth = ggplot(zPetalWidthFrame, aes(x=factor(vectorSpecies), y=zPetalWidth, 
                                            fill = vectorSpecies)) + geom_boxplot()  

bpPetalWidth 






#------------------------------------------------------
#     Petal Length : Setosa, Versicolor, virginica
#------------------------------------------------------

# Split OG iris data into 
vectorSpecies <- iris$Species

#-------------------
#     DataFrame:
#-------------------

zPetalLengthFrame <- data.frame(vectorSpecies, zPetalLength)

#-------------------
#     GGPlot:
#-------------------

bpPetalLength = ggplot(zPetalLengthFrame, aes(x=factor(vectorSpecies), y=zPetalLength, 
                                              fill = vectorSpecies)) + geom_boxplot()  

bpPetalLength  



#------------------------------------------------------
#            Merge the 4 Box Plots
#------------------------------------------------------


allBoxPlots <- ggarrange(bpPetalLength, bpPetalWidth, bpSepalLength, bpSepalWidth,
                    labels = c("Petal Length", "Petal Width", 
                               "Sepal Length", "Sepal Width"),
                    ncol = 2, nrow = 2)
allBoxPlots




