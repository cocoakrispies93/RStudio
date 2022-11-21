library("neuralnet")
library("ISLR") 

data(iris)
iris.dataset <- iris

iris.dataset$setosa <- iris.dataset$Species=="setosa"
iris.dataset$virginica = iris.dataset$Species == "virginica"
iris.dataset$versicolor = iris.dataset$Species == "versicolor"

train <- sample(x = nrow(iris.dataset), size = nrow(iris)*0.5)
train
iristrain <- iris.dataset[train,]
irisvalid <- iris.dataset[-train,]

print(nrow(iristrain))
print(nrow(irisvalid))

nn <- neuralnet(setosa+versicolor+virginica ~ Sepal.Length 
                + Sepal.Width, data=iristrain, hidden=3,  
                rep = 2, err.fct = "ce", linear.output = F, 
                lifesign = "minimal", stepmax = 10000000)

plot(nn, rep="best")

head(irisvalid)
dim(irisvalid)

comp <- compute(nn, irisvalid[-3:-8])
pred.weights <- comp$net.result

idx <- apply(pred.weights, 1, which.max)
pred <- c('setosa', 'versicolor', 'virginica')[idx]

table(pred, irisvalid$Species)