#import datasets(2 methods)

#method 1
# Importing libraries
library(datasets) # Contains the Iris data set
# Importing the Iris data set
data(iris)

#method 2
install.packages("RCurl")##for giving dataset links
library(RCurl)
#iris<-read.csv(text=getURL("https://.....")) ##github link
View(iris)

#head(),tail()
head(iris,5)
tail(iris,5)

#summary
summary((iris))
summary(iris$Sepal.Length)

# Check to see if there are missing data?
sum(is.na(iris))
sum(is.na(iris$Sepal.Length))

##skimr()-like summary function used to get larger set of statistics
install.packages(("skimr"))
library(skimr)

skim(iris) ##to skim,to show more ststistics,total rows,columns etc

#group data by species,then perform skin
iris %>%
  dplyr::group_by(Species) %>%
  skim()

##data visualization

#panel plots, or like  pairplot
plot(iris)
plot(iris,col="red")#for red color

#scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)
plot(iris$Sepal.Length,iris$Sepal.Width,col="red")

#histogram
hist(iris$Sepal.Length)
hist(iris$Sepal.Length,col="yellow")

#feature plots,or box plots for each species,here as output we get 4 diff graphs where each feature(sep length,..)are in y and species name(all 3) in x axis

featurePlot(x = iris[,1:4], 
            y = iris$Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

# Performs stratified random split of the data set
#install.packages("caret"),installed for createDataPartition
#library("caret")
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
##random selection of rows for train and test,p denotes 80% belongs to train
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

#scatterplot
plot(TrainingSet$Sepal.Length,TrainingSet$Sepal.Width,col="red")
plot(TestingSet$Sepal.Length,TestingSet$Sepal.Width,col="red")

# SVM model (polynomial kernel),support vector machines

# Build Training model
##install.packages("e1071")
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly", ##method="lm" for linear regression
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation


# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")

##dhfr dataset dhfr-dihydrofolate reductase
library(datasets)
data(dhfr)

head(dhfr,5)
summary(dhfr)
sum(is.na(dhfr))
skim(dhfr) #to display summary statistics
#can do every step we did for iris dataset

# To achieve reproducible model; set the random seed number
set.seed(100)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,] # Training Set
TestingSet <- dhfr[-TrainingIndex,] # Test Set


# SVM model (polynomial kernel)

# Build Training model
Model <- train(Y ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)
#if you want you can do cross validation

# Save model to RDS file

saveRDS(Model, "Model.rds")

# Read the model from RDS file

read.Model <- readRDS("Model.rds")
# Apply model for prediction
Model.training <-predict(read.Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(read.Model, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)

# Feature importance
Importance <- varImp(Model)

plot(Importance, col = "red")
plot(Importance, top = 25)

