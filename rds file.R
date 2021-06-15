##here we are importing the already developed model which is read.model
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

Model.training <-predict(read.Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(read.Model, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
