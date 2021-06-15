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

# Random forest


# Run normally without parallel processing
start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf" # Learning algorithm
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)
# Use doParallel,for faster process,runtime will be less
# https://topepo.github.io/caret/parallel-processing.html

library(doParallel)

cl <- makePSOCKcluster(5) #make clusters of 5 for the training dataset
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf" # Learning algorithm
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl) ##is used to freeup resources
##by doing doparallel ,the runtime is only 39.36sec

##object.size("dfhr")

##hyperparameter tuning

# Run without parallel processing

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf", # Learning algorithm
               tuneGrid = data.frame(mtry = seq(5,15, by=5))
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

# Using doParallel,doparallel makes diff clusters of data and run parallely,its a backened package

library(doParallel)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf", # Learning algorithm
               tuneGrid = data.frame(mtry = seq(5,15, by=5))
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)
# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)

print(Model.training.confusion)

# Feature importance
Importance <- varImp(Model)

plot(Importance, col = "red")
plot(Importance, top = 25)
