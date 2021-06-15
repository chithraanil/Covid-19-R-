##checking for missing data
library(datasets)
data(dhfr)

head(dhfr,5)
summary(dhfr)
sum(is.na(dhfr))
skim(dhfr)

sum(is.na(dhfr))
 ##if there is any 'na'
# 1. Simply delete all entries with missing data

clean.data <- na.omit(dhfr)

#2. MEAN
dhfr.impute <- dhfr ##clone or duplicate of the dataset
#below code gives the mean at the na position

for (i in which(sapply(dhfr.impute, is.numeric))) { 
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- mean(dhfr.impute[, i],  na.rm = TRUE) 
}

sum(is.na(dhfr.impute))


#. MEDIAN
dhfr.impute <- dhfr

for (i in which(sapply(dhfr.impute, is.numeric))) { 
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- median(dhfr.impute[, i],  na.rm = TRUE) 
}

sum(is.na(dhfr.impute))
