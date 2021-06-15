
rm(list=ls()) # removes all variables stored previously
library(Hmisc) # import
data <- read.csv("~/DATASCIENCE/covid 19 R/datasets_494724_994000_COVID19_line_list_data (1).csv")
describe(data) # Hmisc command
# cleaned up death column,in death column some columns are wrongly provided,it has0,1 amd some place date is given,so its cleaned
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

# AGE
# claim: people who die are older
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
##na.rm==for those age columns with na,we have to remove that.otherwise it woud give na as the result
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
#the p value in the console shows that 2.2e-16 ie,approx 0 chance that null hypothesis is true or difference between the alive and dead is true
##hence result is statiscally significant
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%!
mean(women$death_dummy, na.rm = TRUE) #3.7%
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant

