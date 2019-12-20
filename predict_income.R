#installing/loading packages
library(readr)
install.packages("ggplot2")
library("ggplot2")
#getting the data
Predict_Income<-read.csv(file="C:/Users/ADMIN/Desktop/DSU/Semesters/Fall 2017/INFS 830 Decision Support Systems/Assignment 1/HealthData.csv", header=TRUE, sep=",")
# exploring data
View(Predict_Income)
str(Predict_Income)
head(Predict_Income)
summary(Predict_Income)
#let's add new variable called logIncome
log(Predict_Income$INCOME)
Predict_Income$logIncome <- log(Predict_Income$INCOME)
#check for the new variable logIncome
head(Predict_Income)
str(Predict_Income)
table(Predict_Income$logIncome)
#let now fit regression
##Examine the data before fitting models
##   Start by examining the data to check for problems.
# summary of logIncome, AGE, EDUC, FEMALE, MARRIED, and HHKIDS columns, all rows
sumar <- subset(Predict_Income, select = c("logIncome", "AGE", "EDUC", "FEMALE", "MARRIED", "HHKIDS"))
summary(sumar)
# correlation between logIncome, AGE, EDUC, FEMALE, MARRIED, and HHKIDS
cor(sumar)

##Plot the data before fitting models

##   Plot the data to look for non-linear relationships etc.

# scatter plot of the variables
plot(sumar)

## Linear regression example
##  Linear regression models can be fit with the `lm()' function
##  For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
logIncome.mod <- lm(logIncome ~ AGE + EDUC + FEMALE + MARRIED + HHKIDS, # regression formula
              data=Predict_Income) # data set
# Summarize and print the results
summary(logIncome.mod) # show regression results
#let's now add new column "agesq" to the mode
##first we will need to create a new variable agesq
Predict_Income$agesq <- Predict_Income$AGE*Predict_Income$AGE
table(Predict_Income$agesq)
str(Predict_Income)

cor(subset(Predict_Income, select = c("logIncome", "AGE", "EDUC", "FEMALE", "MARRIED", "HHKIDS", "agesq")))

##let's now add the variable to the model logIncome.mod
logIncome.mod <- lm(logIncome ~ AGE + EDUC + FEMALE + MARRIED + HHKIDS + agesq, # regression formula
                    data=Predict_Income) # data set
summary(logIncome.mod)
#now let's create a new variable FemaleAge and add it to the model
##first, we need to create the variable FemaleAge
Predict_Income$FemaleAge <- Predict_Income$FEMALE*Predict_Income$AGE
table(Predict_Income$FemaleAge)
##now, let's add that to our regression model logIncome
logIncome.mod <- lm(logIncome ~ AGE + EDUC + FEMALE + MARRIED + HHKIDS + FemaleAge, # regression formula
                    data=Predict_Income) # data set
summary(logIncome.mod)

# exploring the phenomenon "moral hazard"
##let's compute a regression of DOCVIS on AGE, EDUC, FEMALE, MARRIED, INCOME, PUBLIC
docvis.mod <- lm(DOCVIS ~ AGE + EDUC + FEMALE + MARRIED + INCOME + PUBLIC, # regression formula 
                 data=Predict_Income) # data set
summary(docvis.mod)
q()
