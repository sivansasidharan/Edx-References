# Final Exam Part 1 
# Box-office 

# Before start, set working directory.

# 1
# Load the dataset.
# Create a training set that consists of movies released before 2010 
# and a testing set that consists of movies released in 2010 and after.
# How many observations are in the training set MoviesTrain?

Movies <- read.csv('Movies.csv')
MoviesTrain <- subset(Movies, Movies$Year < 2010)
MoviesTest <- subset(Movies, Movies$Year >= 2010)

nrow(MoviesTrain)
nrow(MoviesTest)

# 2
# The sample.split function is used to split data for a classification 
# problem (a categorical dependent variable) and we have a continuous 
# dependent variable here. We also do not wish to split our data randomly.

# 3
# Build a linear regression model to predict "Worldwide" based on all 
# of the remaining variables, except for Name and Year.
# What is the model's R-squared?

fit1 <- lm(Worldwide ~ .-Name-Year, data=MoviesTrain)
summary(fit1)

# 4
# The significance can be found by looking at the summary output.

# 5
# What is the correlation between Worldwide and Production.Budget 
# in the training set?

cor(MoviesTrain$Worldwide, MoviesTrain$Production.Budget)
# A strong correlation between an independent variable and the dependent 
# variable suggests high predictive value.

# 6-7
# Create a new linear regression model on the training set with only 
# the significant variables 
# What is the model's R-squared?

fit2 <- lm(Worldwide ~ Runtime+Crime+Horror+Animation+History+Nominations+Production.Budget, data=MoviesTrain)
summary(fit2)
# The coefficient should be interpreted as the change in the predicted 
# value of the dependent variable associated with a one unit change in 
# the independent variable. We cannot interpret the coefficient to imply 
# causality about the relationship between Runtime and Worldwide.

# 8
# Make predictions on the test set using your linear regression model. 
predictTest <- predict(fit2, newdata = MoviesTest)

# What is the Sum of Squared Errors (SSE) on the test set?
SSE <- sum((MoviesTest$Worldwide - predictTest)^2)
SSE

# What is the Total Sum of Squares (SST) on the test set? 
SST <- sum((MoviesTest$Worldwide - mean(Movies$Worldwide))^2)
SST

# What is the R-squared on the test set?
1 - SSE/SST

# 9
# Overfitting?
# The R-squared value on the test set is higher 
# than on the training set, so there is no overfitting going on here.

# 10
# Let's turn this problem into a multi-class classification

Movies$Performance <- factor(ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .75), "Excellent", ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .25), "Average", "Poor")))

# How many movies in the dataset Movies have Performance value Average and Excellent?
table(Movies$Performance)

# Remove the original dependent andrandomly split Movies into a training 
# set, containing 70% of the observations
Movies$Worldwide = NULL
library(caTools)
set.seed(15071)
spl <- sample.split(Movies$Performance, SplitRatio = 0.7)
MoviesTrain <- subset(Movies, spl==TRUE)
MoviesTest <- subset(Movies, spl==FALSE)

# 11
# Build a CART model to predict "Performance" using all of the other variables 
# except for "Name and "Year"
library(rpart)
library(rpart.plot)

summary(MoviesTrain)
str(MoviesTrain)
CARTmodel <- rpart(Performance ~ ., data=MoviesTrain[ , 3:ncol(MoviesTrain)], method="class")
prp(CARTmodel)

# What is the overall accuracy of the model?
PredTrain <- predict(CARTmodel, type = "class")
t <- table(MoviesTrain$Performance, PredTrain)
t
(t[1,1] + t[2,2] + t[3,3])/nrow(MoviesTrain)

# 12
# What is the accuracy on the training set of a baseline model that predicts 
# the most frequent outcome (Average) for all observations?
table(MoviesTrain$Performance)
116/(nrow(MoviesTrain))

# 13
# What is the overall accuracy of the model on the testing set?
PredCARTTest <- predict(CARTmodel, newdata = MoviesTest, type = "class")
t <- table(MoviesTest$Performance, PredCARTTest)
t
(t[1,1] + t[2,2] + t[3,3])/nrow(MoviesTest)

# 14 
# What is the accuracy on the testing set of a baseline model that predicts
# the most frequent outcome (Average) for all observations?
table(MoviesTest$Performance)
50/(nrow(MoviesTest))

# 15
# Both models performed much better than 
# corresponding baseline models on the testing set.