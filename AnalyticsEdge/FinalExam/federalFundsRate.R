# Final Exam Part 2 
# Federal Funds Rate

# Before start, set working directory.

# 1 -2 
# Load the dataset.
fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors=FALSE)
str(fedFunds)

# What proportion of months did the Fed raise the interest rate?
table(fedFunds$RaisedFedFunds)[2]/nrow(fedFunds)

# Which Federal Reserve Chair has presided over the most interest rate decisions?
table(fedFunds$Chairman)

# 3
# Convert the following variables to factors using the as.factor function:
# Chairman, DemocraticPres, RaisedFedFunds
fedFunds$Chairman <- as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres <- as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds <- as.factor(fedFunds$RaisedFedFunds)
# We convert the outcome variable to a factor for the randomForest() method.

# 4
# Obtain a random training/testing set split
set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)
training <- subset(fedFunds, spl==TRUE)
testing <- subset(fedFunds, spl==FALSE)
# It balances the dependent variable between the training and testing sets.

# 5
# logistic regression

Logmodel <- glm(RaisedFedFunds ~ PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data = training, family = "binomial")
summary(Logmodel)
#You know that the rate has been lowered for 3 straight months (Streak = -3) 
#and that the previous month's rate was 1.7%. The unemployment rate is 5.1% 
#and the homeownership rate is 65.3%. The current U.S. president is a Republican 
#and the next election will be held in 18 months. 
#According to the logistic regression model you built in Problem 5, 
#what is the predicted probability that the interest rate will be raised?

# 9.121012 + 1.7*(-0.003427) - 3* 0.157658 + 5.1*(-0.047449) + 65.3*(-0.136451) + 0*0.347829 + 18*(-0.006931)
sum(as.numeric(Logmodel$coefficients)*c(1,1.7,-3,5.1,65.3,0,18))
# Then you need to plug this into the logistic response function
# to get the predicted probability.

# 6
# The coefficients of the model are the log odds associated with that variable; 
# so we see that the odds of being sold are exp(0.347829)=1.41599 those of 
# an otherwise identical month. This means the month is predicted to have 41.6% 
# higher odds of being sold.

# 8
# Using your logistic regression model, obtain predictions on the test set.
# Then, using a probability threshold of 0.5, create a confusion matrix for the test set.
PredLogTest <- predict(Logmodel, newdata = testing, type="response")
t <- table(testing$RaisedFedFunds, PredLogTest<0.5)
t

tb <- table(testing$RaisedFedFunds)
tb

# On how many test set observations does your logistic regression model make 
# a different prediction than the prediction the naive baseline model would make?

# Obtain test-set predictions with the predict function, 
# remembering to pass type="response". Using table, you can see that 
# there are 91 test-set predictions with probability less than 0.5.

# 9 
# What is the test-set AUC of the logistic regression model?
library(ROCR)
ROCRpred = prediction(PredLogTest, testing$RaisedFedFunds)
#AUC value of test set
as.numeric(performance(ROCRpred, "auc")@y.values)

# 10
# The AUC is the proportion of time the model can differentiate 
# between a randomly selected true positive and true negative.

# 11
# A model with threshold 0 predicts 1 for all observations, 
# yielding a 100% true positive rate and a 100% false positive rate.

# 12
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf, colorize = TRUE)

# CROSS-VALIDATION 
# In 10-fold cross validation, the model with each parameter 
# setting will be trained on 10 90% subsets of the training set. 
# Hence, a total of 20 models will be trained. The models are 
# evaluated in each case on the last 10% of the training set.

# 14
# train function to perform 10-fold cross validation
library(caret)
set.seed(201)
numFolds = trainControl( method = "cv", number = 10 )
# 50 values 0.001, 0.002, ..., 0.05
cpGrid = expand.grid( .cp = seq(0.001,0.05,0.001)) 
tr <- train(RaisedFedFunds ~ PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data=training, method = "rpart", trControl= numFolds, tuneGrid= cpGrid)
tr

# 15
# Build and plot the CART model trained with the cp parameter from CV
CARTmodel <- rpart(RaisedFedFunds ~ PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data=training, cp=0.016)
prp(CARTmodel)

# 17
# Accuracy

PredCARTTest <- predict(CARTmodel, newdata = testing, type="class")
t <- table(testing$RaisedFedFunds, PredCARTTest)
t
(t[1,1] + t[2,2])/nrow(testing)
