# loading data. stringsAsFactors=FALSE, since we have some text fields
eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

# Dealing with text data.
library(tm)
library(SnowballC)

# 1) Build a new corpus variable called CorpusDescription.
CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))
#writeLines(as.character(CorpusDescription[[1]]))
# 2) Using tm_map, convert the text to lowercase.
CorpusDescription <- tm_map(CorpusDescription, tolower)
# Remember this extra line is needed after running the tolower step:
CorpusDescription <- tm_map(CorpusDescription, PlainTextDocument)
# 3) Using tm_map, remove all punctuation from the corpus.
CorpusDescription <- tm_map(CorpusDescription, removePunctuation)
# 4) Using tm_map, remove all English stopwords from the corpus.
CorpusDescription <- tm_map(CorpusDescription, removeWords, stopwords("english"))
# 5) Using tm_map, stem the words in the corpus.
CorpusDescription <- tm_map(CorpusDescription, stemDocument)
#6) Build a document term matrix from the corpus, called dtm.
dtm <- DocumentTermMatrix(CorpusDescription)
dtm
# Remove sparse words
spdtm <- removeSparseTerms(dtm, 0.99)
spdtm

# Creat DATA FRAME
DescriptionWords <- as.data.frame(as.matrix(spdtm))
colnames(DescriptionWords) <- make.names(colnames(DescriptionWords))

# Split the observations back into the training set and testing set.
train = head(DescriptionWords, nrow(eBayTrain))
test = tail(DescriptionWords, nrow(eBayTest))

# add back the original variables from our datasets. 
str(eBayTrain)
# Train Data
train$biddable <- eBayTrain$biddable
train$startprice <- eBayTrain$startprice
train$condition <- as.factor(eBayTrain$condition)
train$cellular <- as.factor(eBayTrain$cellular)
train$carrier <- as.factor(eBayTrain$carrier)
train$color  <- as.factor(eBayTrain$color )
train$storage <- as.numeric(eBayTrain$storage)
# train$productline <- eBayTrain$biddable
train$sold <- eBayTrain$sold
train$UniqueID <- eBayTrain$UniqueID
train$WordCount = eBayTrain$WordCount

str(train)

# Test Data
str(eBayTest)
test$biddable <- eBayTest$biddable
test$startprice <- eBayTest$startprice
test$condition <- as.factor(eBayTest$condition)
test$cellular <- as.factor(eBayTest$cellular)
test$carrier <- as.factor(eBayTest$carrier)
test$color  <- as.factor(eBayTest$color)
test$storage <- as.numeric(eBayTest$storage)
# test$productline <- eBayTest$biddable
test$UniqueID <- eBayTest$UniqueID
test$WordCount = eBayTest$WordCount

# Missing values
library(mice)
set.seed(144)

train <-  complete(mice(train))
test <-  complete(mice(test))

# Building model

# Log model
Logmodel1 = glm(sold ~ ., data=train, family="binomial")
# And make predictions on our test set:
predictLog = predict(Logmodel1, newdata=train, type="response")
t <- table(train$sold, predictLog > 0.5)
t
(t[1] + t[4]) / nrow(train)

# CART model
library(rpart)
library(rpart.plot)

CARTmodel1 = rpart(sold ~ ., data=train, method="class")
prp(CARTmodel1)
# Evaluate the performance of the model
predictCART = predict(CARTmodel1, newdata=train, type="class")
t <- table(train$sold, predictCART)
t
(t[1] + t[4]) / nrow(train)

# Random forest

library(randomForest)

RFmodel1 <- randomForest(sold ~ ., data=train, method="class")
# Evaluate the performance of the model
predictRF <- predict(RFmodel1, newdata=train, type="class")
t <- table(train$sold, predictRF>0.5)
t
(t[1] + t[4]) / nrow(train)

# Test predictions
testpredictRF <- predict(RFmodel1, newdata=test, type="class")
###E
MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = testpredictRF)
str(MySubmission)
write.csv(MySubmission, "submission.csv", row.names=FALSE)