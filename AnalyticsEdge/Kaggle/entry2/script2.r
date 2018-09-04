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
# train$productline <- eBayTrain$productline
train$sold <- eBayTrain$sold
train$UniqueID <- eBayTrain$UniqueID
train$WordCount = eBayTrain$WordCount
str(train)

# Test Data
test$biddable <- eBayTest$biddable
test$startprice <- eBayTest$startprice
test$condition <- as.factor(eBayTest$condition)
test$cellular <- as.factor(eBayTest$cellular)
test$carrier <- as.factor(eBayTest$carrier)
test$color  <- as.factor(eBayTest$color)
test$storage <- as.numeric(eBayTest$storage)
# test$productline <- eBayTest$productline
test$UniqueID <- eBayTest$UniqueID
test$WordCount = eBayTest$WordCount

str(eBayTest)

# Missing values
library(mice)
set.seed(144)
train <-  complete(mice(train))
test <-  complete(mice(test))

# Clustering
###############################
# Clustering
limitedTrain = train
limitedTrain$sold = NULL

# normalizing
library(caret)
str(train)
preprTrain <- subset(train, select=-c(sold, color, carrier, condition, cellular))
preprTest <- subset(test, select=-c(color, carrier, condition, cellular))
preproc <- preProcess(preprTrain)
normTrain <- predict(preproc, preprTrain)
normTest <- predict(preproc, preprTest)

summary(normTrain)
summary(normTest)

normTrain$sold <- train$sold
normTrain$color <- train$color
normTrain$condition <- train$condition
normTrain$carrier <- train$carrier
normTrain$cellular <- train$cellular

normTest$color <- test$color
normTest$condition <- test$condition
normTest$carrier <- test$carrier
normTest$cellular <- test$cellular

##########################
# clustering
set.seed(144)
distances <- dist(normTrain, method="euclidian")
Hcluster <- hclust(distances, method="ward.D")
plot(Hcluster)
trainClusters = cutree(Hcluster, k = 2)
table(trainClusters)

set.seed(144)
km <- kmeans(normTrain, 2)
table(km$cluster)

# predicting 
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

#  CLUSTER-SPECIFIC PREDICTIONS
eBayTrain1 <- subset(train, clusterTrain == 1)
eBayTrain2 <- subset(train, clusterTrain == 2)

eBayTest1 <- subset(test, clusterTest == 1)
eBayTest2 <- subset(test, clusterTest == 2)


mean(eBayTrain1$sold) 
mean(eBayTrain2$sold)

# model Log
LogClustModel1 <- glm(sold ~ ., data=eBayTrain1, family="binomial")
LogClustModel2 <- glm(sold ~ ., data=eBayTrain2, family="binomial")

PredictTrain1 = predict(LogClustModel1, newdata= eBayTrain1, type="response")
PredictTrain2 = predict(LogClustModel2, newdata= eBayTrain2, type="response")
# acc
ct1 = table(eBayTrain1$sold, PredictTrain1>=0.5)
(ct1[1,1]+ct1[2,2])/nrow(eBayTrain1)
ct2 = table(eBayTrain2$sold, PredictTrain2>=0.5)
(ct2[1,1]+ct2[2,2])/nrow(eBayTrain2)


AllPredictions = c(PredictTrain1, PredictTrain2)
AllOutcomes = c(eBayTrain1$sold, eBayTrain2$sold)
ct = table(AllOutcomes, AllPredictions>=0.5)
(ct[1,1]+ct[2,2])/length(AllPredictions)

# model CART
library(rpart)
library(rpart.plot)

CARTClustModel1 <- rpart(sold ~ ., data=eBayTrain1, method="class")
CARTClustModel2 <- rpart(sold ~ ., data=eBayTrain2, method="class")

CARTPredictTrain1 = predict(CARTClustModel1, newdata=eBayTrain1, type="class")
CARTPredictTrain2 = predict(CARTClustModel2, newdata=eBayTrain2, type="class")
# acc
ct1 = table(eBayTrain1$sold, CARTPredictTrain1)
(ct1[1,1]+ct1[2,2])/nrow(eBayTrain1)
ct2 = table(eBayTrain2$sold, CARTPredictTrain2)
(ct2[1,1]+ct2[2,2])/nrow(eBayTrain2)


AllPredictions = c(CARTPredictTrain1, CARTPredictTrain2)
AllOutcomes = c(eBayTrain1$sold, eBayTrain2$sold)
ct = table(AllOutcomes, AllPredictions)
(ct[1,1]+ct[2,2])/length(AllPredictions)

# model RF
library(randomForest)
set.seed(144)
RFClustModel1 <- randomForest(sold ~ ., data=eBayTrain1, method="class")
RFClustModel2 <- randomForest(sold ~ ., data=eBayTrain2, method="class")

RFPredictTrain1 = predict(RFClustModel1, newdata=eBayTrain1, type="class")
RFPredictTrain2 = predict(RFClustModel2, newdata=eBayTrain2, type="class")
# acc
ct1 = table(eBayTrain1$sold, RFPredictTrain1>0.5)
(ct1[1,1]+ct1[2,2])/nrow(eBayTrain1)
ct2 = table(eBayTrain2$sold, RFPredictTrain2>0.5)
(ct2[1,1]+ct2[2,2])/nrow(eBayTrain2)


AllPredictions = c(RFPredictTrain1, RFPredictTrain2)
AllOutcomes = c(eBayTrain1$sold, eBayTrain2$sold)
ct = table(AllOutcomes, AllPredictions>0.5)
(ct[1,1]+ct[2,2])/length(AllPredictions)
############################

# Predictions on testing set with RANDOM FOREST
RFPredictTest1 = predict(RFClustModel1, newdata=eBayTest1, type="class")
RFPredictTest2 = predict(RFClustModel2, newdata=eBayTest2, type="class")

######### SUBMISSION
MySubmission1 = data.frame(UniqueID = eBayTest1$UniqueID, Probability1 = RFPredictTest1)
MySubmission2 = data.frame(UniqueID = eBayTest2$UniqueID, Probability1 = RFPredictTest2)
MySubmission = rbind(MySubmission1, MySubmission2)
str(MySubmission)
head(MySubmission)
MySubmission <- MySubmission[order(MySubmission$UniqueID) , ]

write.csv(MySubmission, "submission.csv", row.names=FALSE)