library(e1071)
#library(party)
library(plyr)
library(randomForest)
library(rpart)

cat("Start training session for decision trees.\n")
applicants  <- read.csv("applicants.csv",head=TRUE,sep=",")
totRows <- nrow(applicants)
cat("Applicant data ingested, rows: ", totRows, "\n")
applicants$Product_Info_1 <- as.factor(applicants$Product_Info_1)
applicants$Product_Info_2 <- as.factor(applicants$Product_Info_2)
applicants$Product_Info_3 <- as.factor(applicants$Product_Info_3)
applicants$Product_Info_5 <- as.factor(applicants$Product_Info_5)
applicants$Product_Info_6 <- as.factor(applicants$Product_Info_6)
applicants$Product_Info_7 <- as.factor(applicants$Product_Info_7)

set.seed(2718)
trainSize <- totRows / 2
trainInd  <- sample(seq_len(totRows), size = trainSize)
train     <- applicants[trainInd,]
test      <- applicants[-trainInd,]
cat("\nTraining set extracted, mean ID of training set: ", mean(train$Id), "\n")
cat("                        mean ID of test set    : ",   mean(test$Id),  "\n")


fol1 <- formula(Response ~ Product_Info_1 + Product_Info_2 + Product_Info_3 + Product_Info_4
                          + Product_Info_5 + Product_Info_6 + Product_Info_7)
#fit1 <- ctree(fol1, train)
#cat("ctree based on product info:\n")
#fit1

#model1 <- rpart(fol1, method="class", data=train)
#cat("\nDecision tree for product type:\n")
#model1

prod_rf = randomForest(fol1, train)
cat("\nimportance of Product_Info fields:\n")
importance(prod_rf)

fol2   <- formula(Response ~ Ins_Age, Ht, Wt, BMI)

#cat("ctree based on age/weight:\n")
#fit2 <- ctree(fol2, train)
#fit2

#model2 <- rpart(fol2, method="class", data=train)
#cat("\nDecision tree for age, weight:\n")
#model2

cat("importance, age/wt:\n")
age_rf <- randomForest(fol2, train)
importance(age_rf)
