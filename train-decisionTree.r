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

applicants$Employment_Info_2 <- as.factor(applicants$Employment_Info_2)
applicants$Employment_Info_3 <- as.factor(applicants$Employment_Info_3)
applicants$Employment_Info_5 <- as.factor(applicants$Employment_Info_5)

applicants$InsuredInfo_1 <- as.factor(applicants$InsuredInfo_1)
applicants$InsuredInfo_2 <- as.factor(applicants$InsuredInfo_2)
applicants$InsuredInfo_3 <- as.factor(applicants$InsuredInfo_3)
applicants$InsuredInfo_4 <- as.factor(applicants$InsuredInfo_4)
applicants$InsuredInfo_5 <- as.factor(applicants$InsuredInfo_5)
applicants$InsuredInfo_6 <- as.factor(applicants$InsuredInfo_6)

set.seed(2718)
trainSize <- totRows / 2
trainInd  <- sample(seq_len(totRows), size = trainSize)
train     <- applicants[trainInd,]
test      <- applicants[-trainInd,]
cat("\nTraining set extracted, mean ID of training set: ", mean(train$Id), "\n")
cat("                        mean ID of test set    : ",   mean(test$Id),  "\n")


#fol1 <- formula(Response ~ Product_Info_1 + Product_Info_2 + Product_Info_3 + Product_Info_4
#                          + Product_Info_5 + Product_Info_6 + Product_Info_7)
#fit1 <- ctree(fol1, train)
#cat("ctree based on product info:\n")
#fit1

#model1 <- rpart(fol1, method="class", data=train)
#cat("\nDecision tree for product type:\n")
#model1

#prod_rf = randomForest(fol1, train)
#cat("\nimportance of Product_Info fields:\n")
#importance(prod_rf)

#fol2   <- formula(Response ~ Ins_Age, Ht, Wt, BMI)

#cat("ctree based on age/weight:\n")
#fit2 <- ctree(fol2, train)
#fit2

#model2 <- rpart(fol2, method="class", data=train)
#cat("\nDecision tree for age, weight:\n")
#model2

#cat("importance, age/wt:\n")
#age_rf <- randomForest(fol2, train)
#importance(age_rf)

#fol3 <- formula(Response ~ Employment_Info_1, Employment_Info_2, Employment_Info_3, Employment_Info_4,
#                           Employment_Info_5, Employment_Info_6, Employment_Info_7)
#emp_rf <- randomForest(fol3, train, na.action = na.omit)
#cat("\nImportance of Employment_Info:\n")
#importance(emp_rf)

fol4 <- formula(Response ~ InsuredInfo_1, InsuredInfo_2, InsuredInfo_3, InsuredInfo_4,
                           InsuredInfo_5, InsuredInfo_6, InsuredInfo_7)
ins_rf <- randomForest(fol4, train)
cat("\nImportance of InsuredInfo:\n")
importance(ins_rf)
