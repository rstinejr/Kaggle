library(plyr)
library(rpart)

cat("Start training session for decision trees.\n")
applicants  <- read.csv("applicants.csv",head=TRUE,sep=",")
totRows <- nrow(applicants)
cat("Applicant data ingested, rows: ", totRows, "\n")

set.seed(2718)
trainSize <- totRows / 2
trainInd  <- sample(seq_len(totRows), size = trainSize)
train     <- applicants[trainInd,]
test      <- applicants[-trainInd,]
cat("\nTraining set extracted, mean ID of training set: ", mean(train$Id), "\n")
cat("                        mean ID of test set    : ",   mean(test$Id),  "\n")
