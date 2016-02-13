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
r1 <- applicants[applicants$Response == 1,]
r2 <- applicants[applicants$Response == 2,]
r3 <- applicants[applicants$Response == 3,]
r4 <- applicants[applicants$Response == 4,]
r5 <- applicants[applicants$Response == 5,]
r6 <- applicants[applicants$Response == 6,]
r6 <- applicants[applicants$Response == 7,]
r8 <- applicants[applicants$Response == 8,]
