library(plyr)
library(rpart)

cat("Start training session for decision trees.\n")
applicants  <- read.csv("applicants.csv",head=TRUE,sep=",")
totRows <- nrow(applicants)
cat("Applicant data ingested, rows: ", totRows, "\n")

applicants$Product_Info_4_cat <- cut(applicants$Product_Info_4, 5)

applicants$Ins_Age_cat <- cut(applicants$Ins_Age, 8)
applicants$Ht_cat      <- cut(applicants$Ht, 4)
applicants$Wt_cat      <- cut(applicants$Wt, 8)
applicants$BMI_cat     <- cut(applicants$BMI, 4)

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

fol1 <- formula(Response ~ Product_Info_1 + Product_Info_2 + Product_Info_3 + Product_Info_4_cat 
                          + Product_Info_5 + Product_Info_6 + Product_Info_7)
model1 <- rpart(fol1, method="class", data=train)
cat("\nDecision tree for product type:\n")
model1

fol2   <- formula(Response ~ Ins_Age_cat, Ht_cat, Wt_cat, BMI_cat)
model2 <- rpart(fol2, method="class", data=train)
cat("\nDecision tree for age, weight:\n")
model2

