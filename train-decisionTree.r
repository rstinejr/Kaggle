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
applicants$InsuredInfo_7 <- as.factor(applicants$InsuredInfo_7)

applicants$Insurance_History_1 <- as.factor(applicants$Insurance_History_1)
applicants$Insurance_History_2 <- as.factor(applicants$Insurance_History_2)
applicants$Insurance_History_3 <- as.factor(applicants$Insurance_History_3)
applicants$Insurance_History_4 <- as.factor(applicants$Insurance_History_4)
applicants$Insurance_History_7 <- as.factor(applicants$Insurance_History_7)
applicants$Insurance_History_8 <- as.factor(applicants$Insurance_History_8)
applicants$Insurance_History_9 <- as.factor(applicants$Insurance_History_9)

applicants$Family_Hist_1 <- as.factor(applicants$Family_Hist_1)

applicants$Medical_History_3 <- as.factor(applicants$Medical_History_3)
applicants$Medical_History_4 <- as.factor(applicants$Medical_History_4)
applicants$Medical_History_5 <- as.factor(applicants$Medical_History_5)
applicants$Medical_History_6 <- as.factor(applicants$Medical_History_6)
applicants$Medical_History_7 <- as.factor(applicants$Medical_History_7)
applicants$Medical_History_8 <- as.factor(applicants$Medical_History_8)
applicants$Medical_History_9 <- as.factor(applicants$Medical_History_9)

applicants$Medical_History_11 <- as.factor(applicants$Medical_History_11)
applicants$Medical_History_12 <- as.factor(applicants$Medical_History_12)
applicants$Medical_History_13 <- as.factor(applicants$Medical_History_13)
applicants$Medical_History_14 <- as.factor(applicants$Medical_History_14)
applicants$Medical_History_16 <- as.factor(applicants$Medical_History_16)
applicants$Medical_History_17 <- as.factor(applicants$Medical_History_17)
applicants$Medical_History_18 <- as.factor(applicants$Medical_History_18)
applicants$Medical_History_19 <- as.factor(applicants$Medical_History_19)

applicants$Medical_History_20 <- as.factor(applicants$Medical_History_20)
applicants$Medical_History_21 <- as.factor(applicants$Medical_History_21)
applicants$Medical_History_22 <- as.factor(applicants$Medical_History_22)
applicants$Medical_History_23 <- as.factor(applicants$Medical_History_23)
applicants$Medical_History_25 <- as.factor(applicants$Medical_History_25)
applicants$Medical_History_26 <- as.factor(applicants$Medical_History_26)
applicants$Medical_History_27 <- as.factor(applicants$Medical_History_27)
applicants$Medical_History_28 <- as.factor(applicants$Medical_History_28)
applicants$Medical_History_29 <- as.factor(applicants$Medical_History_29)

applicants$Medical_History_30 <- as.factor(applicants$Medical_History_30)
applicants$Medical_History_31 <- as.factor(applicants$Medical_History_31)
applicants$Medical_History_33 <- as.factor(applicants$Medical_History_33)
applicants$Medical_History_34 <- as.factor(applicants$Medical_History_34)
applicants$Medical_History_35 <- as.factor(applicants$Medical_History_35)
applicants$Medical_History_36 <- as.factor(applicants$Medical_History_36)
applicants$Medical_History_37 <- as.factor(applicants$Medical_History_37)
applicants$Medical_History_38 <- as.factor(applicants$Medical_History_38)
applicants$Medical_History_39 <- as.factor(applicants$Medical_History_39)

applicants$Medical_History_40 <- as.factor(applicants$Medical_History_40)
applicants$Medical_History_41 <- as.factor(applicants$Medical_History_41)

set.seed(2718)
trainSize <- totRows / 2
trainInd  <- sample(seq_len(totRows), size = trainSize)
train     <- applicants[trainInd,]
test      <- applicants[-trainInd,]
cat("\nTraining set extracted, mean ID of training set: ", mean(train$Id), "\n")
cat("                        mean ID of test set    : ",   mean(test$Id),  "\n")

if (FALSE)
{
fol1 <- formula(Response ~ Product_Info_1 + Product_Info_2 + Product_Info_3 + Product_Info_4
                          + Product_Info_5 + Product_Info_6 + Product_Info_7)
#fit1 <- ctree(fol1, train)
#cat("ctree based on product info:\n")
#fit1

#model1 <- rpart(fol1, method="class", data=train)
#cat("\nDecision tree for product type:\n")
#model1
#cat("\nprint model1:\n")
#print(model1)

prod_rf = randomForest(fol1, train)
cat("\nimportance of Group 1, Product_Info:\n")
importance(prod_rf)

fol2 <- formula(Response ~ Ins_Age + Ht + Wt +  BMI)

#model2 <- rpart(fol2, method="class", data=train)
#cat("\nDecision tree for age, weight:\n")
#model2

cat("importance, Group 2, age/wt/BMI:\n")
age_rf <- randomForest(fol2, train)
importance(age_rf)

fol3 <- formula(Response ~ Employment_Info_1 + Employment_Info_2 + Employment_Info_3 + Employment_Info_4 +
                           Employment_Info_5 + Employment_Info_6)
emp_rf <- randomForest(fol3, train, na.action = na.omit)
cat("\nImportance of Group 3, Employment_Info:\n")
importance(emp_rf)

fol4 <- formula(Response ~ InsuredInfo_1 + InsuredInfo_2 + InsuredInfo_3 + InsuredInfo_4 +
                           InsuredInfo_5 + InsuredInfo_6 +  InsuredInfo_7)
ins_rf <- randomForest(fol4, train)
cat("\nImportance of Group 4, InsuredInfo:\n")
importance(ins_rf)

fol5 <- formula(Response ~ Insurance_History_1 + Insurance_History_2 + Insurance_History_3 + Insurance_History_4 +
                           Insurance_History_5 + Insurance_History_7 + Insurance_History_8 + Insurance_History_9)
ins_hist_rf <- randomForest(fol5, train, na.action = na.omit)
cat("\nImportance of Group 5, Insurance_History:\n")
importance(ins_hist_rf)

fol6 <- formula(Response ~ Family_Hist_1 + Family_Hist_2 + Family_Hist_4)
ins_hist_rf <- randomForest(fol6, train, na.action = na.omit)
cat("\nImportance of Group 6, Family_Hist:\n")
importance(ins_hist_rf)

fol7 <- formula(Response ~ Medical_History_1 + Medical_History_2 + Medical_History_3 + Medical_History_4 +
                           Medical_History_5 + Medical_History_6 + Medical_History_7 + Medical_History_8 +
                           Medical_History_9)
med_hist_rf <- randomForest(fol7, train, na.action = na.omit)
cat("\nImportance of Group 7, Medical_History 1 - 9:\n")
importance(med_hist_rf)

fol8 <- formula(Response ~ Medical_History_10 + Medical_History_11 + Medical_History_12 + Medical_History_13 +
                           Medical_History_14 + Medical_History_15 + Medical_History_16 + Medical_History_17 +
                           Medical_History_18 + Medical_History_19)
med_hist_rf <- randomForest(fol8, train, na.action = na.omit)
cat("\nImportance of Group 8, Medical_History 10 - 19:\n")
importance(med_hist_rf)

fol9 <- formula(Response ~ Medical_History_20 + Medical_History_21 + Medical_History_22 + Medical_History_23 +
                           Medical_History_24 + Medical_History_25 + Medical_History_26 + Medical_History_27 +
                           Medical_History_28 + Medical_History_29)
med_hist_rf <- randomForest(fol9, train, na.action = na.omit)
cat("\nImportance of Group 9, Medical_History 20 - 29:\n")
importance(med_hist_rf)

fol10 <- formula(Response ~ Medical_History_30 + Medical_History_31 + Medical_History_32 + Medical_History_33 +
                            Medical_History_34 + Medical_History_35 + Medical_History_36 + Medical_History_37 +
                            Medical_History_38 + Medical_History_39 + Medical_History_40 + Medical_History_41)
med_hist_rf <- randomForest(fol10, train, na.action = na.omit)
cat("\nImportance of Group 10, Medical_History 30 - 41:\n")
importance(med_hist_rf)
}

fol <- formula(Response ~ Product_Info_2 + Product_Info_3 + Product_Info_4 + Ins_Age + Ht +
	                      Wt + BMI + Employment_Info_1 + Employment_Info_2 + Employment_Info_3 +
	                      Employment_Info_4 + Employment_Info_5 + Employment_Info_6 +
	                      InsuredInfo_1 + InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + Insurance_History_2 +
	                      Insurance_History_5 + Medical_History_1 + Medical_History_2 + Medical_History_4 +
	                      Medical_History_6)

rf <- randomForest(fol, train, na.action = na.omit)
cat("\nImportance of high-impact factors:\n")
importance(rf)
