cat("Summary of applicant data")
applicants  <- read.csv("applicants.csv",head=TRUE,sep=",")
applicants$Product_Info_1 <- as.factor(applicants$Product_Info_1)
applicants$Product_Info_2 <- as.factor(applicants$Product_Info_2)
applicants$Product_Info_3 <- as.factor(applicants$Product_Info_3)
applicants$Product_Info_5 <- as.factor(applicants$Product_Info_5)
applicants$Product_Info_6 <- as.factor(applicants$Product_Info_6)
applicants$Product_Info_7 <- as.factor(applicants$Product_Info_7)
summary(applicants)
