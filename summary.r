cat("Summary of applicant data")
applicants  <- read.csv("applicants.csv",head=TRUE,sep=",")
applicants$Product_Info_4_cat <- cut(applicants$Product_Info_4, 5)
summary(applicants)
