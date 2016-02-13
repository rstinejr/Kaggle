cat("Summary of applicant data")
applicants  <- read.csv("applicants.csv",head=TRUE,sep=",")
summary(applicants)
