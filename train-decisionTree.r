cat("Start training session for decision trees.\n")
apps <- read.csv("bnp-training.csv",head=TRUE,sep=",")
cat("Training data ingested, rows: ", nrow(apps), "\n");
