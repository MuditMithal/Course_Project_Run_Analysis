## SCROLL SOURCE WINDOW TO HERE FOR STANDARD 80 COLUMN WIDTH >->->->->->->->->->
################################################################################
## run_analysis.R
##
## Peer Assessment
## Coursera: Getting and Cleaning Data Project 
## April 2014


## Required packages
library("plyr")
library("reshape2")


## Unzip the source data
srcDir <- "UCI HAR Dataset"
srcData <- "getdata_projectfiles_UCI HAR Dataset.zip"

if (!file.exists(srcDir)) {
  if (file.exists(srcData)) {
    unzip(srcData)
  } else {
    stop("Couldn't find the source data zipped file")
  }
}


# Gather the training data and bind into a single "train" data table
subject_train <- read.table(paste(srcDir, "train/subject_train.txt", sep="/"),
                            sep="\n", strip.white=TRUE)

train_x <- read.table(paste(srcDir, "train/X_train.txt", sep="/"),
                      sep="\n", strip.white=TRUE)
train_x <- ldply(strsplit(gsub(" {2,}", " ", train_x$V1), " "))

train_y <- read.table(paste(srcDir, "train/y_train.txt", sep="/"),
                      sep="\n", strip.white=TRUE)

train <- cbind(train_y, subject_train, train_x)
rm(subject_train, train_x, train_y)


# Gather the test data and bind into a single "test" data table
subject_test <- read.table(paste(srcDir, "test/subject_test.txt", sep="/"),
                           sep="\n", strip.white=TRUE)

test_x <- read.table(paste(srcDir, "test/X_test.txt", sep="/"),
                     sep="\n", strip.white=TRUE)
test_x <- ldply(strsplit(gsub(" {2,}", " ", test_x$V1), " "))

test_y <- read.table(paste(srcDir, "test/y_test.txt", sep="/"),
                     sep="\n", strip.white=TRUE)

test <- cbind(test_y, subject_test, test_x)
rm(subject_test, test_x, test_y)


# and finally combine the train and test data sets
combined <- rbind(train, test)
rm(train, test)


# Get the feature names
feature_names <- read.table(paste(srcDir, "features.txt", sep="/"),
                            sep="\n", strip.white=TRUE)
feature_names <- gsub("^[0-9]+ ", "", feature_names$V1)
features_kept <- grepl("mean|std", feature_names)


# Trim data frame columns to stuff we are only interested in and label columns
combined <- combined[,c(TRUE, TRUE, features_kept)]
colnames(combined) <- c("activity", "subject", feature_names[features_kept])
rm(feature_names, features_kept)


# make feature factor values numeric values
for (i in 3:ncol(combined)){
  combined[,i] <- as.numeric(combined[,i])
}


# Finally write the tidy 
write.table(combined, file="combined_data.txt")


# Creates a second data set with the average of each variable for each activity
# and subject passing in a function mean
summary <- aggregate(combined[,3] ~ combined$subject+combined$activity,
                     data=combined, FUN=mean)

for (i in 4:ncol(combined)){
  summary[,i] <- aggregate(combined[,i] ~ combined$subject+combined$activity,
                           data=combined, FUN=mean )[,3]
}


# Finally write the tidy summary data of the means
colnames(summary) <- colnames(combined)
write.table(summary, file="summary_data.txt")
