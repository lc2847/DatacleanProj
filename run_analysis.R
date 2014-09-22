require(plyr)
require(RCurl)

# cleanup 
rm(list = ls())
baseDir <- "."

# create data sub-directory if necessary
dataDir <- paste(baseDir, "data", sep="/")
if(!file.exists(dataDir)) {
    dir.create(dataDir)
}

# download original data if necessary (skip if exists already as it takes time)
zipFilePath <- paste(dataDir, "Dataset.zip", sep="/")
if (!file.exists(zipFilePath)) {
    zipFileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file (zipFileUrl, zipFilePath)
    dateDownloaded <- date()
    cat ("Dataset downloaded on:", dateDownloaded,"\n")
}

# unzip and creates dataSetDir if necessary
dataSetDir <-  paste (baseDir, "UCI HAR Dataset", sep="/")
if (!file.exists(dataSetDir)) {
    unzip (zipFilePath, exdir=baseDir)
}
list.files(baseDir)

# Read train and test data together using ldply, 
data.filename <- c("UCI HAR Dataset/train/X_train.txt", "UCI HAR Dataset/test/X_test.txt")
data <- ldply(data.filename, read.table)

# Read labels, assign to colnames(data) to finish Step 1
label <- read.table("UCI HAR Dataset/features.txt")[, 2]
colnames(data) <- label

# Extract activity id and corresponding label
activity.filename = c("UCI HAR Dataset/train/y_train.txt", "UCI HAR Dataset/test/y_test.txt")
activity <- ldply(activity.filename, read.table)
colnames(activity) <- c("activity_id")

activity.label <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"))
activity$activity_label <- apply(activity, 1,function(x) activity.label[x,2] )

# add it to data
data$activity_label <- activity$activity_label

# Extract subject id
subject.filename = c("UCI HAR Dataset/train/subject_train.txt", "UCI HAR Dataset/test/subject_test.txt")
subject <- ldply(subject.filename, read.table)

# Add it to data
data$subject_id <- subject[, 1]

# Extracting those colnames which contains "std()" and "mean()" 
# using customized filter function
Find_Mean_Std <- function (s) {
    return(grepl("-std()", s) || grepl("-mean()", s))
}

mean_std_data <- data[, sapply(label, Find_Mean_Std)]

# Calculate the mean for each (subject, activity) pair

tidy <- aggregate(. ~ subject_id + activity_label, data=mean_std_data, mean)
tidyPath <- paste(dataDir, "tidy.txt", sep="/")
write.table(tidy, tidyPath, sep="\t", col.names=T, row.names = F, quote=T)

# verify data
veriFile <- read.table(tidyPath, sep="\t")
dim(veriFile)
