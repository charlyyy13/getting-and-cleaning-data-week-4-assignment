#Task:
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#install packages
library(data.table)
library(dplyr)

##############################################################################
# STEP 0B - Read data
##############################################################################
#Load data
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destFile <- "UCI_HAR_Dataset.zip"
if (!file.exists(destFile)){
  download.file(URL, destfile = destFile, mode='wb')
}
if (!file.exists("./UCI_HAR_Dataset")){
  unzip(destFile)
}
dateDownloaded <- date()

##
xtest <- read.table((file.choose()))
ytest <- read.table((file.choose()))
subjecttest<- read.table((file.choose()))

xtrain <- read.table((file.choose()))
ytrain <- read.table((file.choose()))
subjecttrain<- read.table((file.choose()))

# read features, don't convert text labels to factors
features <- read.table((file.choose()), as.is = TRUE)

# read activity labels
activities <- read.table(file.choose())
colnames(activities) <- c("activityId", "activityLabel")

##############################################################################
# Merge the training and the test sets to create one data set
##############################################################################

# merge of training and test sets
x_total   <- rbind(xtrain, xtest)
y_total   <- rbind(ytrain, ytest) 
sub_total <- rbind(subjecttest,subjecttrain)

# remove individual data tables to save memory
rm(xtrain, subjecttrain, ytrain,xtest, subjecttest, ytest)

#assign colnames - Use descriptive activity names to name the activities
colnames(x_total)   <- features[,2]
colnames(y_total)   <- "activity"
colnames(sub_total) <- "subject"

#merge final dataset
total <- cbind(sub_total, y_total, x_total)

##############################################################################
#  Extracts only the measurements on the mean and standard deviation for each measurement
##############################################################################

# determine columns of dataset to keep based on column name
columnstokeep <- grepl("subject|activity|mean|std", colnames(total))

# keep only data in these columns
human <- total[, columnstokeep]

##############################################################################
# Appropriately label the data set with descriptive variable names
##############################################################################
#colnames
humancols <- colnames(human)
humancols

## remove special characters
humancols <- gsub("[\\(\\)-]", "", humancols)
humancols

# expand abbreviations and clean up names
humancols <- gsub("^f", "frequencyDomain", humancols)
humancols <- gsub("^t", "timeDomain", humancols)
humancols <- gsub("Acc", "Accelerometer", humancols)
humancols <- gsub("Gyro", "Gyroscope", humancols)
humancols <- gsub("Mag", "Magnitude", humancols)
humancols <- gsub("Freq", "Frequency", humancols)
humancols <- gsub("mean", "Mean", humancols)
humancols <- gsub("std", "StandardDeviation", humancols)
humancols
human

##############################################################################
#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##############################################################################
total_mean <- human %>% group_by(activity, subject) %>% summarize_all(funs(mean)) 
total_mean
#export to file "tidydata.txt"
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE) 

rm(activities, features, x_total, y_total)
