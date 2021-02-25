#Task:
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#install packages
library(data.table)
library(dplyr)
library(magrittr)

##############################################################################
# Read data
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

##read data
xtest <- read.table((file.choose()))
ytest <- read.table((file.choose()))
subjecttest<- read.table((file.choose()))

xtrain <- read.table((file.choose()))
ytrain <- read.table((file.choose()))
subjecttrain<- read.table((file.choose()))

# read features, without converting text labels to factors
features <- read.table((file.choose()), as.is = TRUE)

# read activity labels
activities <- read.table(file.choose())

#assign col names
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

human$activity <- factor(human$activity, levels = activities[,1], labels = activities[,2]) 
human$subject  <- as.factor(human$subject) 

total_mean <- human %>% group_by(activity, subject) %>% summarise_all(funs(mean)) 
total_mean

#colnames
humancols <- colnames(total_mean)
humancols

## remove special characters
humancols <- gsub("[\\(\\)-]", "", humancols)
humancols

##############################################################################
#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##############################################################################

#export to file "tidydata.txt"
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE) 

rm(activities, features, x_total, y_total)

