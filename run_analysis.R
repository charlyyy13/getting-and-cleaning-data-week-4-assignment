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
#test set
xtest <- read.table((file.choose()))
#test label
ytest <- read.table((file.choose()))
subjecttest<- read.table((file.choose()))

xtrain <- read.table((file.choose()))
ytrain <- read.table((file.choose()))
subjecttrain<- read.table((file.choose()))

# read features, without converting text labels to factors
features <- read.table((file.choose()), as.is = TRUE)

# read activity labels
activities <- read.table(file.choose())

##############################################################################
# Merge the training and the test sets to create one data set
##############################################################################
# merge of training and test sets
#was called x_train y_train inclduding rbind (ytest,)
train_total  <- cbind(xtrain, ytrain, subjecttrain)
test_total   <- cbind(xtest, ytest, subjecttest) 

#assign colnames - Use descriptive activity names to name the activities
colnames(train_total) <- features[,2]
colnames(test_total) <- features[,2]

#merge final dataset
total <- rbind(train_total, test_total)
summary(total)
#652 - label activities
#653 - subject
colnames(total)[563] <- "subject"
colnames(total)[562] <- "activities"

##############################################################################
#  Extracts only the measurements on the mean and standard deviation for each measurement
##############################################################################

# Keep columns based on column name -  mean and std (also activities and subject)
human <- select(total, colnames(total) [grepl("mean|std|subject|activities",colnames(total))])

##############################################################################
#Uses descriptive activity names to name the activities in the data set
##############################################################################
human$activities <- factor(human$activities, levels = activities[,1], labels = activities[,2]) 
human$subject  <- as.factor(human$subject) 

##############################################################################
# Appropriately label the data set with descriptive variable names
# and
# creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##############################################################################
#with the average of each variable for each activity and each subject
total_mean <- human %>% group_by(activities, subject) %>% summarise_all(funs(mean)) 
total_mean

#colnames
humancols <- colnames(total_mean)
humancols

## remove special characters
humancols <- gsub("[\\(\\)-]", "", humancols)
humancols
colnames(total_mean) <- humancols

#appropriate label
names(total_mean)<-gsub("Acc", "Accelerometer", names(total_mean))
names(total_mean)<-gsub("Gyr","Gyroscope", names(total_mean))
names(total_mean)<-gsub("^t", "Time", names(total_mean))
names(total_mean)<-gsub("^f", "Frequency", names(total_mean))
names(total_mean)

##########################################################
#Export to file "tidydata.txt"
#############################

#export to file "tidydata.txt"
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE) 

#remove data
rm(xtrain, subjecttrain, ytrain,xtest, subjecttest, ytest)
rm(activities, features, x_total, y_total)
