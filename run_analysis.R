#==============================================================================#
#---------------------------- Environment R -----------------------------------#
#==============================================================================#

options(prompt = "[] ", continue = "..  ", width = 70,
        useFancyQuotes = FALSE, digits = 3, warn = 1)

#==============================================================================#
#------------------------------ Library R -------------------------------------#
#==============================================================================#

#library(gdata)
library(sqldf)
library(plyr)
library(nFactors)
library(RODBC)
library(reshape)
library(foreign)
library(memisc)
library(car)
library(data.table)
library(gtools)
#library(xlsx)
#library(xls)

memory.limit(size = 3000)

#==============================================================================#
#-------------------------- working directory ---------------------------------#
#==============================================================================#

setwd("C:/Users/JCHO/Desktop/Project Course") #
getwd()
#[1] "C:/Users/JCHO/Desktop/Project Course"

dir()
#[] dir()
#[1] "getdata-projectfiles-UCI HAR Dataset.zip"
#[2] "link.txt"                               
#[3] "run_analysis.R"                            
#[4] "UCI HAR Dataset"                         


#==============================================================================#
# Step1. Merges the training and the test sets to create one data set.
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
dim(trainData)
#[1] 7352  561

names(trainData)
head(trainData)


trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
dim(trainLabel)
#[1] 7352   1

names(trainLabel)
head(trainLabel)

table(trainLabel)

trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
dim(trainSubject)
#[1] 7352    1

names(trainSubject)
head(trainSubject)


#------------------------------------------------------------------------------#

testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
dim(testData) 
#[1] 2947 561

testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt") 
table(testLabel) 

testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#------------------------------------------------------------------------------#


joinData <- rbind(trainData, testData)
dim(joinData) 
#[1] 10299 561

joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) 
#[1] 10299 1

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) 
#[1] 10299 1

#==============================================================================#
# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 

features <- read.table("./UCI HAR Dataset/features.txt")
dim(features)
#[1] 561 2

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) 
#[1] 66

joinData <- joinData[, meanStdIndices]
dim(joinData) 
#[1] 10299 66

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
# remove "()"

names(joinData) <- gsub("mean", "Mean", names(joinData)) 
# capitalize M

names(joinData) <- gsub("std", "Std", names(joinData)) 
# capitalize S

names(joinData) <- gsub("-", "", names(joinData)) 
# remove "-" in column names 

#==============================================================================#
# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")

activity[, 2] <- tolower(gsub("_", "", activity[, 2]))

substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))

substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))

activityLabel <- activity[joinLabel[, 1], 2]

joinLabel[, 1] <- activityLabel

names(joinLabel) <- "activity"

#==============================================================================#
# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject) <- "subject"

cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) 
#[1] 10299 68

write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

#==============================================================================#
# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) 
#[1] 30

activityLen <- dim(activity)[1] 
#[1] 6

columnLen <- dim(cleanedData)[2]

result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)

colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
        for(j in 1:activityLen) {
                result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
                result[row, 2] <- activity[j, 2]
                bool1 <- i == cleanedData$subject
                bool2 <- activity[j, 2] == cleanedData$activity
                result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
                row <- row + 1
        }
}
head(result)

# write out the 2nd dataset
write.table(result, "data_with_means.txt") 



data <- read.table("./data_with_average_each_variable.txt")
data[1:12, 1:3]

#[] data[1:12, 1:3]
#   subject          activity tBodyAccMeanX
#1        1           walking     0.2773308
#2        1   walkingUpstairs     0.2554617
#3        1 walkingDownstairs     0.2891883
#4        1           sitting     0.2612376
#5        1          standing     0.2789176
#6        1            laying     0.2215982
#7        2           walking     0.2764266
#8        2   walkingUpstairs     0.2471648
#9        2 walkingDownstairs     0.2776153
#10       2           sitting     0.2770874
#11       2          standing     0.2779115
#12       2            laying     0.2813734

