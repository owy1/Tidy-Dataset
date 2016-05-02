# accomplish the following five goals:

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set
#   with the average of each variable for each activity and each subject

#read files 
file_test_x <- read.table("~/Desktop/UCI HAR Dataset/test/X_test.txt")
file_test_y <- read.table("~/Desktop/UCI HAR Dataset/test/y_test.txt")
file_train_x <- read.table("~/Desktop/UCI HAR Dataset/train/X_train.txt")
file_train_y <- read.table("~/Desktop/UCI HAR Dataset/train/y_train.txt")
subject_test <- read.table("~/Desktop/UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("~/Desktop/UCI HAR Dataset/train/subject_train.txt")

#transpose attributes (561) to one row
attr561temp <- read.table("~/Desktop/UCI HAR Dataset/features_561 attr.txt")
attr561 <- attr561[,2]
attr561 <- t(attr561)
attr561 <- c("subject","activity", as.character(attr561))

#convert activity numbers to descriptive readings (goal 3)
file_test_y[file_test_y == "1"] <- "WALKING"
file_test_y[file_test_y == "2"] <- "WALKING_UPSTAIRS"
file_test_y[file_test_y == "3"] <- "WALKING_DOWNSTAIRS"
file_test_y[file_test_y == "4"] <- "SITTING"
file_test_y[file_test_y == "5"] <- "STANDING"
file_test_y[file_test_y == "6"] <- "LAYING"

file_train_y[file_train_y == "6"] <- "LAYING"
file_train_y[file_train_y == "5"] <- "STANDING"
file_train_y[file_train_y == "4"] <- "SITTING"
file_train_y[file_train_y == "3"] <- "WALKING_DOWNSTAIRS"
file_train_y[file_train_y == "2"] <- "WALKING_UPSTAIRS"
file_train_y[file_train_y == "1"] <- "WALKING"

#comb subject,activity,readings, merge file, add column names (goals 1,4)
temp <- file_test_y[1]
temp_file_test <- cbind(temp,file_test_x)
temp <- subject_test[1]
temp_file_test <- cbind(temp,temp_file_test)
temp <- file_train_y[1]
temp_file_train <- cbind(temp,file_train_x)
temp <- subject_train[1]
temp_file_train <- cbind(temp,temp_file_train)
mergeFile <- rbind(temp_file_train,temp_file_test)
colnames(mergeFile) <- attr561

#load dplyr to extract variables (goal 2)
library(dplyr)
mergeFile_df <- tbl_df(mergeFile)
mergeFile_df

#print out of mergeFile_df
#Source: local data frame [10,299 x 563]
#
#subject activity tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X
#(int)    (chr)             (dbl)             (dbl)             (dbl)            (dbl)
#1        1 STANDING         0.2885845      -0.020294171        -0.1329051       -0.9952786
#2        1 STANDING         0.2784188      -0.016410568        -0.1235202       -0.9982453
#3        1 STANDING         0.2796531      -0.019467156        -0.1134617       -0.9953796

meanstd <- mergeFile_df[, duplicated(colnames(mergeFile_df))]
meanstd_temp <- select(meanstd, subject,activity,contains("mean"), contains("Mean"),contains("std"))
meanstd_temp

#print out of meanstd_temp
#Source: local data frame [10,299 x 88]

#subject activity tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tGravityAcc-mean()-X
#(int)    (chr)             (dbl)             (dbl)             (dbl)                (dbl)
#1        1 STANDING         0.2885845      -0.020294171        -0.1329051            0.9633961
#2        1 STANDING         0.2784188      -0.016410568        -0.1235202            0.9665611
#3        1 STANDING         0.2796531      -0.019467156        -0.1134617            0.9668781

#write tidy data set with average of each variable for each activity and each subject (goal 5)
subject_agg <- group_by(x,subject,activity)
temp <- summarise_each(subject_agg, funs(mean))
write.csv(temp, file = "meanstd_avg.csv", row.names=FALSE)