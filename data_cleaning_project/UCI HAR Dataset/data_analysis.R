##You should create one R script called run_analysis.R that does the following.

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each 
## measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5, From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

library(dplyr)
library(data.table)

features <- fread("features.txt") ##loads variable names
test_set <- fread("test/X_test.txt", col.names = features$V2) #loads data
test_set <- select(test_set, contains(c("mean","std"))) #loads activity dat
test_labels <- fread("test/y_test.txt", col.names = "Activity") #loads labels
test_complete <- tbl_df(cbind(test_labels, test_set)) ##binds 
activity_labels <- fread("activity_labels.txt", select = 2) #numeric to char


test_clean <- filter(test_complete, Activity == 1) %>%
  summarize_all(mean) ##begins clean test data by summarizing first activity
for (i in 2:6) {
  temp <- filter(test_complete, Activity == i) %>%
    summarize_all(mean)
  test_clean <- rbind(test_clean, temp)
}
test_clean$Activity <- activity_labels ##adds descriptive activity variables
test_clean <- mutate(test_clean, Data_Set = "test", .after = Activity) ## adds
##column identifying data set of origin (test or train)

##the following repeats the above code for the train data set
train_set <- fread("train/X_train.txt", col.names = features$V2) #loads data set
train_set <- select(train_set, contains(c("mean","std"))) #loads activity data
train_labels <- fread("train/y_train.txt", col.names = "Activity") #loads labels
train_complete <- tbl_df(cbind(train_labels, train_set)) ##binds 

train_clean <- filter(train_complete, Activity == 1) %>%
  summarize_all(mean)
for (i in 2:6) {
  temp <- filter(train_complete, Activity == i) %>%
    summarize_all(mean)
  train_clean <- rbind(train_clean, temp)
}
train_clean$Activity <- activity_labels
train_clean <- mutate(train_clean, Data_Set = "train", .after = Activity)

mergedData <- rbind(train_clean, test_clean) ##merges test and train data sets
