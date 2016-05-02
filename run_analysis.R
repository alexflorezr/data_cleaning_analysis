rm(list=ls())
# Download the raw data
# Read the variables names and activity names
setwd("~/Downloads/Datacleaning/UCI_HAR_Dataset")
activity_labels <- read.table("activity_labels.txt", stringsAsFactors = F)
variable_names <- read.table("features.txt", stringsAsFactors = F)
# Read the test data: values, subjects and activity
  setwd("~/Downloads/Datacleaning/UCI_HAR_Dataset/test/")
  x_test <- read.table("X_test.txt")
  subject_test <- read.table("subject_test.txt")
  activity_test <- read.table("y_test.txt")
# Read the train data: values, subjects and activity
  setwd("~/Downloads/Datacleaning/UCI_HAR_Dataset/train/")
  x_train <- read.table("X_train.txt")
  subject_train <- read.table("subject_train.txt")
  activity_train <- read.table("y_train.txt")
# merge test and train data 
test_train <- rbind(x_test, x_train)
# Extract the measurements on the mean and standard deviation
mean_measurements <- grep("mean()",variable_names[,2], fixed = T)
std_measurements <- grep("std()",variable_names[,2], fixed = T)
extract <- sort(c(mean_measurements,std_measurements))
test_train_mean_std <- test_train[,(extract)]
# Add descriptive activity names
activity_test_train <- rbind(activity_test,activity_train)
activity_descriptive <- vector(length = length(activity_test_train[,1]))
for (activity in seq_along(activity_test_train[,1])){
  activity_descriptive[activity] <- activity_labels[match(activity_test_train[activity,], activity_labels[,1]),2]
}
test_train_mean_std$activity <- tolower(activity_descriptive)
test_train_mean_std_activity <- test_train_mean_std
# Add subjects 
subject_test_train <- rbind(subject_test, subject_train)
test_train_mean_std_activity_subject <- cbind(test_train_mean_std_activity, subject_test_train)
# Add descriptive variable names
colnames(test_train_mean_std_activity_subject) <- c(gsub("\\(\\)", "", variable_names[extract,2]), "activity", "subject")
# New tidy dataset
new_tidy <- as.data.frame(matrix(nrow = 30*6, ncol=dim(test_train_mean_std_activity_subject)[2]))
newrow <- 1
for (subject in unique(test_train_mean_std_activity_subject$subject)){
  for (activity in unique(test_train_mean_std_activity_subject$activity)){
    temp <- test_train_mean_std_activity_subject[test_train_mean_std_activity_subject$subject == subject & test_train_mean_std_activity_subject$activity == activity,]
    new_tidy[newrow,c(1,2)] <- c(subject, activity)
    new_tidy[newrow,3:68] <- colMeans(temp[1:66])
    newrow <- newrow + 1
  }
}
colnames(new_tidy) <- c("subject", "activity", gsub("\\(\\)", "", variable_names[extract,2]))
setwd("~/Downloads/Datacleaning/Results_tidydata/")
write.table(test_train_mean_std_activity_subject, file = "tidy_data.txt", row.names = F)
write.table(new_tidy, file = "new_tidy_data.txt", row.names = F)
