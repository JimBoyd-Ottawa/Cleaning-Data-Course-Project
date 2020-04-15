## The first step is to download the file from the web site
##
File_URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(File_URL, destfile = "accelerometers.zip")
##
## unzip the files
##
unzip("accelerometers.zip")
##
## read all of the tables into data frames
##
features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
body_acc_x_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt")
body_acc_y_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt")
body_acc_z_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt")
total_acc_x_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt")
total_acc_y_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt")
total_acc_z_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt")
body_gyro_x_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt")
body_gyro_y_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt")
body_gyro_z_test <- read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
body_acc_x_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt")
body_acc_y_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt")
body_acc_z_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt")
total_acc_x_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt")
total_acc_y_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt")
total_acc_z_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt")
body_gyro_x_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt")
body_gyro_y_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt")
body_gyro_z_train <- read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt")
##
## we need to endure our libraries are active
##
library(plyr)
library(tidyr)
library(dplyr)
##
## We combine the test and train datasets since they were arbitrarily split
##
features <- mutate(features, feature_id = V1, feature_name = V2) %>%
  select( feature_id, feature_name)
x_features <- rbind(x_test,x_train) %>%
                  mutate(observation_id = row_number())
## feature_measurements <- 
y_test2 <- mutate(y_test, test_type = "test")
y_train2 <- mutate(y_train, test_type = "train")
y_activity <- rbind(y_test2,y_train2) %>%
                 mutate(activity_id = V1) %>%
                 select(activity_id, test_type)
activities <- mutate(activity_labels, activity_id = V1, activity_desc = V2) %>%
                select(activity_id, activity_desc)
observations <- rbind(subject_test,subject_train) %>%
                  mutate(observation_id = row_number(), subject_id = V1) %>%
                  select(observation_id, subject_id) %>%
                  cbind(y_activity)
body_acc_x <- rbind(body_acc_x_test,body_acc_x_train) %>%
                  mutate(observation_type_id = 1, observation_id = row_number())
body_acc_y <- rbind(body_acc_y_test,body_acc_y_train) %>%
                  mutate(observation_type_id = 2, observation_id = row_number())
body_acc_z <- rbind(body_acc_z_test,body_acc_z_train) %>%
                  mutate(observation_type_id = 3, observation_id = row_number())
total_acc_x <- rbind(total_acc_x_test,total_acc_x_train) %>%
                  mutate(observation_type_id = 4, observation_id = row_number())
total_acc_y <- rbind(total_acc_y_test,total_acc_y_train) %>%
                  mutate(observation_type_id = 5, observation_id = row_number())
total_acc_z <- rbind(total_acc_z_test,total_acc_z_train) %>%
                  mutate(observation_type_id = 6, observation_id = row_number())
body_gyro_x <- rbind(body_gyro_x_test,body_gyro_x_train) %>%
                  mutate(observation_type_id = 7, observation_id = row_number())
body_gyro_y <- rbind(body_gyro_y_test,body_gyro_y_train) %>%
                  mutate(observation_type_id = 8, observation_id = row_number())
body_gyro_z <- rbind(body_gyro_z_test,body_gyro_z_train) %>%
                  mutate(observation_type_id = 9, observation_id = row_number())
observation_types <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9,
                              "body_acc_x", "body_acc_y", "body_acc_z",
                              "total_acc_x", "total_acc_y", "total_acc_z",
                              "body_gyro_x", "body_gyro_y", "body_gyro_z"
                              ), nrow = 9, ncol = 2, byrow = FALSE,
                            dimnames = list(NULL,c("observation_type_id","observation_type")))
##
## We stack all of the raw observations into one table to simplify and tidy it up
##
raw_observations <- rbind(body_acc_x,body_acc_y,body_acc_z,total_acc_x,total_acc_y,
                          total_acc_z,body_gyro_x,body_gyro_y,body_gyro_z)