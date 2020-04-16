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
## then we process the 561 feature vector to update the frequency domain variables table
##with proper names
##
features <- mutate(features, featureid = V1, featurename = V2)
frequencydomainvariables <- rbind(x_test,x_train)
featurenames <- features[,2]
colnames(frequencydomainvariables) <- featurenames
##
## Next we need to select only those values that represent mean or std deviation
##
frequencydomainvariables <- frequencydomainvariables[ !duplicated(names(frequencydomainvariables)) ]
frequencydomainvariables <- select(frequencydomainvariables, contains("mean") | contains("std"))
##
## Here we calculate the basic observation information like subjectid, activityid
##     observationid, & activitydesc & testtype
##
activities <- mutate(activity_labels, activityid = V1, activitydesc = V2) %>%
                  select(activityid, activitydesc)
y_test2 <- mutate(y_test, set = factor("test"))
y_train2 <- mutate(y_train, set = factor("train"))
y_activity <- rbind(y_test2,y_train2) %>%
                 mutate(activityid = V1) %>%
                 select(activityid, set)
observation_root <- merge(x=y_activity,y=activities,by="activityid",all.x=TRUE) %>%
                  select(activitydesc, set)
observations <- rbind(subject_test,subject_train) %>%
                  mutate(observationid = row_number(), subjectid = V1) %>%
                  select(observationid, subjectid) %>%
                  cbind(observation_root, frequencydomainvariables)
names(observations) <- tolower(names(observations)) %>%
                          { gsub("acc","accelerometer",.) } %>%
                          { gsub("gyro","gyroscope",.) } %>%
                          { gsub("std()","standarddeviation",.) } %>%
                          { gsub("\\(\\)","",.) } %>%
                          { gsub("^t","time",.) } %>%
                          { gsub("^angle\\(t","time",.) } %>%
                          { gsub("freq", "frequency",.) } %>%
                          { gsub("^f","frequency",.) }
##
##  Now the first table is done
##  It looks like we don't need to process the raw data so I will comment out this part
##
##body_acc_x <- rbind(body_acc_x_test,body_acc_x_train) %>%
##                  mutate(observation_type_id = 1, observation_id = row_number())
##body_acc_y <- rbind(body_acc_y_test,body_acc_y_train) %>%
##                  mutate(observation_type_id = 2, observation_id = row_number())
##body_acc_z <- rbind(body_acc_z_test,body_acc_z_train) %>%
##                  mutate(observation_type_id = 3, observation_id = row_number())
##total_acc_x <- rbind(total_acc_x_test,total_acc_x_train) %>%
##                  mutate(observation_type_id = 4, observation_id = row_number())
##total_acc_y <- rbind(total_acc_y_test,total_acc_y_train) %>%
##                  mutate(observation_type_id = 5, observation_id = row_number())
##total_acc_z <- rbind(total_acc_z_test,total_acc_z_train) %>%
##                  mutate(observation_type_id = 6, observation_id = row_number())
##body_gyro_x <- rbind(body_gyro_x_test,body_gyro_x_train) %>%
##                  mutate(observation_type_id = 7, observation_id = row_number())
##body_gyro_y <- rbind(body_gyro_y_test,body_gyro_y_train) %>%
##                  mutate(observation_type_id = 8, observation_id = row_number())
##body_gyro_z <- rbind(body_gyro_z_test,body_gyro_z_train) %>%
##                  mutate(observation_type_id = 9, observation_id = row_number())
##observation_types <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9,
##                              "body_acc_x", "body_acc_y", "body_acc_z",
##                              "total_acc_x", "total_acc_y", "total_acc_z",
##                              "body_gyro_x", "body_gyro_y", "body_gyro_z"
##                              ), nrow = 9, ncol = 2, byrow = FALSE,
##                            dimnames = list(NULL,c("observation_type_id","observation_type")))
##
## We stack all of the raw observations into one table to simplify and tidy it up
##
##raw_observations <- rbind(body_acc_x,body_acc_y,body_acc_z,total_acc_x,total_acc_y,
##                          total_acc_z,body_gyro_x,body_gyro_y,body_gyro_z)