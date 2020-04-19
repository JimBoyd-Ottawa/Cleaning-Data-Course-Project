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
## Now we have to create a summary with the average for each subject, activity and variable
##
observationsummary <- select(observations, -observationid, -set ) %>%
        gather( key = "measurementtype", value = "measurement", -activitydesc, -subjectid ) %>%
        group_by(activitydesc, subjectid, measurementtype) %>%
        summarize( average = mean(measurement))
