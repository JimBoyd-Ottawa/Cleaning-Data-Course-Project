To process these files we followed the following steps

Run run_analysis.R 

This performs the following stepsto create the observations data frame:

1) download the file from the web site
2) unzip all the files
3) read all of the tables into data frames
4) endure our libraries are active
5) combine the test and train datasets
6) process the 561 feature vector to update the frequency domain variables table with proper names
7) select only those values that represent mean or std deviation
8) calculate the basic observation information like subjectid, activityid, observationid, & activitydesc & testtype
9) eliminate lower case letters from variable names 
10) expand names that are too short and vague

Then the same script creates a summary called observation summary

11) create a summary with the average for each subject, activity and variable

The detailed variable names are described in the two documentation files:
observationsummary.Rd &
observation.Rd