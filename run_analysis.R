


########################################
## 1. Preliminary Steps
########################################

# set working directory:
setwd("C:/Users/Christian/Documents/Coursera/Data Science/03 - Getting and Cleaning Data/course_project/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
getwd()
dir()


file <- "features.txt"
# Output message if relevant files don't exist.
if (!file.exists(file)) {
    print("Relevant data not available in working directory")
  }


# load required packages:
library(data.table)
library(reshape2)




########################################
## 2. Read the relevant data
########################################

## Read Feature
features <- read.table("features.txt")
names(features) <- c("No","Feature")
dim(features)
## 561 2


## Read Activity Labels
activity_labels <- read.table("activity_labels.txt")
names(activity_labels) <- c("No","Activity")
dim(activity_labels)
# 6 2


## READ Test Set:
X_test <- read.table("test/X_test.txt")
Y_test <- read.table("test/Y_test.txt")
subject_test <- read.table("test/subject_test.txt")
dim(subject_test)
# 2947 1
dim(Y_test)
# 2947 1
dim(X_test)
# 2947 561


## assign Feature names to test set:
names(X_test) <- as.character(features$Feature)


## READ Training Set:
X_train <- read.table("train/X_train.txt")
Y_train <- read.table("train/Y_train.txt")
subject_train <- read.table("train/subject_train.txt")
dim(subject_train)
# 7352 1
dim(Y_train)
# 7352 1
dim(X_train)
# 7352 561

## assign Feature names to training set:
names(X_train) <- as.character(features$Feature)



########################################
## 3. Merging the data sets:
########################################

## (i) Merge the training and the test sets to create one data set.
##     (since the data sets are different but the structures are the same 
##      just copy both tables into one new table using rbind() )
X_all <- rbind(X_train, X_test)
Y_all <- rbind(Y_train, Y_test)
subject_all <- rbind(subject_train, subject_test)
## check dimensions
dim(subject_all)
# 10299 1
dim(Y_all)
# 10299 1
dim(X_all)
# 10299 561
## QC:
7352 + 2947
# 10299
## sweet.

## (ii) merge subject (subject_all) and 
##        Activity (Y_all)
##        + Feature (X_all) 
##      in new data table

subject_Y_all <- data.table(cbind(subject_all, Y_all))
setnames(subject_Y_all, c("Subject", "No"))
names(subject_Y_all)
dim(subject_Y_all)
# 10299 2
## ok.

subject_X_all <- data.table(cbind(subject_Y_all, X_all))
## Set Key in new data table
setkey(subject_X_all, Subject, No)

## check dimensions:
dim(subject_X_all)
# 10299 563
## ok.



########################################
## 4.Extracts only the measurements on the mean and standard deviation 
##   for each measurement. 
########################################

## get relevant columns for mean()
a <- grep("mean\\(\\)",names(subject_X_all))
names(subject_X_all[,a,with=FALSE])
length(names(subject_X_all[,a,with=FALSE]))
## 33

## get relevant columns for std()
b <- grep("std\\(\\)",names(subject_X_all))
names(subject_X_all[,b,with=FALSE])
length(names(subject_X_all[,b,with=FALSE]))
## 33

## Check if all selected columns are either mean()- or std()-related
names(subject_X_all[,c(a,b),with=FALSE]) 
## ok

## get relevant columns that build the key of the data table
d <- which(names(subject_X_all) %in% key(subject_X_all) == TRUE)


## subseting the relevant columns 
##  (i)   containing mean (vector a), OR
##  (ii)  containing std  (vector b), OR
##  (iii) being key (vector d) of the data table
subject_X_all_mean_std <- subject_X_all[,c(a,b,d),with=FALSE]
dim(subject_X_all_mean_std)
## 10299 68
## okay, matches expectations (33 + 33 (std + mean) + 2 (key))


## check the key:
key(subject_X_all_mean_std)



########################################
## 5.Uses descriptive activity names to name the activities in the data set
#    names for activities
########################################


## check the labels for the data set (already set above)
head(subject_Y_all)
setnames(subject_Y_all,c("Subject","No"))
table(subject_Y_all$No)

## Merge the tables (based on column "No") and check the counts
subject_Y_all_activity <- merge(activity_labels, subject_Y_all, by = "No")
table(subject_Y_all_activity$No)
table(subject_Y_all$No)
## okay, counts match.

## Merge activity labels & the subset containg the relevant columns 
##  (created above: subject_X_all_mean_std)
subject_X_all_mean_std_activity_label <- merge(data.table(activity_labels), subject_X_all_mean_std, by = "No")


## check dimension
dim(subject_X_all_mean_std_activity_label)
# 10299 69
## okay, matches expectations.

## set key
setkey(subject_X_all_mean_std_activity_label, Subject, No, Activity)
key(subject_X_all_mean_std_activity_label)


########################################
## 6.Appropriately labels the data set with descriptive variable names. 
########################################

## see above (already executed):
## assign Feature names to training set:
# names(X_train) <- as.character(features$Feature)
# names(X_test) <- as.character(features$Feature)

names(subject_X_all)
names(subject_X_all_mean_std_activity_label)


########################################
## 7.From the data set in step 4, creates a second, 
##    independent tidy data set with the average 
##    of each variable for each activity and each subject.
########################################

## get vector containing the relevant variables that we want to calulate 
##  the average for each activity and each subject
tmp <- which(!(names(subject_X_all_mean_std_activity_label) %in% c(key(subject_X_all)
                                                                   ,"Activity")) == TRUE)
tmp1 <- names(subject_X_all_mean_std_activity_label[,tmp,with=FALSE])


names(subject_X_all_mean_std_activity_label)

## Pivot the table 
melt_data <- melt(subject_X_all_mean_std_activity_label
                      , id = key(subject_X_all_mean_std_activity_label)
                      , measure.vars = tmp1) 
## get dimension
dim(melt_data)
# 679734 5

# Use dcast() to calculate the average for each activity and each subject 
tidy_data <- dcast(melt_data, Subject + Activity ~ variable, mean) 
dim(tidy_data)
# 180 68
head(tidy_data)

## write the tidy dataset
write.table(tidy_data, file = "./tidy_data.txt", row.name=FALSE ) 
