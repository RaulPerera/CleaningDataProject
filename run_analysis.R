##Helpful placeholder depending on what your current working directory is
#getwd()
#setwd("Dataset/UCI HAR Dataset")


###Dataset Source, unzipped outside of R
#URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(URL,"Dataset.zip")


## Create temporary datasets to store variable names and identify columns to keep
columnlabels <- read.table("features.txt")
lowercasenames <- tolower(columnlabels[,2])
keepcolumns <- grepl("mean|std", lowercasenames) 
##This also keeps fields containing meanfreq(), and gravitymean
##Those fields could be removed through special characters by using something like "-mean()|-std()"
##The instructions are somewhat ambigious and felt it safer 
##to possibly include too many fields than not include enough

##Import activitiy labels for future use
activitylabels <- read.table("activity_labels.txt")
names(activitylabels) <- c("activityID","activity")

#Change Working directory to clean train dataset
setwd("train")
## CLEAN train DATASET
## 1. Read the train dataset
train <- read.table("X_train.txt")
## 2. Add column labels in order to appropriately label the raw dataset 
##    with appropriate variable names
names(train) <- lowercasenames
## 3. Keep only columns containing mean or standard deviation of the measurement
trainsubset <-train[,keepcolumns] 
## 4 Read in activity labels and name the columns to enable merge
train_activity_labels<- read.table("Y_train.txt")
names(train_activity_labels) <- "activityID"
## 5. Read subject table into temp table and label it
train_subjects <- read.table("subject_train.txt")
names(train_subjects) <- "Subject"
## 6. Combine activity lables, subject labels, with measurement data
combined_train <- cbind(train_subjects,train_activity_labels,trainsubset)
##7 Add descriptive activity labels to initial table and convert into table data.frame
combined_train_labels <- tbl_df(merge(
  combined_train, activitylabels, by.x="activityID", by.y="activityID"
))
##8 Drop Activitiy ID column and move activity column to the beginning
full_train <- combined_train_labels %>%
  select(-activityID) %>%
  select(activity, everything())

## Repeat train data set steps on test data set
setwd("..")
#Change Working directory to clean test dataset
setwd("test")
## CLEAN test DATASET
## 1. Read the test dataset
test <- read.table("X_test.txt")
## 2. Add column labels in order to appropriately label the raw dataset 
##    with appropriate variable names
names(test) <- lowercasenames
## 3. Keep only columns containing mean or standard deviation of the measurement
testsubset <-test[,keepcolumns] 
## 4 Read in activity labels and name the columns to enable merge
test_activity_labels<- read.table("Y_test.txt")
names(test_activity_labels) <- "activityID"
## 5. Read subject table into temp table and label it
test_subjects <- read.table("subject_test.txt")
names(test_subjects) <- "Subject"
## 6. Combine activity lables, subject labels, with measurement data
combined_test <- cbind(test_subjects,test_activity_labels,testsubset)
##7 Add descriptive activity labels to initial table and convert into table data.frame
combined_test_labels <- tbl_df(merge(
  combined_test, activitylabels, by.x="activityID", by.y="activityID"
))
##8 Drop Activitiy ID column and move activity column to the beginning
full_test <- combined_test_labels %>%
  select(-activityID) %>%
  select(activity, everything())

## Combine test and train data set into one data.frame
rundata <- rbind(full_train,full_test)


##Create 2nd tidy data set with the mean of means for each subject and activity
rundata_mean <- rundata %>%
  group_by(activity,Subject) %>%
  summarise_all(.funs = c(mean="mean"))

setwd("..")
write.table(rundata_mean,file="rundata_mean.txt",row.name=FALSE)
