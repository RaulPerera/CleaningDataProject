###Dataset Source, unzipped outside of R
#URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(URL,"Dataset.zip")

##Helpful placeholder depending on what your current working directory is
#setwd("Dataset/UCI HAR Dataset")


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
names(activitylabels) <- c("activitytype","activity")

#Change Working directory to clean Train dataset
setwd("train")
## CLEAN TRAIN DATASET
## 1. Read the Train dataset
train <- read.table("X_train.txt")
## 2. Add column labels in order to appropriately label the raw dataset 
##    with appropriate variable names
names(train) <- lowercasenames
## 3. Keep only columns containing mean or standard deviation of the measurement
trainsubset <-train[,keepcolumns] 
## 4.1 Read in activity labels and name the columns to enable merge
temp_train_activity_labels<- read.table("Y_train.txt")
names(temp_train_activity_labels) <- "activitytype"
##4.2 Add descriptive activity labels to initial table
full_train_labels <- merge(
  temp_train_activity_labels, activitylabels, by.x="activitytype", by.y="activitytype"
            )
## 5. Read subject table into temp table and label it
train_subjects <- read.table("subject_train.txt")
names(train_subjects) <- "Subject"
## 6. Combine activity lables, subject labels, with measurement data
full_train <- cbind(train_subjects,full_train_labels[,2],trainsubset)
names(full_train)[2] <- "activitytype"

## Repeat train data set steps on test data set
setwd("..")
setwd("test")
#1
test <- read.table("X_test.txt")
#2
names(test) <- lowercasenames
#3
testsubset <-test[,keepcolumns] 
#4
temp_test_activity_labels<- read.table("Y_test.txt")
names(temp_test_activity_labels) <- "activitytype"
full_test_labels <- merge(
  temp_test_activity_labels, activitylabels, by.x="activitytype", by.y="activitytype"
)
#5
test_subjects <- read.table("subject_test.txt")
names(test_subjects) <- "Subject"
#6
full_test <- cbind(test_subjects,full_test_labels[,2],testsubset)
names(full_test)[2] <- "activitytype"

## Combine test and train data set into one data.frame
rundata <- rbind(full_train,full_test)

##Convert rundata into data.table
rundata_tbl <- tbl_df(rundata)

##Create 2nd tidy data set with the mean of means for each subject and activity
rundata_mean <- rundata_tbl %>%
  group_by(activitytype,Subject) %>%
  summarise_all(.funs = c(mean="mean"))

setwd("..")
write.table(rundata_mean,file="rundata_mean.txt",row.name=FALSE)
