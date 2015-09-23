# Course: Coursera - Getting and Cleaning Data
# Instructor: Dr. Jeff Leeks
# Student: ldriggen
# Purpose: using the datafile found athttps://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#          perform the following:
#          1. Merge the training and test sets to create one dataset
#          2. Extract only the measurements on the mean and standard deviation for each measurement
#          3. Use descruptive activity names  to name the activities in the dataset
#          4. Appropriately label the dataset with descriptive variable labels
#          5. From the dataset in step 4, create a second independent tidy dataset with the average of
#             each variable for each activity and each subject




# read in the test related files
subject_test <-read.table("./test/subject_test.txt", header = FALSE)
X_test <-read.table("./test/X_test.txt", header = FALSE)
Y_test <-read.table("./test/Y_test.txt", header = FALSE)

# read in the training related files
subject_train <-read.table("./train/subject_train.txt", header = FALSE)
X_train <-read.table("./train/X_train.txt", header = FALSE)
Y_train <-read.table("./train/Y_train.txt", header = FALSE)

# column bind the individual training and test dataframes to create one
# dataframe containing all the information for test and train
train <-cbind(subject_train,Y_train,X_train)
test <-cbind(subject_test,Y_test,X_test)


# row bind the test and training dataframes to create a single data frame
# containing all data 
all <- rbind(train,test)


# read in the activity labels 
activity_labels <- read.table("activity_labels.txt")
i <- sapply(activity_labels, is.factor)
activity_labels[i] <- lapply(activity_labels[i], as.character)
colnames(activity_labels)<-c("Activity","Activity_Description")

# read in the features table and determine the columns that contain 
# means and standard deviations 
features <- read.table("features.txt")
features_mean <- features[grep("-mean\\(\\)", features$V2), ]
features_std  <- features[grep("-std\\(\\)", features$V2), ]
features_subset <- rbind(features_mean,features_std)
i <- sapply(features_subset, is.factor)
features_subset[i] <- lapply(features_subset[i], as.character)

# subject and activity are the first two columns of dataframe all
# create a vector to containing the columns of all to keep,
# which will be 1, 2, and the V1 values in features subset increased by 2;
feature_indx<-features_subset$V1+2

# create an index of the columns from dataframe all to select
all_indx<-c(c(1,2),feature_indx)
# assemble the variable names to be used for the selected columns
all_desc<-c(c("Subject","Activity"),features_subset$V2)
# dataframe all_select_cols has the columns we need
all_select_cols <- all[,all_indx]
# apply the variable names for the selected columns
colnames(all_select_cols)<-all_desc

library(plyr)
# join the all_select_cols dataframe with the activity labels to include the activity descriptions
all_select_cols_activity_desc<-arrange(join(all_select_cols,activity_labels),Activity)

# verify that the Activities match the Activity Descriptions
table(all_select_cols_activity_desc$Activity,all_select_cols_activity_desc$Activity_Description)

# Start of tidy dataset creation

# dataframe all_select_cols_activity_desc_means contains the means by subject and activity description
all_select_cols_activity_desc_means<-ddply(all_select_cols_activity_desc, c("Subject", "Activity_Description"), function(x) colMeans(x[features_subset$V2]))

library(reshape2)
# transform all_select_cols_activity_desc_means into a long dataframe
pre_tidy1<- melt(all_select_cols_activity_desc_means,id=c("Subject", "Activity_Description"),measure.vars=features_subset$V2)

#convert the factors into character variables
i <- sapply(pre_tidy1, is.factor)
pre_tidy1[i] <- lapply(pre_tidy1[i], as.character)

# split the former variable name into measurement, statistic, and axis
pre_tidy2<-mutate(pre_tidy1,measurement=unlist(strsplit(pre_tidy1$variable, "-", fixed = TRUE))[[1]])
splits=strsplit(pre_tidy1$variable,'-')
pre_tidy1$Measurement<-sapply(splits,function(x)x[1])
pre_tidy1$Statistic<-sapply(splits,function(x)x[2])
pre_tidy1$Axis<-sapply(splits,function(x)x[3])

# put the columns into a more logical order and make columns names lower case
tidy<-cbind(pre_tidy1$Subject,pre_tidy1$Activity_Description,pre_tidy1$Measurement,pre_tidy1$Statistic,pre_tidy1$Axis,pre_tidy1$value)
colnames(tidy)<-c("subject","activity","measurement","statistic","axis","mean")
tidy<-as.data.frame(tidy)

# output the tidy dataframe
write.table(tidy,"tidy.txt",row.name=FALSE)

