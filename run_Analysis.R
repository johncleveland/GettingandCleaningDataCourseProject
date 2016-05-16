library(reshape2)
require(plyr)


# 1. Load Raw Data 


# 1a Load activity labels and  features as characters

activityLabels <- read.table("activity_labels.txt")
#activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("features.txt")
features[,2] <- as.character(features[,2])



###########################################################################################
# 1b Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

features_required <- grep(".*mean.*|.*std.*", features[,2])
features_required.names <- features[features_required,2]



# 1c Load the datasets for the features required

train <- read.table("./train/X_train.txt")[features_required]
trainActivities <- read.table("./train/y_train.txt")
trainSubjects <- read.table("./train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

test <- read.table("./test/X_test.txt")[features_required]
testActivities <- read.table("./test/y_test.txt")
testSubjects <- read.table("./test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)


##################################################################
# 2. Merges the training and the test sets to create one data set
##################################################################
allData1 <- rbind(train, test)

##################################################################################
# 2a Uses descriptive activity names to name the activities in the data set
###########################################################################


colnames(allData1) <- c("Subject", "Activity", features_required.names)

# turn activities & subjects into factors

allData1$Activity <- factor(allData1$Activity, levels = activityLabels[,1], labels = activityLabels[,2])
#allData1$Subject <- as.factor(allData1$Subject)


######################################################################
# 3. Appropriately labels the data set with descriptive variable names.
######################################################################

# Remove parentheses

names(allData1) <- gsub('\\(|\\)',"",names(allData1), perl = TRUE)
names(allData1) <- gsub('Acc',"Acceleration",names(allData1), perl = TRUE)


# Make syntactically valid names

names(allData1) <- make.names(names(allData1))

# More Descriptive Names 

names(allData1) <- gsub('Acc',"Acceleration",names(allData1), perl = TRUE)
names(allData1) <- gsub('GyroJerk',"AngularAcceleration",names(allData1), perl = TRUE)
names(allData1) <- gsub('Gyro',"AngularSpeed",names(allData1), perl = TRUE)
names(allData1) <- gsub('Mag',"Magnitude",names(allData1), perl = TRUE)
names(allData1) <- gsub('^t',"TimeDomain.",names(allData1), perl = TRUE)
names(allData1) <- gsub('^f',"FrequencyDomain.",names(allData1), perl = TRUE)
names(allData1) <- gsub('\\.mean',".Mean",names(allData1), perl = TRUE)
names(allData1) <- gsub('\\.std',".StandardDeviation",names(allData1), perl = TRUE)
names(allData1) <- gsub('Freq\\.',"Frequency.",names(allData1), perl = TRUE)
names(allData1) <- gsub('Freq$',"Frequency",names(allData1), perl = TRUE)


######################################################################################################################
# 4. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
######################################################################################################################

mean_by_act_sub = ddply(allData1, c("Subject","Activity"), numcolwise(mean))
#mean_by_act_sub =mean_by_act_sub[,-1]
write.table(mean_by_act_sub, file = "mean_by_act_sub.txt", row.names = FALSE)
