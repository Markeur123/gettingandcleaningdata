# 1 - Merges the training and the test sets to create one data set

## Loading data sets
train <- read.table("./train/X_train.txt")
test <- read.table("./test/X_test.txt")

## Add into one big set
dataset <- rbind(train, test)

# 2 - Extracts only the measurements on the mean and standard deviation for each measurement

## Feature/variable names
features <- read.table("./features.txt", stringsAsFactor=FALSE)

## Getting measurements on Mean and Standard deviation
##  based on feature names
features.new <- features[(grepl("mean()", features$V2) | grepl("std()", features$V2)), ]

### Exclude meanFreq()
features.new <- features.new[!grepl("meanFreq()", features.new$V2), ]

## Extract above measurements from the big data set
dataset.new <- dataset[, features.new$V1]

## Set new names
colnames(dataset.new) <- features.new$V2

## Adding subject id to each row
subject.train <- read.table("./train/subject_train.txt")
subject.test <- read.table("./test/subject_test.txt")
dataset.new$subject <- rbind(subject.train, subject.test)$V1

# 3 - Uses descriptive activity names to name the activities in the data set

## Adding activity label to each row
label.train <- read.table("./train/y_train.txt")
label.test <- read.table("./test/y_test.txt")
dataset.new$label <- rbind(label.train, label.test)$V1

## Match the activity numbers with appropriate activity names provided

### Read in activity labels
activity.names <- read.table("./activity_labels.txt")

### Merge the two data sets to get the label names
dataset.new <- merge(dataset.new, activity.names, by.x="label", by.y="V1", all=T)

### Remove label number field and rename new field
dataset.new$label <- NULL
colnames(dataset.new)[68] <- "activitylabels"

### Refine factor level names
levels(dataset.new$activitylabels) <- tolower(levels(dataset.new$activitylabels))
levels(dataset.new$activitylabels) <- sub("_", "", levels(dataset.new$activitylabels))

## Sort the dataset by subject id and then by activity name
dataset.new <- dataset.new[order(dataset.new$subject, dataset.new$activitylabels), ]

# 4 - Appropriately labels the data set with descriptive column names
source("NRfeatures.R")
colnames(dataset.new) <- sapply(colnames(dataset.new), NRfeatures)

# 5 - Creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.
tidy <- dataset.new
tidy$activitylabels <- as.character(tidy$activitylabels)

## Group the observations by subject and activity (180 groups = 30 subjects * 6 activities)
tidy$grp <- paste(tidy$subject, tidy$activitylabels)
tidy$grp <- as.factor(tidy$grp)

## Create the final tidy data frame
tidyset=data.frame(1:180)

## Iteratively add each feature/field to the data frame
for (i in 1:66) {
  tidyset[, colnames(tidy)[i]] <- tapply(tidy[, i], tidy$grp, mean)
}

## Rename each row to corresponding group name
rownames(tidyset) <- unique(tidy$grp)

## Unnecessary variable
tidyset[, 1] <- NULL

## Export the tidy set
write.table(tidyset, file="tidy.txt", sep="\t")
