## Steps used to get and clean the data  in run_analysis.R
### 1 - Merge the training and the test sets to create one data set
* Read in the training and testing into separate sets
* Use **rbind()** to merge them to create one big data set

### 2 - Extracts only the measurements on the mean and standard deviation for each measurement
* Read in **feature names**
* Use **grepl()** to extract only feature names that have **"mean()"** or **"std()"**
	* grepl() in this process mistakenly accepts "meanFreq()", so we have to exclude the features whose names contain "meanFreq()"
* Extract those columns from the big data set along with given column names
* Read in **subject IDs** and add to each observation 

### 3 - Use descriptive activity names to name the activities in the data set
* Read in given activity labels (as **numbers**) and add to each observation
* Read in activity labels (as **strings**) and match those names with corresponding numbers added from the previous step
* Refine the activity names (as factor levels) to conform to the **name rules**
* Sort the data set by **subject ID** and then by **activity name**

### 4 - Appropriately label the data set with descriptive activity names
* Define a function that programmatically refines the feature names to conform to the **name rules** and make the names **descriptive**
* Apply the function to the column names of the data set

### 5 - Create a second, independent tidy data set with the average of each variable for each activity and each subject
* Create **180 groups (30 subjects * 5 activities)** corresponding to each combination of subject and activity
* Compute the mean of each feature for each group
* Add the computed numbers into the new data set, each group corresponds to a row/observation
