#Install library packages
install.packages("data.table")
library(data.table)
install.packages("reshape2")
library(reshape2)

# Load activity labels.txt:
activity_load <- read.table("C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/activity_labels.txt")[,2]
activity_load

# Load features.txt:
feature_load <- read.table("C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/features.txt")[,2]
feature_load

# Extracting the measurements (mean and standard deviation)
feature_extract <- grepl("mean|std", feature_load)
feature_extract

# Load & process test data.
X_test <- read.table("C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/test/X_test.txt")
y_test <- read.table("C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/test/y_test.txt")
subject_test <- read.table("C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/test/subject_test.txt")
X_test
y_test
subject_test

#Convert entire features into 1 variable
names(X_test) = feature_load
names(X_test)

# Finding the measurements (mean and standard deviation)
X_test = X_test[,feature_extract]
X_test

# Loading activity labels
y_test[,2] = activity_load[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"
names(y_test)
names(subject_test)

# Merge data with the respectively column
datatest <- cbind(as.data.table(subject_test), y_test, X_test)
#Show top 10 records of the datasets
head(datatest)

# Load & process train data
X_train <- read.table("C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/train/X_train.txt")
y_train <- read.table("C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/train/y_train.txt")
subject_train <- read.table("C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/train/subject_train.txt")

names(X_train) = feature_load
names(X_train)

# Find measurements
X_train = X_train[,feature_extract]
X_train

# Loading data
y_train[,2] = activity_load[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

# Bind data table
data_train <- cbind(as.data.table(subject_train), y_train, X_train)
data_train

# Bind test & train datasets
data_combined = rbind(datatest, data_train)
head(data_combined)

# Define ID/data labels and find the difference then melt data
ID_label = c("subject", "Activity_ID", "Activity_Label")
label_data = setdiff(colnames(data), ID_label)
melting_data = melt(data, id = ID_label, measure.vars = label_data)

melting_data


# Create dcast function
tidy_dataset   = dcast(melting_data, subject + Activity_Label ~ variable, mean)

#Save tidy data table
write.table(tidy_data, file = "C:/Users/200062/Downloads/R Assignment getting & cleaning Dataset/tidy_data.txt")

#Show tidy data table
show(tidy_data)