#You should create one R script called run_analysis.R that does the following. 

#Merges the training and the test sets to create one data set.

#Extracts only the measurements on the mean and standard deviation for 
#each measurement. 

#Uses descriptive activity names to name the activities in the data set

#Appropriately labels the data set with descriptive variable names. 

#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

###########################################################################

#Step 1: Extracting and preparing datasets

###########################################################################

#Prepare workspace
setwd("~/Week 4/Getting-and-Cleaning-Data")
library(dplyr)

# Read the datasets (assuming they are comma-separated)

read.table_instructions <- list(
  file = list(
  activity_labels = "UCI HAR Dataset/activity_labels.txt",
  features = "UCI HAR Dataset/features.txt",
  subject_train = "UCI HAR Dataset/train/subject_train.txt",
  y_train = "UCI HAR Dataset/train/y_train.txt",
  X_train = "UCI HAR Dataset/train/X_train.txt",
  subject_test = "UCI HAR Dataset/test/subject_test.txt",
  y_test = "UCI HAR Dataset/test/y_test.txt",
  X_test = "UCI HAR Dataset/test/X_test.txt"
  ), 
  
  colClasses = list(
    activity_labels = c("integer", "character"),
    features = c("integer", "character"),
    subject_train = "integer",
    y_train = "integer",
    X_train = rep("numeric", 561),
    subject_test = "integer",
    y_test = "integer",
    X_test = rep("numeric", 561)
  ),
  
  nrows = list(
    activity_labels = 6,
    features = 561,
    subject_train = 7352,
    y_train = 7352,
    X_train = 7352,
    subject_test = 2947,
    y_test = 2947,
    X_test = 2947
  )
)

data_files <- with(read.table_instructions,
                   Map(read.table,
                       file = file, colClasses = colClasses, nrows = nrows,
                       quote = "", comment.char = "",
                       stringsAsFactors = FALSE))


## Task 1: Merging the training and the test sets to create one data set.

merged_data <- with(data_files,
                    rbind(cbind(subject_train, y_train, X_train),
                          cbind(subject_test,  y_test,  X_test)))

## Task 2: Extracting only the measurements on the mean and
## standard deviation for each measurement

## Find the target features indexes from the 'features' data frame,
## by searching for matches with pattens 'mean()' or 'std()'

target_features_indexes <- grep("mean\\(\\)|std\\(\\)",
                                data_files$features[[2]])

## Add 2 to each index to adjust for the first 2 column we have bind
## that should also be included
target_variables_indexes <- c(1, 2, # the first two columns that refer to
                              # 'subject' and 'activity'
                              # should be included
                              # adds 2 to correct the indexes
                              # of target features indexes because of
                              # the 2 extra columns we have included
                              target_features_indexes + 2)

target_data <- merged_data[ , target_variables_indexes]

## Task 3: Uses descriptive activity names to name the 
## activities in the data set

## Replace activity values with a factor based on levels and labels
## contained in the activity_labels data file.
target_data[[2]] <- factor(target_data[[2]],
                           levels = data_files$activity_labels[[1]],
                           labels = data_files$activity_labels[[2]])

## Task 4: Appropriately labels the data set with descriptive variable names

## Extract the target variables names
descriptive_variable_names <- data_files$features[[2]][target_features_indexes]

## Correct a typo
descriptive_variable_names <- gsub(pattern = "BodyBody", replacement = "Body",
                                   descriptive_variable_names)

## Create a tidy data set with appropriate labels for the variable names
tidy_data <- target_data
names(tidy_data) <- c("subject", "activity", descriptive_variable_names)

## Task 5: From the data set in step 4, creates a second, independent tidy 
## data set with the average of each variable for each activity and 
## each subject.

## Create a dataset with the mean of each column for 'subject' and 'activity'
tidy_data_summary <- tidy_data %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean = ~mean(.))) %>%
  ungroup()

## Replace the variable names of 'tidy_data_summary' with new descriptive ones.
## Just the prefix "Avrg-" will be added in all variable names,
## except the first two, 'subject' and 'activity'.
new_names_for_summary <- c(names(tidy_data_summary[c(1,2)]),
                           paste0("Avrg-", names(tidy_data_summary[-c(1, 2)])))
names(tidy_data_summary) <- new_names_for_summary

## Save the data frame created as a text file in working directory
write.table(tidy_data_summary, "tidy_data_summary.txt", row.names = FALSE)
