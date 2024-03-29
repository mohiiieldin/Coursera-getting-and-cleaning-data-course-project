 This project has 5 requriments are showen in details here and the Run_analysis.R script performes these data cleaning requirements on a windows 10 system.

requirements:
1)Merges the training and the test sets to create one data set.
2)Extracts only the measurements on the mean and standard deviation for each measurement.
3)Uses descriptive activity names to name the activities in the data set
4)Appropriately labels the data set with descriptive variable names.
5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

step1:
Download the files of the project:
 all requried files are downloded from UCL-Human Activity Recognition Using Smartphones Data Set:     http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The dataset includes the following files:
- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

For each record it is provided:
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

step2:
read every data file to a data frame using reading.csv :
 setting the working directory to the project directory , 
 activity_labels <- read.table("./activity_labels.txt") 6 rows and columns 2
 hold the activity name for each label (number) ,
 features <- read.table("./features.txt") 561 rows   2 columns
 hold the columns header for the test/train data and they are the features of each activity, 
 subject_test <- read.table("./test/subject_test.txt") 2947 rows and 1 column
 number indicating the subject that did the activity for the test data  
 test_set<- read.table("./test/x_test.txt") 2947 rows and 1 column
 the test data set
 test_labels<- read.table("./test/y_test.txt") 2947 rows and 1 column
 labels(numbers) that indicate for each observation on teh test data set what is the activity 
 subject_train <- read.table("./train/subject_train.txt") 7352 rows  1 columns
 number indicating the subject that did the activity for the train data  
 train_set<- read.table("./train/x_train.txt")  7352 rows  1 columns
 the train data set
 train_labels<- read.table("./train/y_train.txt")  7352 rows  1 columns
 labels(numbers) that indicate for each observation on teh test data set what is the activity

step3:
  1) add a coulmn that identify the row type test or train is called set_type to bind the train and test data
  2) bind the test and  train data sets using bind_rows in a data frame called all_data_set 

step4:
  1) filtering the features on the mean and the std only using grep() and make a filterd_features data.frame as a prepreation to merge the data 
  2) subsetting only the colmuns that on the mean and standared deviation for the train and test data frame and giving the coulmns header the feature names as a prepreation to merge the data
   
 step5:
  1)putting descriptive names to the column headers using names and mgsub functions 
 
 step6: 
  1)stacking test_subject and train_subject in one coulmn called subject in a data frame called all_subjects as a preperation to add it to the merged data set
  2)stacking the activity lables in one data frame called all_labels
  3)rename the column in all_labels from V1 to activity using the  chaining rule 
  4)substitute the activity numbers by the activity names using mgsub from the package(mgsub) 
step 7:
  1) add the all_subjects$subject column, all_labels$activity and set type column  to the all_data_set data frame  
  2)reordering the data columns by columns positions and name the data frame tidy data
 
step 8:
 1)grouping the data on activity and subject using group_by
 2)summarizing it by getting the average to all the numeric coulmns using summarise_all and save the result in a new data frame called average_tidy_data
 3)writting the average_tidy_data df in a txt file in order to put it in github
 