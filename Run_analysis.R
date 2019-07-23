library(dplyr)
library(tidyr)
library(mgsub)
#setting the working directory to the project directory and reading the data files 
setwd("E:/Coursera data science specialization/3. getting and cleaning data/course project/UCI HAR Dataset")

activity_labels <- read.table("./activity_labels.txt")
features <- read.table("./features.txt")

subject_test <- read.table("./test/subject_test.txt")
test_set<- read.table("./test/x_test.txt")
test_labels<- read.table("./test/y_test.txt")

subject_train <- read.table("./train/subject_train.txt")
train_set<- read.table("./train/x_train.txt")
train_labels<- read.table("./train/y_train.txt")

#add a coulmn that identify the row type test or train to bind the train and test data
test_set <- test_set %>% mutate(set_type = "Test")
train_set <- train_set %>% mutate(set_type = "Train")
all_data_set <-  bind_rows(train_set,test_set)
set_type <- all_data_set$set_type #to add it to the data frame again as it will be removed during the filtering on the mean and std

#filtering the features on the mean and the std only as a prepreation to merge the data
mean_index <- grep("mean",features$V2)
std_index <- grep("std",features$V2)
filtered_featured <- features[c( mean_index,std_index ),]

#subsetting only the colmuns that on the mean and standared deviation and giving the coulmns header the feature names
all_data_set <- all_data_set[,c( mean_index,std_index)]
names(all_data_set) <- filtered_featured[,"V2"]

#putting descriptive names to the column headers  
names(all_data_set) <-  mgsub( names(all_data_set),c("^t","^f","Acc","-mean\\(\\)","-meanFreq\\(\\)","Gyro","Mag","std\\(\\)"),
                               c("TimeDomain","FrequencyDomain","Accelerometer","Mean","Mean","Gyroscope","Magnitude","STD") 
                            )

#stacking test_subject and train_subject in one coulmn called subject as a preperation to add it to the merged data set
all_subjects <- bind_rows(subject_train ,subject_test)

#stacking the activity lables and substitute the activity number by the activity name 
all_labels <- bind_rows(train_labels,test_labels) %>% rename(activity=V1)
all_labels$activity <- mgsub(as.character(all_labels$activity),c("1","2","3","4","5","6"),
                             c("Walking","Walking_Upstairs","Walking_Downstairs","Sitting","Standing","Laying") )
 
#all_data_set <- mutate(all_data_set, Subject = all_subjects$V1,Activity = all_labels$activity , Set_type = set_type) 

all_data_set[,c("subject","activity","set_type")] = c(all_subjects$V1 , all_labels$activity,set_type)
tidy_data <- all_data_set[,c(82,81,80,1:79)]
averaged_tidy_data <- tidy_data[,2:82] %>% group_by(activity,subject) %>% summarise_all(mean) 

#writting the tidy data in a txt file :
write.table(averaged_tidy_data,file = "./averge_tidy_data.txt",row.names = FALSE)