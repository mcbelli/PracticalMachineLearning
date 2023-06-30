# Coursera Practical Machine Learning, week 4 project
check.packages <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
  if (length(new.pkg))  
    install.packages(new.pkg, dependencies = TRUE, repo="https://mirrors.nics.utk.edu/cran/") 
  sapply(pkg, require, character.only = TRUE) 
} 
# Usage example 
#packages<-c("ggplot2", "afex", "ez", "Hmisc", "pander", "plyr") 
#packages<-c("sqldf","mlogit","rmarkdown") 
packages<-c("sqldf","lattice","ggplot2"
            ,"lubridate","caret") #"pgmm","rpart","rattle","stats","randomForest","ElemStatLearn","httr","forecast","e1071","elasticnet"
check.packages(packages)
#"fable","fpp3","tsibbledata","hts","datarium","randomForest"
# load data
local_folder <- 'C:/Users/MIke/Documents/Git/datasciencecoursera/PracticalMachineLearning_Project/'
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
local_file_train <- paste0(local_folder,'pml-training.csv')
download.file(train_url, local_file_train, mode="wb")
train_data <- read.csv(local_file_train,stringsAsFactors = FALSE)
  
local_file_test <- paste0(local_folder,'pml-testing.csv')
download.file(test_url, local_file_test, mode="wb")
test_data <- read.csv(local_file_test,stringsAsFactors = FALSE)

mydim <- dim(train_data)

#split into traintrain and traintest
set.seed(20230528)
train_data$var_num <- runif(dim(train_data)[1])
traintrain <- train_data[train_data$var_num<(.7),]; traintrain$var_num <- NULL
#train2 <- train_data[train_data$var_num>=.4 & train_data$var_num<.7,]; 
#train2$var_num <- NULL
traintest <- train_data[train_data$var_num>=.7,] ; traintest$var_num <- NULL

#listMissingColumns <- colnames(dataframe)[ apply(dataframe, 2, anyNA)]
listMissingColumns <- colnames(traintrain)[ apply(traintrain, 2, anyNA)]
#summary(traintrain[listMissingColumns])
# there are exactly 7674 missing for all these, out of 7837 rows
table(traintrain$classe)
length(traintrain$classe[is.na(traintrain$classe)])
length(traintrain$classe[is.na(traintrain$var_yaw_forearm)])
# valid seem to be 	roll_belt	pitch_belt	yaw_belt	total_accel_belt
# with num_window denoting session or attempt
traintrainb <- traintrain[c("classe","roll_belt","pitch_belt","yaw_belt","total_accel_belt")]
summary(traintrainb$roll_belt)


# after dealing with missing, preprocess by normalizing
# subtract the mean and divide by the sd, to get a mean of 0
# and an average SD of 1
preproc_obj <- preProcess(traintrainb[,-1],method=c("center","scale"))
# now apply the preprossing to the traintrainb to get the centered and scaled version of the traintrainb set
# do not bother for rf

# use trainControl to cross validate
# 10-fold
control <- trainControl(method='cv', 
                        number=10)

fit <- caret::train(classe ~ ., method="rf", trControl=control, data=traintrainb)
#summary(fit)
traintrainb$p_classe <- predict(fit,data=traintrainb)

check_acc <- as.data.frame(table(traintrainb$p_classe,traintrainb$classe))
names(check_acc)[names(check_acc)=='Var1'] <- 'p_classe'
names(check_acc)[names(check_acc)=='Var2'] <- 'classe'
ls(check_acc)

sqldf('select classe,sum(Freq) as true_number
,sum(case when p_classe=classe then Freq else 0 end) as correct_number
,100*(sum(case when p_classe=classe then Freq else 0 end))/sum(Freq) as pct_correct
  from check_acc
      group by classe order by classe')
traintrainb$p_classe <- NULL

# now apply to traintest and check accuracy

traintestb <- traintest[c("classe","roll_belt","pitch_belt","yaw_belt","total_accel_belt")]
names(traintestb)
traintestb$p_classe <- predict(fit,newdata=traintestb)
table(traintestb$p_classe,traintestb$classe)
check_acc2 <- as.data.frame(table(traintestb$p_classe,traintestb$classe))
names(check_acc2)[names(check_acc2)=='Var1'] <- 'p_classe'
names(check_acc2)[names(check_acc2)=='Var2'] <- 'classe'
ls(check_acc2)


sqldf('select classe,sum(Freq) as true_number
,sum(case when p_classe=classe then Freq else 0 end) as correct_number
,100*(sum(case when p_classe=classe then Freq else 0 end))/sum(Freq) as pct_correct
  from check_acc2
  group by classe order by classe'
      )




#check_acc_all <- check_acc2
#check_acc_all$num <- 10
#check_acc2$num <- 3
#check_acc_all <- rbind(check_acc_all,check_acc2)
#sqldf('select classe,num,sum(Freq) as true_number
#,sum(case when p_classe=classe then Freq else 0 end) as correct_number
#,100*(sum(case when p_classe=classe then Freq else 0 end))/sum(Freq) as pct_correct
#  from check_acc_all
#  group by classe,num order by classe,num')

#check_acc2 <- NULL
#check_acc <- NULL















