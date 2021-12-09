
#   TASK 1 : Discovery


# 1-read and load the dataset
data <- read.csv(file.path("c:", "kidney_disease.csv"))
data

# 2-remove alumin and pus cell attributes, The '-' sign indicates dropping variables  
new_data=subset(data, select=-c(al,pc))
new_data

# 3-Check the structure of the data 

# numbers of instances & attributes
dim(new_data)

#check the datatypes 
str(new_data)

#number of missing values
# returns a vector (F F F T)
is.na(new_data)
#sum of missing values
sum(is.na(new_data))

# 4-Provide general statistical description

summary(new_data)

# 5-Visualize the data in three different ways.

# boxplot 
boxplot(age ~ su, data = new_data, xlab = "sugar",
        ylab = "age", main = "age vs sugar")

#histogram 
hist(new_data$age, xlab="age", col= "red")

#Scatterplot 
my_cols <- c("#00AFBB", "#E7B800")
plot(x = new_data$age, y = new_data$hemo,
     xlab = "age",
     ylab = "hemoglobin",
     main = "age vs hemoglobin",col = my_cols
)






#data cleaning

# fill the NA of numerical varible with median
new_data$bp[is.na(new_data$bp)]<-median(new_data$bp,na.rm=TRUE)
new_data

new_data$sg[is.na(new_data$sg)]<-median(new_data$sg,na.rm=TRUE)
new_data

new_data$su[is.na(new_data$su)]<-median(new_data$su,na.rm=TRUE)
new_data

new_data$bgr[is.na(new_data$bgr)]<-median(new_data$bgr,na.rm=TRUE)
new_data

new_data$bu[is.na(new_data$bu)]<-median(new_data$bu,na.rm=TRUE)
new_data

new_data$sc[is.na(new_data$sc)]<-median(new_data$sc,na.rm=TRUE)
new_data

new_data$sod[is.na(new_data$sod)]<-median(new_data$sod,na.rm=TRUE)
new_data

new_data$pot[is.na(new_data$pot)]<-median(new_data$pot,na.rm=TRUE)
new_data

new_data$hemo[is.na(new_data$hemo)]<-median(new_data$hemo,na.rm=TRUE)
new_data

new_data$age[is.na(new_data$age)]<-median(new_data$age,na.rm=TRUE)
new_data

#check NA for some numerical varible - and the result show there is no missing value
sum(is.na(new_data$hemo))
sum(is.na(new_data$pcv))
sum(is.na(new_data$wc))
sum(is.na(new_data$rc))

#convert categorical varible to binary ( 0 and 1 ) 

new_data$rbc <- ifelse(new_data$rbc == "normal",1,0)
new_data

new_data$pcc <- ifelse(new_data$pcc == "present",1,0)
new_data

new_data$ba <- ifelse(new_data$ba == "present",1,0)
new_data

new_data$htn <- ifelse(new_data$htn == "yes",1,0)
new_data

new_data$dm <- ifelse(new_data$dm == "yes",1,0)
new_data

new_data$cad <- ifelse(new_data$cad == "yes",1,0)
new_data

new_data$appet <- ifelse(new_data$appet == "good",1,0)
new_data

new_data$pe <- ifelse(new_data$pe == "yes",1,0)
new_data

new_data$ane <- ifelse(new_data$ane == "yes",1,0)
new_data

new_data$classification <- ifelse(new_data$classification == "ckd",1,0)
new_data

#drop id colunm because it is not important
updated_data=subset(new_data, select = -c(id) )
updated_data









complete.cases(updated_data)
sum(complete.cases(updated_data))
updated_data


updated_data$pcv <- as.factor(updated_data$pcv)
updated_data$wc <- as.factor(updated_data$wc)
updated_data$rc <- as.factor(updated_data$rc)
str(updated_data)

updated_data$pcv <- as.numeric(updated_data$pcv)
updated_data$wc <- as.numeric(updated_data$wc)
updated_data$rc <- as.numeric(updated_data$rc)
str(updated_data)





#  TASK 2: Hypothesis Testing


hypo_testing <- aov(su~classification, data=updated_data)
summary(hypo_testing)







# TASK 3 :  Model Building & Evaluation


#   LOGISTIC REGRESSION :


L_data<-updated_data[,c(-3,-4,-5,-6,-7,-8,-11,-12,-13,-14,-17,-18,-20,-21,-22)]
L_data


#split the data into 2 sets, first one takes 70% of the data which is a training set
# the second takes 30% which is a testing set

set.seed(2)
random <- sample(2, nrow(L_data), replace = T, prob = c(0.7, 0.3))
d_train <- L_data[random == 1,]
d_test <- L_data[random == 2,]
d_train
d_test

#fit the Logistic Regression method

logistic <- glm(classification~.,data =d_train, family= binomial)
summary(logistic)


#prediction 
pred <- predict(logistic, d_test, type = "response")
pred

#Testing 
library(Metrics)

#Mean Squared Error (MSE)
mean_squared_error <- mse(L_data$classification, pred)
mean_squared_error

#Mean Absolute Error (MAE)
mean_abs_error <- mae(L_data$classification, pred)
mean_abs_error

#Mean Absolute Deviation
mean_abs_deviation <- mean_abs_error/299
mean_abs_deviation


#plot the ROC and calculate the AUC

library(pROC)
auc(d_test$classification, pred)

plot(roc(d_test$classification, pred, direction="<"),
     col="yellow", lwd=3, main="ROC")





# Naive Bayes :
updated_data$pcv <- as.factor(updated_data$pcv)
updated_data$wc <- as.factor(updated_data$wc)
updated_data$rc <- as.factor(updated_data$rc)
str(updated_data)

updated_data$pcv <- as.numeric(updated_data$pcv)
updated_data$wc <- as.numeric(updated_data$wc)
updated_data$rc <- as.numeric(updated_data$rc)
str(updated_data)




NB_data <- updated_data[, c(-3,-4-5,-6,-7,-8,-11,-12,-13,-14,-17,-18,-20,-21,-22)]
NB_data

str(NB_data)

NB_data$classification <- as.factor(NB_data$classification)
str(NB_data)


library(e1071)
library(caret)

#split the data into 2 sets, first one takes 70% of the data which is a training set
# the second takes 30% which is a testing set
set.seed(2)
random <- sample(2, nrow(NB_data), prob = c(0.7, 0.3), replace = T)
data_train <- NB_data[random == 1, ]
data_test <- NB_data[random == 2, ]
data_train
data_test


#fit the naive bayes method
data_nb <- naiveBayes(classification ~ . , data  = data_train)
data_nb

#predict
pred_nb <- predict(data_nb, data_test)
pred_nb

#print metrics
confusionMatrix(pred_nb, data_test$classification)






#Decision Tree :



DT_data <- updated_data[, c(-3,-4-5,-6,-7,-8,-11,-12,-13,-14,-17,-18,-20,-21,-22)]
DT_data

str(DT_data)


#  convert the classification ( ckd or not)  to factor (yes & no)
DT_data[DT_data$classification ==1,]$classification = "yes"
DT_data[DT_data$classification ==0,]$classification = "No"

DT_data$classification = as.factor(DT_data$classification)
str(DT_data)

# Explore it 
head(DT_data,1)
summary(DT_data)
No_Obs = nrow(DT_data)

#split the data into 2 sets, first one takes 70% of the data which is a training set
# the second takes 30% which is a testing set
DT = sample(2, No_Obs, replace = TRUE, prob = c(0.7,0.3))
DT_training_set <- DT_data[DT==1, ] 
DT_testing_set <- DT_data[DT ==2, ]

#draw the tree using the training set 
library(rpart)
library(rpart.plot)

model_DT = rpart(DT_training_set$classification~.,DT_training_set)
print(model_DT)

# visualize the tree
rpart.plot(model_DT)


# predict the classification (ckd or not) for the testing set
# Explore the test set
head(DT_testing_set,1)
summary(DT_testing_set)

test_result = predict(model_DT,DT_testing_set, type="class")
test_result


#Evaluate the model using confusion matrix
CM = confusionMatrix(test_result,DT_testing_set$classification)
CM
conf <- table(actual = DT_testing_set$classification, predicted= test_result )
accuracy= sum(diag(conf))/ sum(conf)
print(accuracy)
percision= conf[1,1]/colSums(conf)[1]
print(percision)
recall_DT = conf[1,1]/rowSums(conf)[1]
print(recall_DT)

