#Final Project 

#set working directory 
setwd("/Users/vgovindan/Desktop/UMD I:O PSYC MPS/classes/PSYC654/Group Project")

#read in data
hrdata <- read.csv("HRDataset.csv")

###########CLASSIFIER 1
#KNN Model 
hrdata <- read.csv("HRDataset.csv")
#Recoding: If Employee status is 1,2, or 3 (all actively employed) code as 1 else (employee who have been fired or left) code as 0 
hrdata$EmpStatusID<-ifelse(hrdata$EmpStatusID == 1 | hrdata$EmpStatusID == 2| hrdata$EmpStatusID ==  3, 1, 0)

#download necessary package
library(dplyr)
#remove unnecessary data columns 
hrdata<-hrdata[,-c(1:5,7, 9:30, 33:36)]
#scale appropriate variables as vectors
hrdata_scale <- hrdata %>%    
  mutate_at(c("EngagementSurvey", "EmpSatisfaction", "PerfScoreID"), ~(scale(.) %>% as.vector))

#read in Employee Status as factor 
hrdata_scale$EmpStatusID<-as.factor(hrdata_scale$EmpStatusID)

#split into training and testing set
hrtrains<-hrdata_scale[c(1:250),]
hrtests<-hrdata_scale[-c(1:250),]

#running KNN, with k=9 
#download necessary package
library(class)
knnhr<-knn(train = hrtrains, test = hrtests, cl = hrtrains$EmpStatusID, k = 9) 

#checking for accuracy
table(hrtests$EmpStatusID, knnhr, dnn = c("actual", "prediction"))

#create confusion matrix
#download necessary package
library(caret)
confusionMatrix(data = knnhr, reference = hrtests$EmpStatusID)
#Results
#accuracy = 93.44%
#Sensitivity : 80.95%          
#Specificity : 100%

###########CLASSIFIER 2 
#DT

#download needed packages
library(ISLR)
library(tree)
library(rpart)
library(rpart.plot)

#read in data
hrdata1 <- read.csv("HRDataset.csv")
#clean up data set
#Re-coding: If Employee status is 1,2, or 3 (all actively employed) code as 1 else (employee who have been fired or left) code as 0 
hrdata1$EmpStatusID<-ifelse(hrdata1$EmpStatusID == 1 | hrdata1$EmpStatusID == 2| hrdata1$EmpStatusID ==  3, 1, 0)

#remove unnecessary data columns 
hrdata1<-hrdata1[,-c(1:5,7, 9:30, 33:36)]

#make Employee Status a factor
hrdata1$EmpStatusID <- as.factor(hrdata1$EmpStatusID)

#split into train & test data 
hrdata1train<-hrdata1[c(1:250),]
hrdata1test<-hrdata1[-c(1:250),]

#create tree with training set
hrdata1tree <- rpart(EmpStatusID  ~., hrdata1train)
#plot tree
rpart.plot(hrdata1tree)

#predicting using test set
predhrdata1 <- predict(hrdata1tree, newdata = hrdata1test, type="class")

#check how well data does
table(hrdata1test$EmpStatusID, predhrdata1)

#create confusion matrix
#download necessary package
library(caret)
confusionMatrix(data = predhrdata1, reference = hrdata1test$EmpStatusID)

#RESULTS
#Accuracy : 49.18% 
#Sensitivity : 09.52%       
#Specificity : 70.00%        

######CLASSIFIER 3 #######
##NBB
#read in data
hrdata2 <- read.csv("HRDataset.csv")
#Re-coding: If Employee status is 1,2, or 3 (all actively employed) code as 1 else (employee who have been fired or left) code as 0 
hrdata2$EmpStatusID<-ifelse(hrdata2$EmpStatusID == 1 | hrdata2$EmpStatusID == 2| hrdata2$EmpStatusID ==  3, 1, 0)

#remove unnecessary data columns 
hrdata2<-hrdata2[,-c(1:3, 7:30, 32:36)]

#factoring gender, marital status and employee status
hrdata2$GenderID <- as.factor(hrdata2$GenderID )
hrdata2$MaritalStatusID <- as.factor(hrdata2$MaritalStatusID )
hrdata2$EmpStatusID <- as.factor(hrdata2$EmpStatusID )

#split into train & test data 
hrdata2train<-hrdata2[c(1:250),]
hrdata2test<-hrdata2[-c(1:250),]

#download necessary package
library(e1071)
#running the nb model 
nb<-naiveBayes(EmpStatusID ~., data = hrdata2train) #predict if person is employed

#predicting employee status
pred<-predict(nb, newdata = hrdata2test, response = "type")

#checking accuracy
table(hrdata2test$EmpStatusID, pred, dnn =c("actual", "prediction"))

#download necessary package
library(caret)
#create confusion matrix
confusionMatrix(data = pred, reference = hrdata2test$EmpStatusID) 

#RESULTS
#Accuracy : 60.66%
#Sensitivity : 19.048%         
#Specificity : 82.50% 

######END######

