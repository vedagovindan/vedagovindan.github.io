##read in data
setwd("/Users/vgovindan/Desktop/UMD I:O PSYC MPS/classes/PSYC654/Group Project")
hdata <- read.csv("HRDataset.csv")

######################### mediation #########################

##we are testing if Predictor Variable (PV) -> Mediation Variable (MV) -> Criterion Variable (CV)
# Engagement (E) -> Performance Score (PS) --> Salary (S)
#Does E predict PS (+ beta), which S (+ beta)? 

#get rid of NAs
hdata<-na.omit(hdata)

##step 1: total effect (PV -> CV)  but in R: (CV ~ PV)
toteff <- lm(Salary ~ EngagementSurvey, data = hdata)
summary(toteff)
# p = .23 --> Predictor does not significantly predict outcome 

##step 2: PV -> MV  but in R: (MV ~ PV)
s2 <- lm(PerfScoreID ~ EngagementSurvey, data = hdata)
summary(s2)
#p < .05 --> Predictor does significantly predict mediator 

#step 3: MV + PV -> CV but in R: (CV ~ PV + MV)
s3<-lm(Salary ~ EngagementSurvey + PerfScoreID, data = hdata)
summary(s3)
#p< .03 --> When holding PV constant, MV does significantly predicts CV
#p= .90 --> When holding MV constant, PV does not significantly predicts CV
#thus, no mediation is present b/c s2 is wrong 

## To confirm no mediation, perform sobel test, which follows the order mediation.test(MV, PV, CV)
library(bda)
mediation.test(hdata$PerfScoreID, hdata$EngagementSurvey, hdata$Salary)
#Sobel test p value = .04 --> Despite significant Sobel results, MV does not mediate the relationship between PV and CV

#Hence, Performance Score does not mediate the relationship between Engagement and Salary

######################### moderation #########################
##we are testing if PV -> CV and if there is a moderator (Mo) that changes that relation
#Engagement (E) --> Salary (S); Gender (G) as moderator
#Does E predict S (+); are they moderated by G (+)? 

#Without looking at moderation, does PV and Mo predict CV? 
#Test if: CV ~ PV + Mo 
reg <- lm(Salary ~ EngagementSurvey + GenderID , data = hdata)
summary(reg)
# p= .22--> When holding Mo constant, PV does not predict CV
# p= .33 --> When holding PV constant, Mo does not predict CV

#There is probably no moderation, but let's double check with interaction analysis 
#Test if: CV ~ PV + Mo + IV*Mo
modreg <- lm(Salary ~ EngagementSurvey + GenderID + EngagementSurvey*GenderID , data = hdata)
summary(modreg)
# p = .07 --> there is no significant interaction between IV and Mo on CV
# p= .03 --> When holding Mo & interaction constant, PV does predict CV
# p= .04 --> When holding PV & interaction constant, Mo does predict CV

#There is no moderation of Gender on the relationship between Engagement and Salary
#No probing required, but we can do one just to explore
library(interactions)
probe_interaction(model = modreg, pred = EngagementSurvey, modx = GenderID, data = hdata)

####################  K-means Clustering ####################  
cdata <- read.csv("HRDataset.csv")

##Ask
##renames the rows as the department and then gets rid of department column
#rownames(cdata)<-cdata$Department
#cdata<-cdata[,-c(26)]

#omitting redundant variables 
cdata<-cdata[,-c(1:4, 6, 9, 11, 13:30, 33:35 )]

#omitting NAs in data
cdata<-na.omit(cdata)

#scale numerical values in data 
#####error######
library(dplyr)
library(vars)
cdata <- cdata %>%           
  mutate_at(c("Salary", "EngagementSurvey", "EmpSatisfaction", "Absences"),
            ~(scale(.) %>% as.vector))

library(factoextra)
fviz_nbclust(cdata, kmeans, method = 'silhouette')
###  optimal clusters: 2 

##creating 2 clusters 
ckm<-kmeans(cdata, 2)

#calculating mean of clusters
ckm$centers
#Big differences were seen in gender, salary and position

#calculating size of each group
ckm$size
#Group 1 has 255 employees, group 2 has 56 employees

##graphing results
fviz_cluster(ckm, data = cdata)











