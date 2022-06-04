#setting working directory
setwd("/Users/vgovindan/Desktop/UMD I:O PSYC MPS/classes/PSYC652/Group Project")
#reading in data
teachrate <- read.csv("TeachingRatings.csv")
#gender to 0= male and 1=female

####Independent Samples t-test####
teachrate$gender <- as.factor(teachrate$gender)
t.test(teachrate$eval ~ teachrate$gender)

#plotting data                        
library(ggplot2)
library(ggpubr)

ggbarplot(teachrate, x = "gender", y = "eval", add= "mean_se", fill = "gender",
          palette = c("pink", "blue"))+
  ylab("Evaluation Score") + xlab("Gender") + ggtitle("Gender and Evaluations") +
  stat_compare_means(method = "t.test") +
  theme(legend.position = "none") 

####Regression####
reg <- lm(teachrate$eval ~teachrate$beauty)
summary(reg)

#plotting data
plot(teachrate$beauty, teachrate$eval)
abline(reg, col="red")

ggplot(teachrate, aes(x = beauty, y=eval)) + geom_point(color = "blue") + geom_smooth(method = 'lm', color= "red") +
ylab("Teacher Evaulation") + xlab("Beauty Scores") + ggtitle("Beauty and Evaluations")

####Correlation####
cor(final.dat$beauty, final.dat$eval)
## [1] 0.1890391
cor(final.dat$age, final.dat$eval)
## [1] -0.05169619
cor(final.dat$beauty, final.dat$age)
## [1] -0.2978925

#creating correlation table 
Library(apaTables)
cor.dat<-final.dat[c(3,6,7)]
apa.cor.table(cor.dat)

