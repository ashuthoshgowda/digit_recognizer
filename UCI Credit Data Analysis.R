

library(GGally)
library("ggthemes")
library(caTools)
library(caret)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library(gtools) # for discretisation
library(corrplot)
library(Hmisc)
library(devtools)
library(PerformanceAnalytics)
library(FactoMineR)


fdata <- read.csv("C:/.../UCI_Credit_Card.csv")
summary(creditcard)
head(creditcard)
str(fdata)
factor_vars <- c('SEX','EDUCATION','MARRIAGE','default.payment.next.month')
fdata[factor_vars] <- lapply(fdata[factor_vars], function(x) as.factor(x))

fdata$EDUCATION[fdata$EDUCATION == 4 | fdata$EDUCATION == 5  | fdata$EDUCATION == 6 ]<- 0

hist(fdata$AGE,col = "turquoise",freq  = 2,main = "histogram & distrubution of client's age",breaks = 10,xlab = "Age of the client", ylab = "Frequency")
#education distribution
plot(fdata$EDUCATION,col = "turquoise",main = "histogram & distrubution of client's Education",xlab = "Education of the client", ylab = "Frequency")
#Sex Distribution
plot(fdata$EDUCATION,col = "turquoise",main = "histogram & distrubution of client's Education",xlab = "Education of the client", ylab = "Frequency")

boxplot(fdata$LIMIT_BAL, col = "turquoise", xlab="LIMIT Balance", ylab="Distribution Range", main="LIMIT BALANCE DISTRIBUTION")

#Age Distribution wrt Default Payment
ggplot(fdata, aes(x = AGE, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Age') +
  theme_excel()

#Education Distribution wrt Default Payment
ggplot(fdata, aes(x = PAY_2, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'PAY_2') +
  theme_excel()

M<-as.data.frame.matrix(table(fdata$SEX,fdata$default.payment.next.month))
M[3,]<-M[1,]/M[2,]

#SEX Distribution wrt Default Payment
ggplot(fdata, aes(x = SEX, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Sex', main ="Distribution of Sex w.r.t Defaulters") +
  theme_excel()

#Marraige Distribution wrt Default Payment
ggplot(fdata, aes(x = MARRIAGE, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Marriage') +
  theme_few()

#Education Distribution wrt Marriage
ggplot(fdata, aes(x = EDUCATION, fill = MARRIAGE)) +
  geom_bar() +
  labs(x = 'Education') +
  theme_excel()

#Box Plot of Marriage 
plot(fdata$MARRIAGE,fdata$LIMIT_BAL, xlab = 'Marriage', ylab = 'Limit Balance')

plot(fdata$EDUCATION,col="turquoise",fdata$LIMIT_BAL, xlab = 'Education', ylab = 'Limit Balance')


boxplot(f)
table(fdata$EDUCATION)

#Chi Squared Tests
chisq.test(fdata$SEX,fdata$default.payment.next.month, correct = FALSE)
chisq.test(fdata$EDUCATION,fdata$default.payment.next.month, correct = FALSE)
chisq.test(fdata$MARRIAGE,fdata$default.payment.next.month, correct = FALSE)
chisq.test(fdata$MARRIAGE,fdata$EDUCATION, correct = FALSE)

head(fdata)
#finding Correlation between Billamts
df <- fdata[,c(2,13,14,15,16,17,18,19,20,21,22,23,24)]
head(df)
M<-cor(df)
corrplot(M,method = "circle")

#apply PCA on BILLAMT
dfpca=fdata[,c(13,14,15,16,17,18)]
pca=prcomp(dfpca)
plot(pca,col = "turquoise" )
pca$rotation=-pca$rotation
pca$x=-pca$x
biplot (pca , scale =0)
head(pca$x)
pca$PC1

#replacing PCA in fdata
fdata[,13:14]<-PCA[,1:2]
fdata[,15:18]<-NULL
colnames(fdata)[13]<-"BILLPCA1"
colnames(fdata)[14]<-"BILLPCA2"

str(fdata)
dim(fdata)

#splitting the Data
set.seed(12345)
split_log <- sample.split(fdata$default.payment.next.month,SplitRatio=0.7)
dataTrain_log=subset(fdata,split_log==TRUE)
dataTest_log=subset(fdata,split_log==FALSE)
table(dataTrain_log$default.payment.next.month)
table(dataTest_log$default.payment.next.month)

#Building Model
Log_reg_refined<-glm(dataTrain_log$default.payment.next.month ~ LIMIT_BAL + SEX +
                       EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_5 +
                       BILLPCA1 + BILLPCA2 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 +
                       PAY_AMT5,family=binomial(logit),data=dataTrain_log)

pred = predict(Log_reg_refined,newdata=dataTest_log,type="response")

#creating an Array of Cutoff Values from 0 to 1, in steps of 0.05
Cutoff<- seq(0,1,0.01)

#Running a loop 20 times to update Sensitivity and Specificity from corresponding Cutoffs
for(i in 1:101){
  pred = predict(Log_reg_refined,newdata=dataTest_log,type="response")
  cut_off<-Cutoff[i]
  pred[pred<cut_off]<-0
  pred[pred>=cut_off]<-1
  C<-confusionMatrix(pred,dataTest_log$default.payment.next.month)
  #Calculating the Specificity and Sensitivity from the Confusion matrix
  CM<-as.data.frame.matrix(C$table)
  sps[i]<-CM[2,2]/(CM[2,2]+CM[1,2])
  sns[i]<-CM[1,1]/(CM[1,1]+CM[2,1])
  
  
}

#plotting the Sensitivty and Specificity vs Cutoff
plot(Cutoff,sns,type="l",col="red", ylab = 'Sensitiivity/Specificity')
lines(Cutoff,sps,col="green")
legend("topright", c("Sensitivity", "Specificity"), fill=c("red","green"))

#To find intersection, create an array where sensitivity > specifity
above<-sns>sps
# Points always intersect when above=TRUE, then FALSE or reverse
pos<-which(diff(above)!=0)
# To find Threshold, Find the "Position"th value of Cutoff
Threshold<-Cutoff[pos]
Threshold

#Setting Threshold to 0.21
cut_off<-0.21
pred[pred<cut_off]<-0
pred[pred>=cut_off]<-1

#confusion Matrix 
C<-confusionMatrix(pred,dataTest_log$default.payment.next.month)
C
CM<-as.data.frame.matrix(C$table)





