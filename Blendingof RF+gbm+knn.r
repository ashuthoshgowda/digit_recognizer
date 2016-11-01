library(caret)
library(randomForest)
library(gbm)
library(kknn)
library(e1071)
library(rpart)

#train = read.csv("train.csv")
#test= read.csv("test.csv")


labels= as.factor(train[,1])
train = train[,-1]
newtrain = rbind(train,test)

test = newtrain[10001:28000,]
parttrain = newtrain[1:7000,]
valid = newtrain[7001:10000,]

set.seed(1234)
rf = randomForest(labels[1:7000]~., data=parttrain, nodesize=1, ntree=250, mtry=5)
rf_pred = predict(rf,newdata=valid)

confusionMatrix(rf_pred,labels[7001:10000])

knnpred = kknn(labels[1:7000]~., train=parttrain, test=valid, k=3)
kpred = fitted.values(knnpred)

confusionMatrix(fitted.values(knnpred),labels[7001:10000])

tunecontrol = trainControl(method = "repeatedcv",
                           number = 2,
                           repeats = 1
)


gbm_mod = train(labels[1:7000]~., data=parttrain, method= 'gbm', trControl=tunecontrol, n.trees = 100,interaction.depth=7 ,shrinkage = 0.209)

pred_gbm = predict(gbm_mod, newdata=valid)

confusionMatrix(pred_gbm,labels[7001:10000])


comb_pred = as.factor(ifelse(pred_gbm==kpred,kpred,rf_pred))
levels(comb_pred) = 0:9

confusionMatrix(comb_pred,labels[7001:10000])
