bank <- read.csv("bank.csv")
str(bank)
head(bank)
dim(bank)

levels(bank[,17])[1] <- "0"
levels(bank[,17])[2] <- "1"

set.seed(12870847)
subset <- sample(nrow(bank), nrow(bank) * 0.75)
bank.train = bank[subset, ]
bank.test = bank[-subset, ]

bank.glm<- glm(deposit~., family=binomial, data=bank.train)
summary(bank.glm)
?regsubsets
library(leaps)
subset_result <- regsubsets(deposit~.,data=bank.train, nbest=2, nvmax = 16)
summary(subset_result)
plot(subset_result, scale="bic")

nullmodel=glm(deposit~1,family = binomial, data=bank.train)
fullmodel=glm(deposit~., family = binomial, data=bank.train)
model_step_b <- step(fullmodel, direction='backward')

deposit ~ job + marital + education + balance + housing + loan + 
  contact + day + month + duration + campaign + previous +      6821.93
  poutcome
  
model_step_b <- step(nullmodel,scope=list(lower=nullmodel, upper=fullmodel),direction='forward')
model_step_c <- step(nullmodel,scope=list(lower=nullmodel, upper=fullmodel),direction='both')

final.model.glm <- glm(deposit~job + marital + education + balance + housing + loan + contact + day + month + duration + campaign + previous + poutcome, family = binomial, data= bank.train)

#In sample
pred.glm0.train<- predict(final.model.glm, type="response")
library(ROCR)
pred <- prediction(pred.glm0.train, bank.train$deposit)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

#Out sample
pred.glm0.test<- predict(final.model.glm, type="response", newdata = bank.test)
library(ROCR)
pred <- prediction(pred.glm0.test, bank.test$deposit)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

pcut <- 0.5

class.glm0.train<- (pred.glm0.train>pcut)*1
# get confusion matrix
table(bank.train$deposit, class.glm0.train, dnn = c("True", "Predicted"))
MR<- mean(bank.train$deposit!=class.glm0.train)  #16.56%
# False positive rate 13% &80%
FPR<- sum(bank.train$deposit==0 & class.glm0.train==1)/sum(bank.train$deposit==0)
TPR <- sum(bank.train$deposit==1 & class.glm0.train==1)/sum(bank.train$deposit==1)


costfunc = function(obs, pred.p, pcut){
  weight1 = 5   # define the weight for "true=1 but pred=0" (FN)
  weight0 = 1    # define the weight for "true=0 but pred=1" (FP)
  c1 = (obs==1)&(pred.p<pcut)    # count for "true=1 but pred=0"   (FN)
  c0 = (obs==0)&(pred.p>=pcut)   # count for "true=0 but pred=1"   (FP)
  cost = mean(weight1*c1 + weight0*c0)  # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
} 

p.seq = seq(0.01, 1, 0.01) 

cost = rep(0, length(p.seq))  
for(i in 1:length(p.seq)){ 
  cost[i] = costfunc(obs = bank.train$deposit, pred.p = pred.glm0.train, pcut = p.seq[i])  
}
plot(p.seq, cost)
optimal.pcut.glm0 = p.seq[which(cost==min(cost))]

class.glm0.train.opt<- (pred.glm0.train>optimal.pcut.glm0)*1
table(bank.train$deposit, class.glm0.train.opt, dnn = c("True", "Predicted"))
MR<- mean(bank.train$deposit!= class.glm0.train.opt)
FPR<- sum(bank.train$deposit==0 & class.glm0.train.opt==1)/sum(bank.train$deposit==0)
FNR<- sum(bank.train$deposit==1 & class.glm0.train.opt==0)/sum(bank.train$deposit==1)
FPR<- sum(bank.train$deposit==0 & class.glm0.train.opt==1)/sum(bank.train$deposit==0)
TPR <- sum(bank.train$deposit==1 & class.glm0.train.opt==1)/sum(bank.train$deposit==1)

cost<- costfunc(obs = bank.train$deposit, pred.p = pred.glm0.train, pcut = optimal.pcut.glm0)

##########################################################################################
library(randomForest)
bank.rf <- randomForest(as.factor(deposit)~., mtry= 500, data = bank.train)
bank.rf
plot(bank.rf, lwd=rep(2, 3))
legend("right", legend = c("OOB Error", "FPR", "FNR"), lwd=rep(2, 3), lty = c(1,2,3), col = c("black", "red", "green"))



p.seq = seq(0.01, 0.5, 0.01)
cost = rep(0, length(p.seq))  
for(i in 1:length(p.seq)){ 
  cost[i] = costfunc(obs = bank.train$deposit, pred.p = bank.rf.pred, pcut = p.seq[i])  
}
plot(p.seq, cost)

library(ROCR)
#In sample
bank.rf.pred.train<- predict(bank.rf, type = "prob")[,2]
pred <- prediction(bank.rf.pred.train, bank.train$deposit)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))
optimal.pcut= p.seq[which(cost==min(cost))]
bank.rf.class.train<- (bank.rf.pred.train>optimal.pcut)*1
table(bank.train$deposit, bank.rf.class.train, dnn = c("True", "Pred"))
MR<- mean(bank.train$deposit!=bank.rf.class.train)

#Out sample
bank.rf.pred.test<- predict(bank.rf, newdata= bank.test, type = "prob")[,2]
pred <- prediction(bank.rf.pred.test, bank.test$deposit)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))
bank.rf.class.test<- (bank.rf.pred.test>optimal.pcut)*1
table(bank.test$deposit, bank.rf.class.test, dnn = c("True", "Pred"))
MR<- mean(bank.train$deposit!=bank.rf.class.test)
