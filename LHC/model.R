# Kaggle Challenge
# 3rd June, 2014
rm(list = setdiff(ls(), "jdata"))
setwd("D:/247NETWORKBACKUP/BackupOn21May/Kaggle")

AMS <- function(s,b,breg=10){
  val <- sqrt(2*((s+b+breg)*log(1+s/(b+breg))-s))
  return(val)
}
# using a cutoff of cut, calculate sensitivity, specificity, and classification rate
# For other classifiers
perfAll = function(cut, yraw, y){
  yhat = (yraw>cut)
  w = which(y==1)
  precision   = mean( y[which(yhat==1)]==1)
  sensitivity = mean( yhat[w] == 1 ) # recall
  specificity = mean( yhat[-w] == 0 ) 
  c.rate = mean( y==yhat ) 
  d = cbind(sensitivity,specificity)-c(1,1)
  d = sqrt( d[1]^2 + d[2]^2 ) 
  f1 = 2*precision*sensitivity/(precision+sensitivity)
  out = t(as.matrix(c(precision, sensitivity, f1)))
  colnames(out) = c("precision", "recall", "f1-score")
  return(out)
}

# Apply only on Logistic Regression model
perf = function(cut, mod, y){
  yhat = (mod$fit>cut)
  w = which(y==1)
  precision   = mean( y[which(yhat==1)]==1)
  sensitivity = mean( yhat[w] == 1 ) # recall
  specificity = mean( yhat[-w] == 0 ) 
  c.rate = mean( y==yhat ) 
  d = cbind(sensitivity,specificity)-c(1,1)
  d = sqrt( d[1]^2 + d[2]^2 ) 
  out = t(as.matrix(c(sensitivity, specificity, c.rate, d, precision)))
  colnames(out) = c("sensitivity", "specificity", "c.rate", "distance", "precision")
  return(out)
}

plotperf <- function(model, y){
  s = seq(.01,.99,length=1000)
  OUT = matrix(0,1000,5)
  for(i in 1:1000) OUT[i,]=perf(s[i], model, y)
  plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT[,2],col="darkgreen",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  lines(s,OUT[,4],col="darkred",lwd=2)
  lines(s,OUT[,5],col="black",lwd=2)
  box()
  legend(0,.25,col=c(2,"darkgreen",4,"darkred","black"), lwd=c(2,2,2,2,2), 
         c("Recall","Specificity","Classification Rate","Distance","precision"))
  return(OUT)
}

plotperfAll <- function(yraw, y){
  s = seq(.01,.99,length=1000)
  OUT = matrix(0,1000,3)
  for(i in 1:1000) OUT[i,]=perfAll(s[i], yraw, y)
  plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  # lines(s,OUT[,2],col="darkgreen",lwd=2)
  # lines(s,OUT[,3],col=4,lwd=2)
  # lines(s,OUT[,4],col="darkred",lwd=2)
  lines(s,OUT[,2],col="black",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  box()
  
  legend(0,.25,col=c(2,"black",4), lwd=c(2,2,2), 
         c("Precision", "Recall", "F1-score"))
  d <- apply(OUT, 1, function(x) abs(x[1]-x[2])+abs(x[1]-x[3]+abs(x[2]-x[3])))
  print(sprintf("F1-score is maximum at %g", s[which.max(OUT[,3])]))
  print(sprintf("Three curves meet at %g", s[which.min(d)]))
  
  return(cbind(s,OUT))
}

# Read the training and test data files
jdata <- read.csv(file="training.csv", header=TRUE)
tdata <- read.csv(file="test.csv", header=TRUE)

# Remove the missing values
jdata <-  do.call(data.frame,lapply(jdata,function(x) ifelse(as.character(x) == "-999" | as.character(x) == "",NA,as.character(x))))
primary <- grep("PRI", names(jdata), value=TRUE)
primary.retained <- primary[apply(subset(jdata, select=primary), 2, function(x) sum(is.na(x))==0)]
derivative <- grep("DER", names(jdata), value=TRUE)
derivative.retained <- derivative[apply(subset(jdata, select=derivative), 2, function(x) sum(is.na(x))==0)]

(selected.vars <- c(primary.retained, derivative.retained))

jdata2 <- subset(jdata, select = selected.vars)
jdata2 <- do.call(data.frame, lapply(jdata2, function(x) as.numeric(as.character(x))))

jdata3 <- subset(jdata, select=c(-EventId,-Weight,-Label))
jdata3 <- do.call(data.frame, lapply(jdata3, function(x) as.numeric(as.character(x))))

resp <- ifelse(jdata$Label=="b",0,1)

### SVM Model Data
svmdata <- scale(jdata2, center = TRUE, scale = TRUE)
svmscale <- attr(svmdata,"scaled:center")
svmcenter <- attr(svmdata,"scaled:scale")
svmtest <- scale(subset(tdata, select=selected.vars), center = svmcenter, 
                 scale = svmscale) 
write.csv(x=cbind(svmdata,resp), file="traindata_SVM.csv", row.names=FALSE)
write.csv(x=svmtest, file="testdata_SVM.csv", row.names=FALSE)

# Logistic Regression Model
M1 <- glm(resp~., family=binomial(), data = cbind(jdata2, resp))

# ypred <- predict(M1, jdata3, type="response")
# pred <- prediction( ypred, resp)
# perf0 <- performance(pred,"tpr","fpr")
# perf1 <- performance(pred, "prec", "rec")
# perf2 <- performance(pred, "sens", "spec")
# plot(perf2)
out.stat <- plotperf(M1, resp)

#### --------------------------------------------------------------------------
xnew <- subset(tdata, select=selected.vars)

# xdata <- do.call(data.frame,lapply(xnew,function(x) ifelse(as.character(x) == "-999" | as.character(x) == "",NA,as.character(x))))
# xdata2 <- na.omit(xdata)
# xdata3 <- do.call(data.frame, lapply(xdata2, function(x) as.numeric(as.character(x))))
ynew <- predict(M1, xnew, type="response")
ynew.rank <- rank(ynew)
ynew.class <- ifelse(ynew > 0.36, "s", "b")
y.out <- data.frame(cbind(tdata$EventId, ynew.rank, ynew.class))
names(y.out) <- c("EventId", "RankOrder", "Class")
write.csv(y.out, file="submission.csv", row.names=FALSE)

# Recursive Partitioning based Decision Tree
# Score = 1.80451 
library(rpart)
M2 <- rpart(resp~., data = cbind(jdata2, resp), method="class")
yprob <- predict(M2, jdata2, type="prob") # yprob has two columns, for 0 and 1
yclass <- predict(M2, jdata2, type="class")
yclass2 <- ifelse(yprob[,2]>0.3,1,0)

out.stat <- plotperfAll(yprob[,2], resp)
(summary(as.factor(yprob[,2]))) # there are 6 discrete values

ynew.tree <- predict(M2, xnew, type="prob")[,2]
ynew.rank.tree <- rank(ynew.tree, , ties.method="first") 
ynew.class.tree <- ifelse(ynew.tree > 0.2, "s", "b")

y.tree.out <- data.frame(cbind(tdata$EventId, ynew.rank.tree, ynew.class.tree))
names(y.tree.out) <- c("EventId", "RankOrder", "Class")
write.csv(y.tree.out, file="submission_rpart.csv", row.names=FALSE)

# AdaBoost Model: 
# Score = 2.24998
library(ada)
M3 <- ada(resp~., data = cbind(jdata2, resp), type = "discrete", iter = 50, 
          nu = 1.0)
control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
M3 <- ada(resp~., data=cbind(jdata3, resp), type="discrete", iter=100, 
          bag.frac=0.5, nu = 1.0)

print(levels(M3$fit)) # to know which class label is given first
yprob.ada <- predict(M3, jdata2, type="prob")[,2]
out.stat <- plotperfAll(yprob.ada, resp)

yclass.ada <- predict(M3, jdata2, type="vector")
(table(resp, yclass.ada))
yclass2.ada <- ifelse(yprob.ada>0.29,1,0)
(table(resp, yclass2.ada))

ynew.ada <- predict(M3, xnew, type="prob")[,2]
ynew.rank.ada <- rank(ynew.ada, ties.method="first") 
# ynew.rank.ada <- as.numeric(factor(ynew.ada))
# threshold = 0.30, Rank = 2.24998
# threshold = 0.25, Rank = 2.13524
ynew.class.ada <- ifelse(ynew.ada > 0.25, "s", "b")

y.ada.out <- data.frame(cbind(tdata$EventId, ynew.rank.ada, ynew.class.ada))
names(y.ada.out) <- c("EventId", "RankOrder", "Class")
write.csv(y.ada.out, file="submission_ada.csv", row.names=FALSE)

# AdaBoost with training, validation and test set
n <- nrow(jdata)
set.seed(100)
ind <- sample(1:n)
trainval <- ceiling(n * .7)
 testval <- ceiling(n * .2)
  jdata4 <- cbind(jdata3, resp)
   train <- jdata4[ind[1:trainval],]
    test <- jdata4[ind[(trainval + 1):(trainval + testval)],]
   valid <- jdata4[ind[(trainval + testval + 1):n],]
 control <- rpart.control(cp = -1, maxdepth = 14, maxcompete = 1, xval = 0)
   gen1 <- ada(resp~., data = train, test.x = test[,-31], test.y = test[,31], 
               type = "gentle", control = control, iter = 100)
   # gen1 <- addtest(gen1, valid[,-31], valid[,31])
 wvalid <- jdata$Weight[ind[(trainval + testval + 1):n]]
pred.valid <- predict(gen1, valid[-31], type="prob")[,2]
 wvalid <- as.numeric(as.character(wvalid))

thr = seq(.01,.99,length=1000)
amsval = vector(mode = "numeric",length=1000)
for(i in 1:1000) {
  signal <- (pred.valid > thr[i])
  s <- sum(wvalid[signal])
  b <- sum(wvalid[!signal])
  amsval[i]=AMS(s, b)
}
plot(thr,amsval,type="l")

    #
    # yclass2.ada
    # resp      0      1
    # 0 149502  14831
    # 1  14803  70864

   # Need to understand the effect of the loss function, type, bag fraction
   # The direct approach shown below performing much worse
#   gen1 <- ada(resp~., data=jdata4, type="gentle", iter=200, bag.frac=0.5)


print(levels(gen1$fit)) # to know which class label is given first
yprob.ada <- predict(gen1, jdata3, type="prob")[,2]
out.stat <- plotperfAll(yprob.ada, resp)

# yclass.ada <- predict(gen1, jdata3, type="vector")
# (table(resp, yclass.ada))

yclass2.ada <- ifelse(yprob.ada>0.437708,1,0)
(table(resp, yclass2.ada))

# Process the test data
tdata2 <- subset(tdata, select=-EventId)
tdata2 <-  do.call(data.frame,lapply(tdata2,function(x) ifelse(as.character(x) == "-999" | as.character(x) == "",NA,as.character(x))))
tdata3 <- do.call(data.frame, lapply(tdata2, function(x) as.numeric(as.character(x))))

ynew.ada <- predict(gen1, tdata3, type="prob")[,2]
ynew.rank.ada <- rank(ynew.ada, ties.method="first") 
# ynew.rank.ada <- as.numeric(factor(ynew.ada))
# threshold = 0.30, Rank = 2.44388
# threshold = 0.386697 (max. F1-score), Rank = 2.59943
# threshold = 0.43, (precision-recall-F1-score meet) Rank = 2.67489
# threshold = 0.428879, (precision-recall-F1-score meet) Rank = 2.67631
# threshold = 0.437708, (precision-recall-F1-score meet) Rank = 2.71700

ynew.class.ada <- ifelse(ynew.ada > 0.437708, "s", "b")
y.ada.out <- data.frame(cbind(tdata$EventId, ynew.rank.ada, ynew.class.ada))
names(y.ada.out) <- c("EventId", "RankOrder", "Class")
write.csv(y.ada.out, file="submission_ada.csv", row.names=FALSE)




# SVM Model
library(e1071)
M4 <- svm(resp~., data = cbind(jdata2, resp), kernel="radial", gamma=0.1, cost=1.0)

# Gradient Boosting Model
# Rank: 1.63657
library(gbm)
M5 <- gbm(resp~., data = cbind(jdata2, resp), distribution="bernoulli")
y5 <- predict(M5, jdata2, n.trees = 100, type="response")
summary(y5)
y5.class <- ifelse(y5>0.3255,1,0)
table(resp,y5.class)

ynew.gb <- predict(M5, xnew, n.trees = 100, type="response")
ynew.rank.gb <- rank(ynew.gb, ties.method="first") 
ynew.class.gb <- ifelse(ynew.gb > 0.3255, "s", "b")

y.gb.out <- data.frame(cbind(tdata$EventId, ynew.rank.gb, ynew.class.gb))
names(y.gb.out) <- c("EventId", "RankOrder", "Class")
write.csv(y.gb.out, file="submission_gb.csv", row.names=FALSE)


# LogitBoost Model
# threshold = 0.125: Rank = 1.75184
library(caTools)
M6 <- LogitBoost(jdata2, resp, nIter = 10)
table(predict(M6, jdata2), resp)
print(M6$lablist) # to know which class label is given first
yprob.lb <- predict(M6, jdata2, type="raw")[,2]
out.stat <- plotperfAll(yprob.lb, resp)
yclass.lb <- ifelse(yprob.lb>0.125,1,0)
(table(resp, yclass.lb))

ynew.lb <- predict(M6, xnew, type="raw")[,2]
ynew.rank.lb <- rank(ynew.lb, ties.method="first") 
# ynew.rank.ada <- as.numeric(factor(ynew.ada))
ynew.class.lb <- ifelse(ynew.lb > 0.125, "s", "b")

y.lb.out <- data.frame(cbind(tdata$EventId, ynew.rank.lb, ynew.class.lb))
names(y.lb.out) <- c("EventId", "RankOrder", "Class")
write.csv(y.lb.out, file="submission_lb.csv", row.names=FALSE)

rows.retained <- apply(jdata, 1, function(x) sum(is.na(x))==0)

