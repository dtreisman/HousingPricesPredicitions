#####
train <- read.csv("C:/Users/Dani Treisman/Documents/STAT 388 Predictive/project/train.csv", header=T)
test <- read.csv("C:/Users/Dani Treisman/Documents/STAT 388 Predictive/project/test.csv", header=T)

# removed "Utilities" because they are all the same value
train <- subset(train, select = -c(Utilities))
train$OverallQual <- as.factor(train$OverallQual)
train$OverallCond <-as.factor(train$OverallCond)
train$YearBuilt <- as.factor(train$YearBuilt)
train$YearRemodAdd <- as.factor(train$YearRemodAdd)
train$MoSold <- as.factor(train$MoSold)
train$YrSold <- as.factor(train$YrSold)

test <- subset(test, select = -c(Utilities))
test$OverallQual <- as.factor(test$OverallQual)
test$OverallCond <-as.factor(test$OverallCond)
test$YearBuilt <- as.factor(test$YearBuilt)
test$YearRemodAdd <- as.factor(test$YearRemodAdd)
test$MoSold <- as.factor(test$MoSold)
test$YrSold <- as.factor(test$YrSold)


library(tree)
library(rpart)
library(rpart.plot)

tree.salesprice=rpart(SalePrice~., data=train)

printcp(tree.salesprice) 

plotcp(tree.salesprice)

rpart.plot(tree.salesprice)

plot(tree.salesprice, uniform=TRUE, main="Regression of Sales Price")
text(tree.salesprice, use.n=TRUE, all=TRUE, cex=0.8)

library(mice)
md.pattern(train)
md.pattern(test)
# potentially delete: Alley, FireplaceQu, Fence, MiscFeature, PoolQC

#mice#####

fix <- mice(train, m=5, maxit=1, method='cart', seed=500)
missing <- md.pattern(fix$data)
imp1 <- complete(fix, 1)
imp2 <- complete(fix, 2)
imp3 <- complete(fix, 3)
imp4 <- complete(fix, 4)
imp5 <- complete(fix, 5)
train <- imp

fix_test <- mice(test, m=5, maxit=1, method='cart', seed=500)
missing <- md.pattern(fix$data)
imp_test <- complete(fix_test)
test <- imp_test

library(gbm)
# fit <- gbm(SalePrice~., data=train, distribution = "gaussian", 
#            n.trees=1000, shrinkage=.01, cv.folds = 4,interaction.depth = 4)
# dev.off()
# plot(fit$cv.error)

#1#####
set.seed(1234)
train$fold <- sample(rep(1:4, length=nrow(train)), nrow(train),replace=FALSE)

lambda <- seq(.01, .3, .03)
MSE = c()
for (j in 1:length(lambda)){
  for (i in 1:4){print(i)
    fit1 <- gbm(SalePrice~., data=train[train$fold!=i,], 
                n.trees = 1500, shrinkage = lambda[j] ,distribution = "gaussian", interaction.depth = 4)
    train$yhat[train$fold==i] <- predict(fit1, newdata=train[train$fold==i,], n.trees = 1500)
  }
  MSE[j] <- mean((train$yhat-train$SalePrice)^2)
}
plot(lambda, MSE, type="l")

train <- train[,-c(81:82)]
fit_final <- gbm(SalePrice~., data=train, distribution = "gaussian", 
                 n.trees=1500, shrinkage=.04, cv.folds = 4,interaction.depth = 4)
plot(fit_final$cv.error)
min(fit_final$cv.error)
y1 <- predict(fit_final, test)

summary.gbm(fit_final, cBars = length(fit_final$var.names),
        n.trees = fit_final$n.trees, plotit = TRUE, order = TRUE,
        method = relative.influence)

#2#####
# train_2 <- subset(train, select= -c(Street, PoolArea, Heating))
train_2 <- subset(train, select= -c(Street, PoolArea, Heating, MiscVal, PavedDrive, 
                                    X3SsnPorch, Condition2, ExterCond, HeatingQC, 
                                    Electrical, LowQualFinSF, GarageQual, GarageCond))
set.seed(1234)
train_2$fold <- sample(rep(1:4, length=nrow(train_2)), nrow(train_2),replace=FALSE)

lambda <- seq(.01, .3, .03)
MSE = c()
for (j in 1:length(lambda)){
  for (i in 1:4){print(i)
    fit2 <- gbm(SalePrice~., data=train_2[train_2$fold!=i,], 
                n.trees = 1500, shrinkage = lambda[j] ,distribution = "gaussian", interaction.depth = 4)
    train_2$yhat[train_2$fold==i] <- predict(fit2, newdata=train_2[train_2$fold==i,], n.trees = 1500)
  }
  MSE[j] <- mean((train_2$yhat - train_2$SalePrice)^2)
}
plot(lambda, MSE, type="l")

train_2 <- train_2[,-c(68:69)]
fit_final2 <- gbm(SalePrice~., data=train_2, distribution = "gaussian", 
                 n.trees=1500, shrinkage=.04, cv.folds = 4,interaction.depth = 4)
plot(fit_final2$cv.error)

y2 <- predict(fit_final2, test)

summary.gbm(fit_final2, cBars = length(fit_final2$var.names),
            n.trees = fit_final2$n.trees, plotit = TRUE, order = TRUE,
            method = relative.influence)

#3#####
train_3 <- subset(train_2, select= -c(Alley, FireplaceQu, Fence))

set.seed(1234)
train_3$fold <- sample(rep(1:4, length=nrow(train_3)), nrow(train_3),replace=FALSE)

lambda <- seq(.01, .3, .03)
MSE = c()
for (j in 1:length(lambda)){
  for (i in 1:4){print(i)
    fit3 <- gbm(SalePrice~., data=train_3[train_3$fold!=i,], 
                n.trees = 1500, shrinkage = lambda[j] ,distribution = "gaussian", interaction.depth = 4)
    train_3$yhat[train_3$fold==i] <- predict(fit3, newdata=train_3[train_3$fold==i,], n.trees = 1500)
  }
  MSE[j] <- mean((train_3$yhat-train_3$SalePrice)^2)
}
plot(lambda, MSE, type="l")

train_3 <- train_3[,-c(72:73)]
fit_final3 <- gbm(SalePrice~., data=train_3, distribution = "gaussian", 
                  n.trees=1500, shrinkage=.04, cv.folds = 4,interaction.depth = 4)
plot(fit_final3$cv.error)

y3 <- predict(fit_final3, test)

summary.gbm(fit_final3, cBars = length(fit_final3$var.names),
            n.trees = fit_final3$n.trees, plotit = TRUE, order = TRUE,
            method = relative.influence)




#####
#
fit1 <- gbm(SalePrice~., data=imp1, distribution = "gaussian", 
                 n.trees=1500, shrinkage=.04, cv.folds = 4,interaction.depth = 4)
plot(fit1$cv.error)

Y1 <- predict(fit1, test)

summary.gbm(fit1, cBars = length(fit1$var.names),
            n.trees = fit1$n.trees, plotit = TRUE, order = TRUE,
            method = relative.influence)

#
fit2 <- gbm(SalePrice~., data=imp2, distribution = "gaussian", 
            n.trees=1500, shrinkage=.04, cv.folds = 4,interaction.depth = 4)
plot(fit2$cv.error)

Y2 <- predict(fit2, test)

summary.gbm(fit2, cBars = length(fit2$var.names),
            n.trees = fit2$n.trees, plotit = TRUE, order = TRUE,
            method = relative.influence)

#
fit3 <- gbm(SalePrice~., data=imp3, distribution = "gaussian", 
            n.trees=1500, shrinkage=.04, cv.folds = 4,interaction.depth = 4)
plot(fit3$cv.error)

Y3 <- predict(fit3, test)

summary.gbm(fit3, cBars = length(fit3$var.names),
            n.trees = fit3$n.trees, plotit = TRUE, order = TRUE,
            method = relative.influence)

#
fit4 <- gbm(SalePrice~., data=imp4, distribution = "gaussian", 
            n.trees=1500, shrinkage=.04, cv.folds = 4,interaction.depth = 4)
plot(fit4$cv.error)

Y4 <- predict(fit4, test)

summary.gbm(fit4, cBars = length(fit4$var.names),
            n.trees = fit4$n.trees, plotit = TRUE, order = TRUE,
            method = relative.influence)

#
fit5 <- gbm(SalePrice~., data=imp5, distribution = "gaussian", 
            n.trees=1500, shrinkage=.04, cv.folds = 4,interaction.depth = 4)
plot(fit5$cv.error)

Y5 <- predict(fit5, test)

summary.gbm(fit5, cBars = length(fit5$var.names),
            n.trees = fit5$n.trees, plotit = TRUE, order = TRUE,
            method = relative.influence) 


all_y <- as.data.frame(cbind(Y1, Y2, Y3, Y4, Y5))
for (i in 1:nrow(all_y)){
  all_y[i]$Y <- (sum(all_y[i,])/5)
}
all_y$means <- rowMeans(all_y)
#####
#predcitions
sub1 <- cbind(test$Id, y1)
colnames(sub1) <- c("Id","SalePrice")
sub2 <- cbind(test$Id, y2)
colnames(sub2) <- c("Id","SalePrice")
sub3 <- cbind(test$Id, all_y$means)
colnames(sub3) <- c("Id","SalePrice")



#write to file
write.csv(sub1, file = "C:/Users/Dani Treisman/Documents/STAT 388 Predictive/project/sub1.csv", row.names = F)
write.csv(sub2, file = "C:/Users/Dani Treisman/Documents/STAT 388 Predictive/project/sub2.csv", row.names = F)
write.csv(sub3, file = "C:/Users/Dani Treisman/Documents/STAT 388 Predictive/project/sub3.csv", row.names = F)


