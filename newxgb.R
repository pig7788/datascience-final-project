library(xgboost)
library(argparser, quietly=TRUE)
library(Matrix)
library(caret)

p <- arg_parser("Process input and output csv")
p <- add_argument(p, "--train",help="iutput result csv file")
p <- add_argument(p, "--test", help="output result csv file", default="test.csv")
p <- add_argument(p, "--predict", help="output result csv file", default="test.csv")
args <- parse_args(p, commandArgs(trailingOnly = TRUE))
train <- read.csv(args$train)
test <- read.csv(args$test, header=T)
# fillna with mean value
for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}
train$slope_label <- sapply(train$slope, function(x) {ifelse(x==1, 1,0)})
test$slope_label <- sapply(test$slope, function(x) {ifelse(x==1, 1,0)})

# one hot 
train$thalium_scan_a<- ifelse(train$thalium_scan==3, 1, 0)
train$thalium_scan_b<- ifelse(train$thalium_scan==6, 1, 0)
train$thalium_scan_c<- ifelse(train$thalium_scan==7, 1, 0)
train$thalium_scan<-NULL
# train$thalium_scan_b<-NULL
head(train)
test$thalium_scan_a<- ifelse(test$thalium_scan==3, 1, 0)
test$thalium_scan_b<- ifelse(test$thalium_scan==6, 1, 0)
test$thalium_scan_c<- ifelse(test$thalium_scan==7, 1, 0)
test$thalium_scan<-NULL

head(test)



 
#train/test split
dtrain<-train[1:200,]
dtrain<-train[1:200,]
dtest<-train[201:228,]
# print(train[1:11,])
dtrain$heart_disease <- NULL


dtest$heart_disease<-NULL
dtrain<-as.matrix(dtrain)
dtest<-as.matrix(dtest)
id<-test$id
test<-as.matrix(test)
dtrain <- xgb.DMatrix(dtrain, label =train[1:200,]$heart_disease)
bst = xgboost(data=dtrain, max_depth=5, eta=1,nfold=5, objective='binary:logistic', nrounds=5)

# feature importance
imp = xgb.importance(names(train[,-15]),model=bst)
print(imp)

# 計算預測值 (get prediction)
dev_pred = predict(bst,dtest)
dev_pred <- ifelse(dev_pred >0.5, 1, 0)
# print(dev_pred)
# print(train[201:228,15])
test_y<-train[201:228,]$heart_disease
tab=table(dev_pred,test_y)
#confusion matrix
print('----------------------------------------------------------------')
print(tab)
heart_disease = predict(bst,test)


id<-test[,1]
# train with feature selection
train<-train[,-1]
train<-train[,-6]
# train<-train[,-9]
train<-train[,-15]
test<-test[,-1]
test<-test[,-6]
test<-test[,-14]
dtrain<-train[1:200,]

dtest<-train[201:228,]

dtrain$heart_disease <- NULL

dtest$heart_disease<-NULL
dtrain<-as.matrix(dtrain)
dtest<-as.matrix(dtest)

test<-as.matrix(test)
dtrain <- xgb.DMatrix(dtrain, label =train[1:200,]$heart_disease)
params <- list(booster = "gbtree", objective = "binary:logistic", 
eta=0.3, gamma=0, max_depth=4, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, 
print.every.n = 10, early.stop.round = 20, maximize = F)
bst = xgboost(data=dtrain, max_depth=5, eta=0.3,nfold=5, objective='binary:logistic', nrounds=20)


heart_disease = predict(bst,test)

df = data.frame(id,heart_disease)
# df
df$heart_disease <- ifelse(df$heart_disease>0.5, 1, 0)
# df
write.csv(df, args$predict, row.names=FALSE)










































































# Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# # colnames(Ypred) = levels(iris2$Species)
# Ypred = levels(train$heart_disease)[max.col(Ypred)]
# Ypred = factor(Ypred,levels=levels(train$heart_disease))
 
# # 混淆矩陣 (confusion matrix)
# t0 = table(iris2$Species,Ypred)
# t0
 
# 預測正確率 (accuracy)
# sum(diag(t0))/sum(t0)
 
# 解釋變數重要性 (variable importance)
# imp = xgb.importance(names(iris2[,-5]),model=result)
# print(imp)
 
# library(Ckmeans.1d.dp)
# xgb.plot.importance(imp)
# library(DiagrammeR)
# xgb.plot.tree(feature_names=names(iris[,-5]),model=result, n_first_tree=2)

