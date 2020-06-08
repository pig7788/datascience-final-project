library(caTools)
library(caret)
library(ggplot2)
library(pROC)
library(rpart)

round_df <- function(x) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns],2)
  (x)
}


# read argument
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw5_studentID.R --fold n --train Titanic_Data/train.csv --test Titanic_Data/test.csv --report performance.csv --predict predict.csv", call.=FALSE)
}
i<-1

# process argument
while( i < length(args) ){
  if(args[i] == "--fold"){
    n_fold <- args[i+1]
  }
  else if(args[i]=="--train"){
    train_file_path <- args[i+1]
  }
  else if(args[i]=="--test"){
    test_file_path <- args[i+1]
  }
  else if(args[i]=="--report"){
    perforamance_file <- args[i+1]
  }
  else if(args[i]=="--predict"){
    predict_file <- args[i+1]
  }
  i <- i+2
}

# read csv - train.csv
train <- read.csv(train_file_path)
train <-train[-1]

summary(train)

# fill na's of age (vessels,thalium_scan)
#train<-na.omit(train)
vessels.model <- rpart(vessels ~ age+sex+chest_pain+resting_bp+cholestoral+high_sugar+ecg+max_rate+exercise_angina+st_depression+slope+heart_disease
                       ,data=train[!is.na(train$vessels), ],method='class')
train$vessels[is.na(train$vessels)]<- predict(vessels.model, newdata=train[is.na(train$vessels),],type = "class")

thalium_scan.model <- rpart(thalium_scan ~ age+sex+chest_pain+resting_bp+cholestoral+high_sugar+ecg+max_rate+exercise_angina+st_depression+slope+vessels+heart_disease
                            , data=train[!is.na(train$thalium_scan), ], method='class')

train$thalium_scan[is.na(train$thalium_scan)] <- predict(thalium_scan.model, newdata=train[is.na(train$thalium_scan),],type="class")

#rename category
train$vessels <- train$vessels + 1
train$thalium_scan[train$thalium_scan == 3] <- 1
train$thalium_scan[train$thalium_scan == 6] <- 2
train$thalium_scan[train$thalium_scan == 7] <- 3

train$thalium_scan <- as.factor(train$thalium_scan)

#age_cut
#age_cut <- cut(train$age, c(28, 43, 55, 65, 77))
#train$age_interval_without_label <- as.numeric(age_cut)

# age_normalize
# train$age <- with(train, (age - min(age)) / (max(age) - min(age)))
resting_bp_cut <- cut(train$resting_bp, c(93, 113, 133, 160, 192))
train$resting_bp_without_label <- as.numeric(resting_bp_cut)

#min_max_normalization
#train$resting_bp <- with(train, (resting_bp - min(resting_bp)) / (max(resting_bp) - min(resting_bp)))
train$slope_label <- sapply(train$slope, function(x) {ifelse(x==1, 1,0)})


categorical_frames <- c("thalium_scan")
drop <- c()

for(i in 1:length(categorical_frames)){
  
  level <- unique(train[[categorical_frames[i]]])
  names <- lapply(categorical_frames[i],function(x) paste(x,level,sep='_'))
  print(names)
  drop <- c(drop,names[[1]][length(names)])
  m <- model.matrix(~factor(train[[categorical_frames[i]]])-1,train)
  for(i in 1:length(colnames(m))){
    colnames(m)[[i]] <- names[[1]][i]
  }
  train<-cbind(train,as.data.frame(m))
}

drop<-c(drop,"thalium_scan")
print(drop)
train <- train[ , !(names(train) %in% drop)]

write.table(train, file="g.csv", row.names = F, quote = F,sep=",")


n_fold <- as.integer(n_fold)
prob <- 1/as.integer(n_fold)

col = length(train) #col
d_size = nrow(train)


set.seed(4)
train <- train[sample(d_size),]
folds1 <- cut(seq(1,d_size),breaks=n_fold,labels=FALSE)

acc_train <- c()
acc_val <- c()
acc_test <-c()
aucscore<-c()
folds <-c()

i = 1
for(i in 1:n_fold){
  folds <- c(folds,paste("fold",as.character(i),sep=""))
  
  testIdx <- which(folds1==i,arr.ind=TRUE)
  valIdx <- which(folds1==(i%%n_fold+1),arr.ind=TRUE)
  tmp <- setdiff(seq(1,d_size),testIdx)
  trainIdx <- setdiff(tmp , valIdx)
  
  testData <- train[testIdx,]
  valData <- train[valIdx,]
  trainData <- train[trainIdx,]
  
  prop.table(table(trainData$y))
  model<-glm( heart_disease ~ resting_bp_without_label#resting_bp
              +slope_label+chest_pain
              +vessels+thalium_scan_1+thalium_scan_3#thalium_scan# thalium_scan_1+thalium_scan_3
              ,data=trainData,family=binomial())
  summary(model)
  model_name <- paste("model",i,".rds",sep="")
  saveRDS(model, model_name)
  
  pre<-predict(model,type='response')
  res <- ifelse(pre > 0.5,"1","0")
  res<-as.factor(res)
  res <- table(data = res, reference = trainData$heart_disease)
  
  
  pre1<-predict(model,newdata=valData,type='response')
  res1 <- ifelse(pre1 > 0.5,"1","0")
  res1 <-as.factor(res1)
  res1 <- table(data = res1, reference = valData$heart_disease)
  
  
  #modelroc<-roc(valData$y,pre)
  #aucscore<-c(aucscore,auc(modelroc))
  
  pre2<-predict(model,newdata=testData,type='response')
  res2 <- ifelse(pre2 > 0.5,"1","0")
  res2 <-as.factor(res2)
  res2 <- table(data = res2, reference = testData$heart_disease)
  
  
  modelroc1<-roc(testData$heart_disease,pre2)
  aucscore<-c(aucscore,auc(modelroc1))
  
  acc_train<-c(acc_train,sum(diag(res))/sum(res))
  acc_val<-c(acc_val,sum(diag(res1))/sum(res1))
  acc_test<-c(acc_test,sum(diag(res2))/sum(res2))
}
  
  print(aucscore)
  folds <- c(folds,"ave.")
  acc_train <- c(acc_train,mean(acc_train))
  acc_val <- c(acc_val,mean(acc_val))
  acc_test <- c(acc_test,mean(acc_test))
  
  #I choose the test model based on auc, but not accuracy(I have calculated but not used)
  best_moodel <- which.max(acc_test)
  model_name <- paste("model",best_moodel,".rds",sep="")
  model <- readRDS(model_name)
  
  out_data<-data.frame(set = folds,training= acc_train,validation = acc_val,test = acc_test,stringsAsFactors = F)
  out_data<-round_df(out_data)
  write.table(out_data, file=perforamance_file, row.names = F, quote = F,sep=",")
  
  test <- read.csv(test_file_path)
  
  id <- test[1]
  test <- test[-1]
  d_size = nrow(test)
  
  test$vessels <- test$vessels + 1
  test$thalium_scan[test$thalium_scan == 3] <- 1
  test$thalium_scan[test$thalium_scan == 6] <- 2
  test$thalium_scan[test$thalium_scan == 7] <- 3
  
  test$thalium_scan <- as.factor(test$thalium_scan)
  
  #age_cut <- cut(test$age, c(28, 43, 55, 65, 77))
  #test$age_interval_without_label <- as.numeric(age_cut)
  #test$age <- with(test, (age - min(age)) / (max(age) - min(age)))
  
  resting_bp_cut <- cut(test$resting_bp, c(93, 113, 133, 160, 192))
  test$resting_bp_without_label <- as.numeric(resting_bp_cut)
  
  test$slope_label <- sapply(test$slope, function(x) {ifelse(x==1, 1,0)})
  
  for(i in 1:length(categorical_frames)){
    
    level <- unique(test[[categorical_frames[i]]])
    names <- lapply(categorical_frames[i],function(x) paste(x,level,sep='_'))
    print(names)

    m <- model.matrix(~factor(test[[categorical_frames[i]]])-1,train)
    for(i in 1:length(colnames(m))){
      colnames(m)[[i]] <- names[[1]][i]
    }
    test<-cbind(test,as.data.frame(m))
  }
  
  print(drop)
  test <- test[ , !(names(test) %in% drop)]
  
  pred_val <- predict(model,test,type = "response")
  pred_val <- ifelse(pred_val > 0.5,"1","0")
  res <- data.frame(Id=id,heart_disease=pred_val)
  write.table(res, file=predict_file, row.names = F,col.names = c("Id","heart_disease"), quote = F,sep=",")
  
  