# Machine Learning Part

path <- "C:/Users/Jayce/Documents/NYCDataAcademy/Week4-R/Shiny Project/data"
setwd(path)

library(data.table)

train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))

train[,income_level := ifelse(income_level == "-50000",0,1)]
test[,income_level := ifelse(income_level == "-50000",0,1)]

factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

cat_train1 <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

num_train1 <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE]
rm(train,test)

#checking for missing value in the num data
# table(is.na(num_train))

#caret package offers a convenient way to filter out variables with high correlation. 
library(caret)

#set threshold as 0.8
ax <- findCorrelation(x = cor(num_train1), cutoff = 0.6)
ax

num_train1 <- num_train1[,-ax,with=FALSE]
num_test[,weeks_worked_in_year := NULL]

#We'll use base sapply() to find out percentage of missing values per column.

mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x))/length(x)})*100
mvtr
mvte

cat_train1 <- subset(cat_train, select=mvtr <5)
cat_test <- subset(cat_test, select=mvte <5)

cat_train1 <- cat_train1[,names(cat_train1) := lapply(.SD, as.character),.SDcols = names(cat_train1)]
for (i in seq_along(cat_train1)) set(cat_train1, i=which(is.na(cat_train1[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_train1 <- cat_train1[, names(cat_train1) := lapply(.SD,factor), .SDcols = names(cat_train1)]

cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]


for(i in names(cat_train1)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train1[[i]])) < p))
  levels(cat_train1[[i]])[levels(cat_train1[[i]]) %in% ld] <- "Other"
}

for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}

library(mlr)
summarizeColumns(cat_train1)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]

num_train1[,.N,age][order(age)]
num_train1[,.N,wage_per_hour][order(-N)]

num_train1[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train1[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

num_train1[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train1[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train1[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train1[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_train1[,income_level := NULL]
cat_train1$income_level <- as.factor(cat_train1$income_level)

d_train <- cbind(num_train1,cat_train1)
d_test <- cbind(num_test,cat_test)

d_train1 <- data.frame(d_train)
d_test <- data.frame(d_test)

View(d_train)

rm(num_train1,num_test,cat_train1,cat_test)

train.task <- makeClassifTask(data = d_train1, target = "income_level")
test.task <- makeClassifTask(data=d_test, target = "income_level")

train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#Feature importances
var_imp <- generateFilterValuesData(train.task, method = c("FSelector_information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#lets see which algorithms are available
listLearners("classif","twoclass")[c("class","package")]

getParamSet("classif.svm")
svm_learner <- makeLearner("classif.svm",predict.type = "response")
svm_learner$par.vals<- list(class.weights = c("0"=1,"1"=10),kernel="radial")

svm_param <- makeParamSet(
  makeIntegerParam("cost",lower = 10^-1,upper = 10^2), 
  makeIntegerParam("gamma",lower= 0.5,upper = 2)
)

set_search <- makeTuneControlRandom(maxit = 5L) #5 times
set_cv <- makeResampleDesc("CV",iters=5L,stratify = TRUE)

svm_tune <- tuneParams(learner = svm_learner,task = train.task,measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = svm_param,control = set_search,resampling = set_cv)

svm_new <- setHyperPars(learner = svm_learner, par.vals = svm_tune$x)

svm_model <- train(svm_new,train.task)

predict_svm <- predict(svm_model,test.task)

confusionMatrix(d_test$income_level,predict_svm$data$response)
