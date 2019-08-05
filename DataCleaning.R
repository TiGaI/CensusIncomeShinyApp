path <- "C:/Users/Jayce/Documents/NYCDataAcademy/Week4-R/Shiny Project/data"
setwd(path)

library(data.table)
library(dplyr)
library(ggplot2)
library(wesanderson)

train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))

# dim(train); str (train); View(train)
# dim(test); str (test); View(test)

train[1:5]
test [1:5]

factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#subset categorical variables
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

#subset numerical variables
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE]
rm(train,test) #to save memory

unique(cat_train$income_level)
cat_train[,income_level := ifelse(income_level == "-50000", 0, 1)]

unique(cat_test$income_level)
cat_test[, 'income_level'] = ifelse(cat_test$income_level == "-50000", 0, 1)

# View(cat_train)
# View(num_train)

num_train = cbind(num_train, income_level = cat_train$income_level)
columnName = subset(num_train, select = -income_level)

round(prop.table(table(num_train$income_level))*100)

scatt1 <- ggplot(data=num_train,aes(x = age, y=wage_per_hour))+
  geom_point(aes(colour=income_level))+
  scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

scatt1

#barplot
newData <- cat_train %>% 
  group_by(race, income_level) %>%
  tally()

newData


ggplot(newData, aes(x=class_of_worker, y=n, fill=factor(income_level)))+
geom_bar(stat="identity",position="dodge")+
scale_fill_manual(name="Income Level",
                      labels=c("Less than 50000", "Greater than 50000"), values=wes_palette(n=2, name="Darjeeling1"))+
xlab("class_of_worker")+ylab("Count")+ theme(axis.text.x =element_text(angle  = 30,hjust = 1,size=10))

#scatterplot

scatterTest <- ggplot(data=num_train,aes(x=age, y=wage_per_hour, color=as.factor(income_level)))+
  geom_point()+
  scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

scatterTest + scale_color_manual(name = "Income Level", labels = c("Less than $50000", "Greater Than $50000"), values=wes_palette(n=2, name="GrandBudapest1"))

help(scale_color_manual)

# Proportionate table
barplot(prop.table(table(cat_train$marital_status,cat_train$income_level),1),col="dodgerblue")

barplot(prop.table(table(cat_train$class_of_worker,cat_train$income_level),1),col="dodgerblue")

myplot <- ggplot(cat_train, aes(class_of_worker, group = income_level)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative Percent") +
  # scale_x_discrete(labels = c('Federal Gov','Local Gov','Never Worked', "Not in Universe", "Private", "Self-Employed-Incorp", "Self-Employed-NotIncorp", "State Gov", "Without Pay")) +
  facet_grid(~income_level) + 
  labs(title="Percent of class_of_worker category per income level", size = 15) +
  theme(axis.text.x =element_text(angle  = 35,hjust = 1,size=10))+
  scale_color_manual(values=wes_palette(n=3, name="GrandBudapest1"))
myplot




