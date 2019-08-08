library(ggmap)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(RGtk2)
library(caret)
library(e1071)
library(DMwR)
library(Epi)
library(arules)
data<-read.csv("C:/Users/user/Downloads/統計諮詢/titanic.csv",header=T)
a<-0
for(i in 1:1309){if(data[i,1]==1&data[i,2]==1){a<-a+1}}
b<-0
for(i in 1:1309){if(data[i,1]==2&data[i,2]==1){b<-b+1}}
c<-0
for(i in 1:1309){if(data[i,1]==3&data[i,2]==1){c<-c+1}}
dat<-data.frame(freq=c(a,323-a,b,277-b,c,709-c),pclass=c("pclass1","pclass1","pclass2","pclass2","pclass3","pclass3"),status=c("survive","dead","survive","dead","survive","dead"))
ggplot(data=dat,aes(x=pclass,y=freq,fill=status))+
  geom_bar( stat="identity", position=position_dodge())+
  geom_text(aes(label=freq),vjust=1.6,color="black",position =
              position_dodge(0.9), size=3.5)+
  theme(plot.title = element_text(hjust = 0.5,size=20))+
  labs(title="各船艙生存情況 ")
ggplot(data=data,aes(x=survived,y=fare,fill=as.factor(survived)))+
  geom_boxplot(alpha = 0.5, show.legend = FALSE)+
  geom_jitter(position=position_jitter(width=0.3, height=0.2),aes(colour=as.factor(survived)), alpha=0.9)+
  theme(plot.title = element_text(hjust = 0.5,size=20))+labs(title="Boxplot of survived vs fare", colour="survived",fill="survived")

ggplot(data=data,aes(x=survived,y=age,fill=as.factor(survived)))+
  geom_boxplot(alpha = 0.5, show.legend = FALSE)+
  geom_jitter(position=position_jitter(width=0.3, height=0.2),aes(colour=as.factor(survived)), alpha=0.9)+
  theme(plot.title = element_text(hjust = 0.5,size=20))+
  labs(title="Boxplot of survived vs age", colour="survived",fill="survived")
##############遺漏值處理######################
data <- knnImputation(data[,-c(12:14)]) (k defaults to 10)
data$pclass<-as.factor(data$pclass)
data$survived<-as.factor(data$survived)
levels(data$embarked)[1]<-"S"#因大部分乘客都是在southampton上船
fit<-glm(survived~pclass+sex+age+sibsp,data,family=binomial)
########################################################################
set.seed(30)
n<-round(0.2*nrow(data)) 
test<-sample(1:nrow(data),n)
train<-data[-test,]
test<-data[test,] #80% for training set,20% for test set
tree<-rpart(survived~pclass+sex+age+sibsp+parch+fare+embarked,data=train,method="class")
rattle::fancyRpartPlot(tree) #visualize tree
predict.train<-predict(tree,train,type="class")
table(real=train$survived,predict=predict.train)####Accuracy is about 0.84
predict.test<-predict(tree,test,type="class")
mat<-table(real=test$survived,predict=predict.test)#####Accuracy is about 0.76
numFolds = trainControl( method = "cv", number = 10)
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002))  
cv1<-train(as.factor(survived)~pclass+sex+age+sibsp+parch+fare+embarked,data=train,method="rpart",trControl = numFolds, tuneGrid = cpGrid )
new_tree = rpart(survived~pclass+sex+age+sibsp+parch+fare+embarked,data=train, method='class', cp=0.014) #prune the tree
predict.test1<-predict(new_tree,test,type="class")
mat1<-table(real=test$survived,predict=predict.test1)#######Accuracy is about 0.78
rattle::fancyRpartPlot(new_tree)#visualize tree
######################################################
new_data<-data
for(i in 1:1309){if(new_data[i,5]<3 && new_data[i,5]>=0){new_data[i,5]<-0}
  else if(new_data[i,5]>=3 && new_data[i,5]<12){new_data[i,5]<-1}
  else if(new_data[i,5]>=12 && new_data[i,5]<18){new_data[i,5]<-2}
  else if(new_data[i,5]>=18 && new_data[i,5]<28){new_data[i,5]<-3}
  else if(new_data[i,5]>=28 && new_data[i,5]<50){new_data[i,5]<-4}
  else{new_data[i,5]<-5}}
new_data$age<-as.factor(new_data$age)
new_data$pclass<-as.factor(new_data$pclass)
new_data$survived<-as.factor(new_data$survived)
levels(new_data$age)<-c("baby","child","teenager","young","adult","old")
new_data<-data.frame(new_data[,1],new_data[,2],new_data[,4],new_data[,5])
colnames(new_data)<-c("pclass","survived","sex","age")
rule<-apriori(new_data,parameter = list(minlen=3,supp=0.1,conf=0.7),appearance = list(default="lhs",rhs=c("survived=0","survived=1")))
sort.rule <- sort(rule, by="support")
subset.matrix <- is.subset(x=sort.rule, y=sort.rule)
subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
sort.rule <- sort.rule[!redundant]
inspect(sort.rule)
