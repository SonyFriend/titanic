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
data<-read.csv("C:/Users/user/Downloads/参璸吭高/titanic.csv",header=T)
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
  labs(title="差康ネ薄猵 ")
ggplot(data=data,aes(x=survived,y=fare,fill=as.factor(survived)))+
  geom_boxplot(alpha = 0.5, show.legend = FALSE)+
  geom_jitter(position=position_jitter(width=0.3, height=0.2),aes(colour=as.factor(survived)), alpha=0.9)+
  theme(plot.title = element_text(hjust = 0.5,size=20))+labs(title="Boxplot of survived vs fare", colour="survived",fill="survived")

ggplot(data=data,aes(x=survived,y=age,fill=as.factor(survived)))+
  geom_boxplot(alpha = 0.5, show.legend = FALSE)+
  geom_jitter(position=position_jitter(width=0.3, height=0.2),aes(colour=as.factor(survived)), alpha=0.9)+
  theme(plot.title = element_text(hjust = 0.5,size=20))+
  labs(title="Boxplot of survived vs age", colour="survived",fill="survived")
####################################################
location<-data.frame(locat=c("queenstown","cherbourg","southampton","Titanic"),lon=c(-8.29,-1.668305,-1.4705326,-49.945839),lat=c(51.85,49.6415363,50.913908,41.732128))
df<-data.frame(lon=location$lon,lat=location$lat)
map <- get_googlemap(center=c(lon=mean(df$lon)-10,lat=mean(df$lat)),language = "zh-TW", zoom =4, markers = df, scale = 2,maptype="roadmap")
ggmap(map)+geom_text(label="queenstown",x=-8.29,y=51.85,size=6)+geom_text(label="cherbourg",x=-1.668305,y=49.6415363,size=6)+geom_text(label="southampton",x=-1.4705326,y=50.913908,size=6)+geom_text(label="Titanic",x=-49.945839,y=41.732128,size=6)
###########################################################################
grep("Jack|Rose",data$name,value=TRUE)
data1<-NA
for(i in 1:1309){if(data[i,2]==0){data1<-rbind(data1,data[i,])}}
data1<-data1[-1,]
name <- as.character(data1$name)
title <- sapply(name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][3]})
which.max(table(title))
table(title)[332]
################################################
data <- knnImputation(data[,-c(12:14)])
data$pclass<-as.factor(data$pclass)
data$survived<-as.factor(data$survived)
levels(data$embarked)[1]<-"S"#场だ常琌southampton差
fit<-glm(survived~pclass+sex+age+sibsp,data,family=binomial)
########################################################################
set.seed(30)
n<-round(0.2*nrow(data))
test<-sample(1:nrow(data),n)
train<-data[-test,]
test<-data[test,]
tree<-rpart(survived~pclass+sex+age+sibsp+parch+fare+embarked,data=train,method="class")
rattle::fancyRpartPlot(tree)
predict.train<-predict(tree,train,type="class")
table(real=train$survived,predict=predict.train)####非絋瞯0.84
predict.test<-predict(tree,test,type="class")
mat<-table(real=test$survived,predict=predict.test)#####非絋瞯0.76
numFolds = trainControl( method = "cv", number = 10)
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002))  
cv1<-train(as.factor(survived)~pclass+sex+age+sibsp+parch+fare+embarked,data=train,method="rpart",trControl = numFolds, tuneGrid = cpGrid )
new_tree = rpart(survived~pclass+sex+age+sibsp+parch+fare+embarked,data=train, method='class', cp=0.014)
predict.test1<-predict(new_tree,test,type="class")
mat1<-table(real=test$survived,predict=predict.test1)#######非絋瞯0.78
rattle::fancyRpartPlot(new_tree)
######################################################
set.seed(30)
data <- knnImputation(data[,-c(12:14)])
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
