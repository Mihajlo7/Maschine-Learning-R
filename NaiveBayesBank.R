###############
## Ucitivamo dataSet
data<-read.csv("dataSets/bank.csv",stringsAsFactors = F)

rm(chr_vars)
rm(num_vars)

### Kreiranje izlazne varijable
any(is.na(data$Exited))
data$Stayed<-ifelse(data$Exited==0,"yes","no")
data$Stayed<-as.factor(data$Stayed)
data$Exited<-NULL
#### Napraviti podskup koji ne sadrzi klijente starije od 87 godina
any(is.na(data$Age))

## Sredjivanje varijable Age

sum(data$Age=="" | data$Age=="-" | data$Age=="-",na.rm = T)
data$Age[data$Age=="" | data$Age=="-" | data$Age=="-"]<-NA

data$Age<-as.numeric(data$Age)

shapiro.test(data$Age)

data$Age[is.na(data$Age)]<-median(data$Age,na.rm = T)

dataSub<-subset(data,Age<87)
data<-dataSub
rm(dataSub)
### Procena atributa za model naivnog bayesa ###
unique(data$Surname)
## Surname -iskljuciti jer ima previse razlicith vrednosti
data$Surname<-NULL

chr_vars<-c("Geography","Gender","Card.Type")

apply(data[,chr_vars],2,function(x) sum(x=="" | x=="-" | x==" ",na.rm = T))
data$Geography[data$Geography=="" | data$Geography=="-" | data$Geography==" "]<-NA
### Sredjivanje numerickih varijabli ###
num_vars<-c(1,4:11,13)
apply(data[,num_vars],2,function(x) sum(is.na(x)))
#### Pretvaranje u numeric tip sve
data[,num_vars]<-apply(data[,num_vars],2,function(x) as.numeric(x))
######## Geography ######
any(is.na(data$Geography))
unique(data$Geography)
table(data$Geography)
data$Geography[is.na(data$Geography)]<-"France"
data$Geography<-as.factor(data$Geography)
########### Gender #####
unique(data$Gender)
data$Gender<-as.factor(data$Gender)
########### Card.Type #####
unique(data$Card.Type)
data$Card.Type<-factor(data$Card.Type,levels = c("SILVER","GOLD","PLATINUM","DIAMOND"))

data$HasCrCard<-factor(data$HasCrCard,levels = 0:1,labels = c("no","yes"))
data$IsActiveMember<-factor(data$IsActiveMember,levels = 0:1,labels = c("no","yes"))
###############
which(complete.cases(data)==F)
###############
library(ggplot2)
###############
apply(data[,num_vars],2,shapiro.test)
##### Credit Score ###
ggplot(data,aes(x=CreditScore,fill=Stayed))+
  geom_density(alpha=0.55)+
  theme_classic()
# Na prvi pogled izgleda da ne postoji statisticka znacajna razlika izmedju ovih oblati
wilcox.test(data$CreditScore[data$Stayed=="yes"],data$CreditScore[data$Stayed=="no"])
#### ne postoji statisticka znacajna razlika pa je odbacujemo
data$CreditScore<-NULL
#### Age ####
ggplot(data,aes(x=Age,fill=Stayed))+
  geom_density(alpha=0.55)+
  theme_classic()
#### postoji statisticka znacajna razlika izmedju ove dve oblasti pa uzimamo ovu promenljivu
#### Tanure #####
ggplot(data,aes(x=Tenure,fill=Stayed))+
  geom_density(alpha=0.55)+
  theme_classic()
# Na prvi pogled izgleda da ne postoji statisticka znacajna razlika izmedju ovih oblati
wilcox.test(data$Tenure[data$Stayed=="yes"],data$Tenure[data$Stayed=="no"])
### ne postoji statisticki znacajna razlika izmedju ove dve oblaste pa je odbacujemo
data$Tenure<-NULL
#### Balance #####
ggplot(data,aes(x=Balance,fill=Stayed))+
  geom_density(alpha=0.55)+
  theme_classic()
### postoji statisticki znacajna razlika, prihvatamo prom
#### Num of products ####
ggplot(data,aes(x=NumOfProducts,fill=Stayed))+
  geom_density(alpha=0.55)+
  theme_classic()
### postoji statisticki znacajna razlika, prihvatamo
#### Estimated Salary
ggplot(data,aes(x=EstimatedSalary,fill=Stayed))+
  geom_density(alpha=0.55)+
  theme_classic()
# Na prvi pogled izgleda da ne postoji statisticka znacajna razlika izmedju ovih oblati
wilcox.test(data$EstimatedSalary[data$Stayed=="yes"],data$EstimatedSalary[data$Stayed=="no"])
### ne postoji statisticki znacajna razlika, ovu promenljivu necemo uzimati
data$EstimatedSalary<-NULL
#### Satisfactional score ###
ggplot(data,aes(x=Satisfaction.Score,fill=Stayed))+
  geom_density(alpha=0.55)+
  theme_classic()
wilcox.test(data$Satisfaction.Score[data$Stayed=="yes"],data$Satisfaction.Score[data$Stayed=="no"])
### ne postoji statisticki znacajna razlika izmedju ovih promenljivi, pa je odbacujemo
data$Satisfaction.Score<-NULL
### Point Earned ###
ggplot(data,aes(x=Point.Earned,fill=Stayed))+
  geom_density(alpha=0.55)+
  theme_classic()
### na prvi pogled ne postoji statisticki znacajna razlika
wilcox.test(data$Point.Earned[data$Stayed=="yes"],data$Point.Earned[data$Stayed=="no"])
data$Point.Earned<-NULL
############################
### Geography ###
ggplot(data,aes(x=Geography,fill=Stayed))+
  geom_bar(position = "fill")+
  theme_classic()
### Gender ####
ggplot(data,aes(x=Gender,fill=Stayed))+
  geom_bar(position = "fill")+
  theme_classic()
### prihvatamo ovu jer nisu iste proporcije
### HasCrCard ####
ggplot(data,aes(x=HasCrCard,fill=Stayed))+
  geom_bar(position = "fill")+
  theme_classic()
### ista je proporcija, pa cemo ovu varijablu odbaciti
data$HasCrCard<-NULL
#### IsActiveMember ####
ggplot(data,aes(x=IsActiveMember,fill=Stayed))+
  geom_bar(position = "fill")+
  theme_classic()
### prihvatamo jer ne postoje iste proporcije ###
#### Card Type ####
ggplot(data,aes(x=Card.Type,fill=Stayed))+
  geom_bar(position = "fill")+
  theme_classic()
data$Card.Type<-NULL
################ ALGORITAM ###############
#### Diskretizacija #####
apply(data[,3:5],2,shapiro.test)
library(bnlearn)
transform<-discretize(data[,3:4],method = "quantile",breaks = c(5,2))
#### Promenljiva NumOfProducts ne moze da se podeli u grupe sa jednakim brojem opservacija, pa cemo je odbaciti
### Naivni Bayes nije najbolje resenje za ovaj problem
ggplot(data,aes(x=Balance))+
  geom_histogram()+
  theme_classic()

ggplot(data,aes(x=NumOfProducts))+
  geom_histogram()+
  theme_classic()
coll_to_add<-setdiff(colnames(data),colnames(transform))
transform<-cbind(transform,data[,coll_to_add])

transform$NumOfProducts<-NULL
############ Podela na trainig i test #############
library(caret)

set.seed(4623)
train_ind<-createDataPartition(transform$Stayed,p=0.8,list = F)

train<-transform[train_ind,]
test<-transform[-train_ind,]
library(e1071)
nb1<-naiveBayes(Stayed~.,data = train)
summary(nb1)
nb1
######################
compute_eval_metrics<-function(cmatrix){
  TP<-cmatrix[2,2]
  TN<-cmatrix[1,1]
  FP<-cmatrix[1,2]
  FN<-cmatrix[2,1]
  
  a<-(TP+TN)/sum(cmatrix)
  p<-TP/(TP+FP)
  r<-TP/(TP+FN)
  f1<-2*p*r/(p+r)
  
  c(accuracy=a,precision=p,recall=r,F1=f1)
  
}
##########################
nb1_pred<-predict(nb1,newdata = test,type = "class")
cm1<-table(actual=test$Stayed,predicted=nb1_pred)
cm1
eval1<-compute_eval_metrics(cm1)
eval1

######### ROC KRIVE ########
nb1_pred_probs<-predict(nb1,newdata = test,type = "raw")
nb1_pred_probs
head(nb1_pred_probs)
library(pROC)
nb1_roc<-roc(response=as.integer(test$Stayed),
             predictor=nb1_pred_probs[,2],
             levels=c(1,2))
nb1_roc$auc

plot.roc(nb1_roc,
         print.thres = 'best',
         print.thres.best.method = 'youden')

nb1_coords<-coords(roc=nb1_roc,
                   x="local maximas",
                   ret=c("threshold","specificity","sensitivity","accuracy"),
                   transpose=F)
nb1_coords
nb1_coords$sum<-nb1_coords$specificity+nb1_coords$sensitivity
nb1_coords
nb1_coords$threshold[max(nb1_coords$sum)]
sort(nb1_coords$sum,decreasing = T)
best_threshold<-nb1_coords[34,1]

nb1_pred2<-ifelse(nb1_pred_probs[,2]>best_threshold,"yes","no")
cm2<-table(actual=test$Stayed,predicted=nb1_pred2)
cm2
eval2<-compute_eval_metrics(cm2)
data.frame(rbind(eval1,eval2),row.names = c("eval1","eval2"))
cm1
