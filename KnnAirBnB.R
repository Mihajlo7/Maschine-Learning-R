########
data<-read.csv("dataSets/airbnb_.csv",stringsAsFactors = F)

### postavljamo da review_scores_rating nisu NA
data<-subset(data,!is.na(review_scores_rating))
### poseduje preko 11 razlicitih sadrzaja
data$count<-sapply(data$amenities,
                   function(x) length(unlist(strsplit(x,split = ","))))
data<-subset(data,count>11)
#### Sredjivanje podataka
data$id<-NULL
data$name<-NULL
data$amenities<-NULL

### Provera nedostajucih vrednosti

chr_vars<-c(1,6)

apply(data[,chr_vars],2,function(x) sum(x=="" || x=="-" || x=="?"))
## Pretvaranje u kategoriscku
data$property_type<-as.factor(data$property_type)

## Sredjivanje price varijable
data$price<-substr(data$price,start = 2,stop = length(data$price))
data$price<-as.numeric(data$price)
data$price[is.na(data$price)]<-1200

num_vars<-c(2:5,7:10)

apply(data[,num_vars],2,function(x) sum(is.na(x)))

apply(data[,num_vars],2,shapiro.test)

## bathrooms, bedrooms, beds imaju NA vrednosti
num_na<-c("bathrooms","bedrooms","beds")

data$bathrooms[is.na(data$bathrooms)]<-median(data$bathrooms,na.rm = T)
data$bedrooms[is.na(data$bedrooms)]<-median(data$bedrooms,na.rm=T)
data$beds[is.na(data$beds)]<-median(data$beds,na.rm = T)
####### Pravljenje varijable price

data$price<-ifelse(data$price>median(data$price),"yes","no")
data$price<-as.factor(data$price)
data$expensive<-data$price
data$price<-NULL
levels(data$expensive)
######## Selekcija atributa######
library(ggplot2)

#### property type ###
ggplot(data,aes(x=property_type,fill=expensive))+
  geom_bar(position = "dodge")+
  theme_classic()
levels(data$property_type)
table(data$property_type)
data$property_features<-ifelse(data$property_type %in% c("Apartment","Condominium","Loft"),"Flat","House")
unique(data$property_features)
table(data$property_features)
data$property_type<-NULL
ggplot(data,aes(x=property_features,fill=expensive))+
  geom_bar(position = "dodge")+
  theme_classic()
data$property_features<-as.factor(data$property_features)
#### mozemo reci da postoji statisticki znacajna razlika izmedju grupa pa prihvatamo property_fetaures

#####
ggplot(data = data,aes(x=accommodates,fill=expensive))+
  geom_density(alpha=0.55)+
  theme_bw()
### mozemo reci da postoji statisticki znacajna razlika za ove dve vrednosti
ggplot(data = data,aes(x=bathrooms,fill=expensive))+
  geom_density(alpha=0.55)+
  theme_bw()
wilcox.test(data$bathrooms[data$expensive=="yes"],data$bathrooms[data$expensive=="no"])
### postoji statisticki znacajna razlika i upotrebicemo varijablu bathrooms
ggplot(data = data,aes(x=bedrooms,fill=expensive))+
  geom_density(alpha=0.55)+
  theme_bw()
### posotji statisticka znacajna razlika pa cemo je koristiti
ggplot(data = data,aes(x=beds,fill=expensive))+
  geom_density(alpha=0.55)+
  theme_bw()
wilcox.test(data$bedrooms[data$expensive=="yes"],data$bedrooms[data$expensive=="no"])
### beds-postoji statisticka znacajna razlika
ggplot(data = data,aes(x=minimum_nights,fill=expensive))+
  geom_density(alpha=0.55)+
  theme_bw()
unique(data$minimum_nights)
wilcox.test(data$minimum_nights[data$expensive=="yes"],data$minimum_nights[data$expensive=="no"])
### minimum nights- postoji statisticki znacajna razlika
ggplot(data = data,aes(x=number_of_reviews,fill=expensive))+
  geom_density(alpha=0.55)+
  theme_bw()
wilcox.test(data$number_of_reviews[data$expensive=="yes"],data$number_of_reviews[data$expensive=="no"])
#### Mozemo reci da ne posotji statisticki znacajna razlika, pa cemo je izbaciti
data$number_of_reviews<-NULL

ggplot(data = data,aes(x=review_scores_rating,fill=expensive))+
  geom_density(alpha=0.55)+
  theme_bw()
### Mozemo reci da ne postoji statisticki znacajna razlika pa cemo je odbaciti
data$review_scores_rating<-NULL

ggplot(data = data,aes(x=count,fill=expensive))+
  geom_density(alpha=0.55)+
  theme_bw()
wilcox.test(data$count[data$expensive=="yes"],data$count[data$expensive=="no"])

data$count<-NULL
######## KNN ALGORITAM

### Proveravanje outlajera
num_vars<-c(1:5)
apply(data[,num_vars],2,function(x) length(boxplot.stats(x)$out))
## Imaju outlajere treba standardizacija
apply(data[,num_vars],2,shapiro.test)
### Sve trebaju po mediani
transformed<-apply(data[,num_vars],2,function(x) scale(x,center = median(x),scale = IQR(x)))
transformed<-as.data.frame(transformed)

library(DescTools)
boxplot(data$bedrooms)
bedrooms_w<-Winsorize(data$bedrooms,probs=c(0.05,0.50))
data$bedrooms<-NULL
transformed$bedrooms<-NULL

transformed$property<-as.numeric(data$property_features)
transformed$expensive<-data$expensive
######
library(caret)

set.seed(4623)
train_ind<-createDataPartition(transformed$expensive,p=0.8,list = F)

train<-transformed[train_ind,]
test<-transformed[-train_ind,]
#######
## Krosvalidacija ###
library(e1071)
tr_Control<-trainControl(method = "cv",number = 10)
tune_Grid<-expand.grid(.k=seq(3,25,2))

set.seed(4623)
knn_cv<-train(x=train[,-6],
              y=train$expensive,
              method="knn",
              trControl=tr_Control,
              tuneGrid=tune_Grid)
plot(knn_cv)

best_k<-knn_cv$bestTune$k

#########
library(class)
knn1_pred<-knn(train[,-6],test[,-6],cl=train$expensive,k=best_k)

cm1<-table(actual=test$expensive,predicted=knn1_pred)
cm1

compute_eval_metrics<-function(cmatrix){
  TP<-cmatrix[2,2]
  TN<-cmatrix[1,1]
  FP<-cmatrix[1,2]
  FN<-cmatrix[2,1]
  
  a<-(TP+TN)/sum(cmatrix)
  p<-TP/(TP+FP)
  r<-TP/(TP+FN)
  f1<-2*p*r/(p+r)
  
  c(accutracy=a,precision=p,recall=r,F1=f1)
  
}
eval1<-compute_eval_metrics(cm1)
eval1
