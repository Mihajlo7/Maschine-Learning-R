#####

data<-read.csv("dataSets/airbnb_.csv",stringsAsFactors = F)

sum(is.na(data$review_scores_rating))
#### Napraviti podskup onih koje nisu NA #######
data<-data[!is.na(data$review_scores_rating),]

##### Vraca broj amenties za svaku nekretninu ###
data$count<-sapply(data$amenities,
                   function(x) length(unlist(strsplit(x,split = ","))))

data<-subset(data,count>11)
############
## SELEKCIJA ATRIBUTA #######

### Atribut id necemo koristiti
data$id<-NULL
### Atribut name necemo korisiti jer je karakternog tipa i ne moze se poredjati ordinalno
data$name<-NULL
#### Kolonu maenties necemo korisiti jer smo od nje napravili kolonu count koja nosi dovoljno informacija
data$amenities<-NULL

chr_vars<-c(1,6)

apply(data[,chr_vars],2,function(x) sum(x=="" || x=="-" || x=="?"))
#### Nema nedostajacuh vrednosti za vraijable karakternog tipa
### accommodes ###
unique(data$property_type)
data$property_type<-as.factor(data$property_type)
##### pretvorili smo u faktorsku promenljivu
####### price ###########
data$price<-trimws(data$price,whitespace = "\$")

data$price<-substr(data$price,start = 2,stop = length(data$price))
data$price<-as.numeric(data$price)
data$price[is.na(data$price)]<-1200
######### Numeric atributi #######
num_vars<-c(2,3,4,5,7,8,9)

apply(data[,num_vars],2,function(x) sum(is.na(x)))
#### bathrooms,bedrooms,beds####
num_vars<-c("bathrooms","bedrooms","beds")
apply(data[,num_vars],2,shapiro.test)
#### Nijedan ne podlaze normalnoj raspodeli tako da ga menjamo sa median

data$bathrooms[is.na(data$bathrooms)]<-median(data$bathrooms,na.rm = T)
data$bedrooms[is.na(data$bedrooms)]<-median(data$bedrooms,na.rm = T)
data$beds[is.na(data$beds)]<-median(data$beds,na.rm = T)

###### Provera znacajnosti#####
back<-data
### Postoji linearna zavisnost
library(ggplot2)
ggplot(data,aes(x=accommodates,y=price))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)
  theme_classic()
#### isto je i sa promenljivom bathrooms
library(ggplot2)
ggplot(data,aes(x=bathrooms,y=price))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
#### Ne postoji linearna zavisnost izmedju ove dve pr tako da cu je iskljuciti
library(ggplot2)
ggplot(data,aes(x=review_scores_rating,y=price))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
data$review_scores_rating<-NULL
### Postoji linearna zavisnost
library(ggplot2)
ggplot(data,aes(x=bedrooms,y=price))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
#### Malo je bolje stanje sa beds
library(ggplot2)
ggplot(data,aes(x=beds,y=price))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)+
  theme_classic()

library(ggplot2)
ggplot(data,aes(x=bedrooms,y=review_scores_rating))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)+
  theme_classic()

## Treba razmotriti njeno iskljucivanje zbog male linearnosti
library(ggplot2)
ggplot(data,aes(x=minimum_nights,y=price))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
### Treba je iskljuciti jer ne postoji linearna zavisnot
library(ggplot2)
ggplot(data,aes(x=number_of_reviews,y=price))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
data$number_of_reviews<-NULL
### Ne postoji linearna zavisnost izmedju ove dve
library(ggplot2)
ggplot(data,aes(x=count,y=price))+
  geom_point()+
  labs(title = "Dijagram rasipanja")+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
data$count<-NULL
#########
##Posto faktorske farijable ne mogu da se postave ordinalno treba ih iskljuciti iz daljih razmatranja
##
data$property_type<-NULL
library(corrplot)
cor_matrix<-cor(data)
corrplot.mixed(cor_matrix)
## Na osnovu matrice korelacije uvidjamo da treba iskljuciti promenljivu minimum_nights
data$minimum_nights<-NULL

##### LINEARNA REGRESIJA #######
library(caret)

set.seed(4623)
train_indeces<-createDataPartition(data$price,p=0.8,list = F)

train<-data[train_indeces,]
test<-data[-train_indeces,]
###################

lm1<-lm(price~.,data = data)
summary(lm1)

### Na osnovu modela vidimo da beds nije znacajan prediktor tako da cemo ga izbaciti iz modela
lm2<-lm(price~.-beds,data)
summary(lm2)

par(mfrow=c(2,2))
plot(lm2)
par(mfrow=c(1,1))

lm2_pred<-predict(lm2,newdata = test)
head(lm2_pred)
lm_TSS<-sum((test$price-mean(train$price))^2)
lm2_RSS<-sum((test$price-lm2_pred)^2)
lm2_R2<-1-lm2_RSS/lm_TSS

lm2_RMSE<-sqrt(lm2_RSS/nrow(test))
lm2_RMSE/mean(test$price)

#####################
## VIF ####
library(car)
sort(sqrt(vif(lm1)))
### Sve vrednosti su ispod 2 tako da je model prihvatljiv
lm1_pred<-predict(lm1,newdata = test)
lm1_RSS<-sum((lm1_pred-test$price)^2)
lm1_R2<-1-lm1_RSS/lm_TSS
lm1_RMSE<-sqrt(lm1_RSS/nrow(test))
lm1_RMSE/mean(test$price)
#### Prihvatamo model lm2 jer je jednostavniji a metrike su svuda slicne
########
## R -oznacava koliko posto varijable price je obajsnjeno uz pomoc nezavisnih varijavli
## RMSE- koliku gresku pravimo sa modelom/ koliko su opservacije blizu predvidjenim vrednostima
