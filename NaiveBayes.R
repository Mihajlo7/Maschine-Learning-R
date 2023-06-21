######
data<-read.csv("dataSets/countries.csv",stringsAsFactors = F)
str(data)
## Sredjivanje NA i pretvaranje char u numeric

### Ispitivanje da li postoje NA vrednsoti
apply(data[,c(1,2,5:20)],2,function(x) sum(x=="" | x=="-" ))

data[,c(1,2,5:20)]<-apply(data[,c(1,2,5:20)],2,function(x) ifelse(x=="" | x=="-" | x==" ",NA,x))
apply(data[,c(1,2,5:20)],2,function(x) sum(is.na(x)))

### Pretvaranje char u numeric ######
num_vars<-c(5:20)

data[,num_vars]<-apply(data[,num_vars],2,function(x) as.numeric(gsub(",",".",x)))

apply(data[,num_vars],2,function(x) sum(is.na(x)))

## Provera raspodele

apply(data[,num_vars],2,shapiro.test)

### samo service ima normalnu raspodelu
data[,c(5:19)]<-apply(data[,c(5:19)],2,function(x) ifelse(is.na(x),median(x,na.rm = T),x))
data$Service[is.na(data$Service)]<-mean(data$Service,na.rm = T)
###### PRAVLJENJE SUBSETA ####
data$Region<-trimws(data$Region)
data<-subset(data,Migration<20 & Region=="SUB-SAHARAN AFRICA")

#### DODAVANJE IZLAZNE VARIJABLE #####
high_gdp<-quantile(data$GDP,probs = 0.75)
data$High_GDP<-ifelse(data$GDP>high_gdp,"True","False")
data$High_GDP<-as.factor(data$High_GDP)
data$GDP<-NULL

### Sredjivanje kolona
data$Country<-NULL
data$Region<-NULL
data$Arable....<-NULL
data$Crops....<-NULL
data$Other....<-NULL

library(ggplot2)
## Uzumamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Population,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Area,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=PDensity,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Coastline..coast.area.ratio.,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Migration,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=InfantMortility,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Literacy,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Phones,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Climate,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Birthrate,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Deathrate,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Agriculture,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Industry,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
## Uzimamo, postoji znacajna statisticka razlika
ggplot(data,aes(x=Service,fill=High_GDP))+
  geom_density(alpha=0.55)+
  theme_classic()
#####################################
## KNN Algorithm ####

apply(data[,-15],2,function(x)length(boxplot.stats(x)$out))

transform<-as.data.frame(apply(data[,c(1:13)],2,function(x) scale(x,center = median(x),scale = IQR(x))))
transform$Service<-as.vector(scale(data$Service))
transform$High_GDP<-data$High_GDP
##########################
transform$Climate<-NULL
##########################