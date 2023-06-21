##############
data<-read.csv("dataSets/airbnb_.csv",stringsAsFactors = F)

### podela samo na one koje imaju vise od 11 sadrzaja ###

data$count<-sapply(data$amenities,function(x) length(unlist(strsplit(x,","))))

data<-subset(data,count>11)
###### podela podataka tako da spadaju samo one koje su poznate
data<-subset(data,!is.na(review_scores_rating))

###### Provera nedostajucih vrednosti #####
chr_vars<-c(2,3,8,9)

apply(data[,chr_vars],2,function(x) sum(x=="-" | x=="" | x==" ",na.rm = T))

apply(data[,chr_vars],2,function(x) sum(is.na(x)))

num_vars<-c(1,4:7,10:12)

apply(data[,num_vars],2,function(x) sum(is.na(x)))
##### bathrooms, bedrooms, beds

apply(data[,5:7],2,shapiro.test)

data[,5:7]<-apply(data[,5:7],2,function(x) ifelse(is.na(x),yes = median(x,na.rm = T),x))
#### Sredjene promenljive ####
#### Prebacivanje price promenljive u numeric

data$price<-substr(data$price,start = 2,stop = length(data$price))
data$price<-as.numeric(data$price)
data$price[is.na(data$price)]<-1200
##### Sredjivanje varijabli za model ####
data$high_reviews<-ifelse(data$number_of_reviews>90,"High","Low")
data$high_reviews<-as.factor(data$high_reviews)
data$number_of_reviews<-NULL
## Id i name cemo izbaciti jer imaju previse razlicit vrednosti i nemaju pred. moc
data$id<-NULL
data$name<-NULL
unique(data$property_type)
data$property<-ifelse(data$property_type %in% c("Apartment","Condominium","Loft"),"Flat","House")
table(data$property)
data$property_type<-NULL
### Pretvorili smo promenljivu property_type u property sa dve grupe podataka
data$property<-as.factor(data$property)
library(ggplot2)
#### accommodates
ggplot(data,aes(x=accommodates,fill=high_reviews))+
  geom_density(alpha=0.55)+
  theme_classic()
# na prvi pogled ima razlike ali provericemo pomocu neparametarskog testa
wilcox.test(data$accommodates[data$high_reviews=="High"],data$accommodates[data$high_reviews=="Low"])
shapiro.test(data$accommodates)
## Dobijamo da ne posotji statisticki znacajna razlika, pa odbacujemo promenljivu
data$accommodates<-NULL
### bathrooms
ggplot(data,aes(x=bathrooms,fill=high_reviews))+
  geom_density(alpha=0.55)+
  theme_classic()
# na prvi pogled ima razlike ali provericemo pomocu neparametarskog testa
wilcox.test(data$bathrooms[data$high_reviews=="High"],data$bathrooms[data$high_reviews=="Low"])
### test odbacuje H0 hipotezu da postoji statisticki znacajna razlika, ali za malo pa cemo 
### o ovoj promenljivoj odlucutu na kraju
### bedrooms
ggplot(data,aes(x=bedrooms,fill=high_reviews))+
  geom_density(alpha=0.55)+
  theme_classic()

# na prvi pogled ima razlike ali provericemo pomocu neparametarskog testa
wilcox.test(data$bedrooms[data$high_reviews=="High"],data$bedrooms[data$high_reviews=="Low"])
#### ne postoji statisticki znacajna razlika izmedju ove dve oblasti pa je odbacujemo
data$bedrooms<-NULL
###beds ###
ggplot(data,aes(x=beds,fill=high_reviews))+
  geom_density(alpha=0.55)+
  theme_classic()

wilcox.test(data$beds[data$high_reviews=="High"],data$beds[data$high_reviews=="Low"])
### ne postoji statisticki znacajna razlika izmedju ove dve oblasti pa je odbacujemo
data$beds<-NULL
### amenties ###
### Ovu promenljivu cemo odbaciti zbog njene slabe prediktivne moci
data$amenities<-NULL
### price ###
ggplot(data,aes(x=price,fill=high_reviews))+
  geom_density(alpha=0.55)+
  theme_classic()
wilcox.test(data$price[data$high_reviews=="High"],data$price[data$high_reviews=="Low"])

### minimum nights #####
ggplot(data,aes(x=minimum_nights,fill=high_reviews))+
  geom_density(alpha=0.55)+
  theme_classic()
wilcox.test(data$minimum_nights[data$high_reviews=="High"],data$minimum_nights[data$high_reviews=="Low"])
