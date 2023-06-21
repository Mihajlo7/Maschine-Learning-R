data<-read.csv("dataSets/hospital_ratings.csv",stringsAsFactors = F)

crh_vars<-c(2:9,11:16)

apply(data[,crh_vars],2,function(x) sum(is.na(x)))

data$County[data$County==""]<-NA

shapiro.test(data$Overall.rating)
data$Overall.rating[is.na(data$Overall.rating)]<-median(data$Overall.rating,na.rm = T)

### City
unique(data$City)
###
unique(data$State)
data<-subset(data,State=="TX" | State=="CA" | State=="FL" | State=="PA")

data$State<-as.factor(data$State)
###
data$ID<-NULL
data$Name<-NULL
data$Address<-NULL
data$City<-NULL

unique(data$County)
data$County<-NULL

apply(data,2,function(x) sum(is.na(x)))
### Type
unique(data$Type)
data$Type<-as.factor(data$Type)
unique(data$Ownership)
data$Ownerhship_New[startsWith(data$Ownership,"Voluntary")]<-"Volunatary"
data$Ownerhship_New[startsWith(data$Ownership,"Goverment")]<-"Goverment"
data$Ownerhship_New[is.na(data$Ownerhship_New)]<-"Private"
data$Ownership<-as.factor(data$Ownership)
data$Ownerhship_New<-as.factor(data$Ownerhship_New)
data$Emergency.Services<-as.factor(data$Emergency.Services)
data$Can.use.EHRs<-as.factor(data$Can.use.EHRs)

unique(data$Mortality.national.comparison)

data$Mortality.national.comparison<-factor(
  data$Mortality.national.comparison,
  levels = c("Below the national average","Same as the national average","Above the national average")
)
table(data$Mortality.national.comparison)
data$Mortality.national.comparison[is.na(data$Mortality.national.comparison)]<-"Same as the national average"

#### Readmission
unique(data$Readmission)
data$Readmission<-factor(
  data$Readmission,
  levels = c("Below the national average","Same as the national average","Above the national average")
)
table(data$Readmission)
data$Readmission[is.na(data$Readmission)]<-"Same as the national average"
### Patient expirence
unique(data$Patient.experience)
data$Patient.experience<-factor(
  data$Patient.experience,
  levels = c("Below the national average","Same as the national average","Above the national average")
)
table(data$Patient.experience)
data$Patient.experience[is.na(data$Patient.experience)]<-"Below the national average"
### Effectiveness of care
unique(data$Effectiveness.of.care)
data$Effectiveness.of.care<-factor(
  data$Effectiveness.of.care,
  levels = c("Below the national average","Same as the national average","Above the national average")
)
table(data$Effectiveness.of.care)
data$Effectiveness.of.care[is.na(data$Effectiveness.of.care)]<-"Same as the national average"
### Timelines
unique(data$Timeliness.of.care)
data$Timeliness.of.care<-factor(
  data$Timeliness.of.care,
  levels = c("Below the national average","Same as the national average","Above the national average")
)
table(data$Timeliness.of.care)
data$Timeliness.of.care[is.na(data$Timeliness.of.care)]<-"Same as the national average"
data$rating<-ifelse(data$Overall.rating>3,"High","Low")
data$rating<-as.factor(data$rating)
data$Overall.rating<-NULL

#### grafici
library(ggplot2)
### Posto na grafiku uvizdjamo da ne postoji statitiscki znacajna razlika, ne korisitimo
ggplot(data,aes(x=State,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
back<-data
data$State<-NULL

ggplot(data,aes(x=Type,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
# isto vazi
data$Type<-NULL
ggplot(data,aes(x=Ownership,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
ggplot(data,aes(x=Ownerhship_New,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
data$Ownership<-NULL

ggplot(data,aes(x=Can.use.EHRs,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
ggplot(data,aes(x=State,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
ggplot(data,aes(x=Mortality.national.comparison,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
ggplot(data,aes(x=Readmission,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
ggplot(data,aes(x=Patient.experience,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
ggplot(data,aes(x=Effectiveness.of.care,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
ggplot(data,aes(x=Timeliness.of.care,fill=rating))+
  geom_bar(position = "dodge")+
  theme_classic()
