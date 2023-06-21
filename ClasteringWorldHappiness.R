##############
data<-read.csv("dataSets/world-happiness-report-2017.csv",stringsAsFactors = F)
##########
## Varijable Country i Happiness.Rank su jedinstvene i nemaju prediktivnu moc
data$Country<-NULL
data$Happiness.Rank<-NULL
## Provera NA vrednosti
apply(data,2,function(x) sum(is.na(x)))

apply(data,2,shapiro.test)

### Genosity ####
data$Generosity[is.na(data$Generosity)]<-mean(data$Generosity,na.rm = T)

### Health.Life>Expactancy
data$Health.Life.Expectancy[is.na(data$Health.Life.Expectancy)]<-median(data$Health.Life.Expectancy,na.rm = T)

which(complete.cases(data)==F)
##########
##ALGORITAM ####

#### 1. Winzorizing
apply(data,2,function(x) length(boxplot.stats(x)$out))
library(DescTools)

## Family
boxplot(data$Family)
family_w<-Winsorize(data$Family,probs = c(0.05,0.95))
boxplot(family_w)
## Trust. Goverment
boxplot(data$Trust.Government.Corruption)
trust_w<-Winsorize(data$Trust.Government.Corruption,probs = c(0.05,0.95))
boxplot(trust_w)

data$Family<-family_w
data$Trust.Government.Corruption<-trust_w

source('Utility.R')
### 2. Normalizacija ###
norm_data<-apply(data,2,normalize.feature)
norm_data<-as.data.frame(norm_data)

### 3. Provera korelacija
library(corrplot)
norm_data$Happiness.Score<-NULL
norm_data$Whisker.high<-NULL
norm_data$Whisker.low<-NULL
norm_data$Economy.GDP.per.Capita<-NULL
cor_matrix<-cor(norm_data)
corrplot.mixed(cor_matrix)
##### Model ###
set.seed(4623)
kmn<-kmeans(norm_data,centers = 4,iter.max = 20,nstart = 1000)
kmn

########
eval_measures<-data.frame()
for(k in 2:8){
  set.seed(4623)
  res<-kmeans(norm_data,centers = k,iter.max = 20,nstart = 1000)
  eval_measures<-rbind(eval_measures,c(k,res$tot.withinss,res$betweenss/res$totss))
}
colnames(eval_measures)<-c("k","tot.withinss","ratio")
eval_measures

library(ggplot2)
ggplot(data = eval_measures,aes(x=k,y=tot.withinss))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = 2:8)+
  theme_classic()

diff_df<-apply(eval_measures[,2:3],2,compute.difference)
diff_df<-cbind(k=2:8,diff_df)
diff_df

set.seed(4623)
kmn1<-kmeans(norm_data,3,20,1000)

kmn1_stats<-summary.stats(norm_data,kmn1$cluster,3)
kmn1_stats
