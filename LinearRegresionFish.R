########
data<-read.csv("Fish.csv",stringsAsFactors = F)
##########
unique(data$ï..Species)

dataSub<-data[data$ï..Species=="Bream",]

##########
data$ï..Species<-NULL

apply(data,2,function(x) sum(is.na(x)))

###########
library(ggplot2)
## Length 1
ggplot(data,aes(x=Length1,y=Weight))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
#### Mozemo reci da postoji velika linearna zavisnost izmedju zavisne i nezavisne

### Length2
ggplot(data,aes(x=Length2,y=Weight))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
#### Mozemo reci da postoji velika linearna zavisnost izmedju zavisne i nezavisne

### Length3
ggplot(data,aes(x=Length3,y=Weight))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
#### Mozemo reci da postoji velika linearna zavisnost izmedju zavisne i nezavisne

### Height
ggplot(data,aes(x=Height,y=Weight))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
#### Mozemo reci da postoji velika linearna zavisnost izmedju zavisne i nezavisne

### Width
ggplot(data,aes(x=Width,y=Weight))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  theme_classic()
#### Mozemo reci da postoji velika linearna zavisnost izmedju zavisne i nezavisne
library(corrplot)
cor_matrix<-cor(data[,c(1,5:6)])
corrplot.mixed(cor_matrix)
