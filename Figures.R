library(tidyverse)
library(ggpubr)

met <- read.csv("LSI_MULTI_MET.csv",header=TRUE,fileEncoding='UTF-8-BOM')
met$Model <- as.factor(met$Model)

t1 <- theme_bw()+
  theme(text=element_text(size=10,color="black"),
        legend.text=element_text(size=10,color="black"),
        axis.title.x=element_text(size=8,color="black"),
        axis.title.y=element_text(size=8,color="black"),
        axis.text.y=element_text(size=8,color="black"),
        axis.text.x=element_text(size=8,color="black",angle=45,vjust=0.7)
        )

p1 <- ggplot(met,aes(fill=Model))+t1+
  scale_x_continuous(expand=c(0,0),limits=c(0,1))+
  scale_y_continuous(expand=c(0,0),limits=c(0.5,1))+
  xlab('Recall')+ylab('Precision')+
  geom_point(aes(x=Recall,y=Precision),size=5,color='black',pch=21)
p1

p2 <- ggplot(met,aes(fill=Model))+t1+
  scale_x_continuous(expand=c(0,0),limits=c(0.5,0.8))+
  scale_y_continuous(expand=c(0,0),limits=c(0,1))+
  xlab('Accuracy')+ylab('F1 Score')+
  geom_point(aes(x=Accuracy,y=F1_Score),size=5,color='black',pch=21)
p2