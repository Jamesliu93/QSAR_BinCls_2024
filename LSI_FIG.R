library(tidyverse)
library(scales)
library(viridis)

met <- read.csv("LSI_MULTI_MET.csv",header=TRUE,fileEncoding='UTF-8-BOM')
met$Model <- as.factor(met$Model)

t1 <- theme_bw()+
  theme(text=element_text(size=10,color="black"),
        legend.text=element_text(size=10,color="black"),
        axis.title.x=element_text(size=10,color="black"),
        axis.title.y=element_text(size=10,color="black"),
        axis.text.y=element_text(size=10,color="black"),
        axis.text.x=element_text(size=10,color="black",angle=45,vjust=0.7)
        )

pl <- list(
  t1,
  geom_point(pch=21,size=3,stroke=0.7,color='black'),
  scale_fill_viridis(discrete=T)
)

p1 <- ggplot(met,aes(fill=Model,x=Recall,y=Precision))+pl+
  scale_x_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.2,1),breaks=c(0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.5,0.9),breaks=c(0.5,0.6,0.7,0.8,0.9))+
  xlab('Recall')+ylab('Precision')
  

p2 <- ggplot(met,aes(fill=Model,x=Accuracy,y=F1_Score))+pl+
  scale_x_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.5,0.9),breaks=c(0.5,0.6,0.7,0.8,0.9))+
  scale_y_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.2,1),breaks=c(0.2,0.4,0.6,0.8,1))+
  xlab('Accuracy')+ylab('F1 Score')

tiff("LSI_REC-PRE.tiff",height=2,width=3.25,units='in',res=300,compression="lzw")
p1
dev.off()

tiff("LSI_ACC-F1S.tiff",height=2,width=3.25,units='in',res=300,compression="lzw")
p2
dev.off()