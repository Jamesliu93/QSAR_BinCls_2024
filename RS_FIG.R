library(tidyverse)
library(scales)
library(viridis)

met <- read.csv("RS_MULTI_MET.csv",header=TRUE,fileEncoding='UTF-8-BOM')
met$Model <- as.factor(met$Model)

prc <- read.csv("RS_MULTI_PRC.csv",header=TRUE,fileEncoding='UTF-8-BOM')
prc$M <- as.factor(prc$M)

roc <- read.csv("RS_MULTI_ROC.csv",header=TRUE,fileEncoding='UTF-8-BOM')
roc$M <- as.factor(roc$M)

t1 <- theme_bw()+
  theme(text=element_text(size=10,color="black"),
        legend.text=element_text(size=10,color="black"),
        axis.title.x=element_text(size=10,color="black"),
        axis.title.y=element_text(size=10,color="black"),
        axis.text.y=element_text(size=10,color="black"),
        axis.text.x=element_text(size=10,color="black",angle=45,vjust=0.7)
        )

l1 <- list(
  t1,
  geom_point(pch=21,size=3,stroke=0.7,color='black'),
  scale_fill_viridis(discrete=T)
)

p1 <- ggplot(met,aes(fill=Model,x=Recall,y=Precision))+l1+
  scale_x_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.3,0.8),breaks=c(0.3,0.4,0.5,0.6,0.7,0.8))+
  scale_y_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.5,1),breaks=c(0.5,0.6,0.7,0.8,0.9,1))+
  xlab('Recall')+ylab('Precision')
  

p2 <- ggplot(met,aes(fill=Model,x=Accuracy,y=F1_Score))+l1+
  scale_x_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.5,1),breaks=c(0.5,0.6,0.7,0.8,0.9,1))+
  scale_y_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.4,0.9),breaks=c(0.4,0.5,0.6,0.7,0.8,0.9))+
  xlab('Accuracy')+ylab('F1 Score')

tiff("RS_REC-PRE.tiff",height=2,width=3.25,units='in',res=300,compression="lzw")
p1
dev.off()

tiff("RS_ACC-F1S.tiff",height=2,width=3.25,units='in',res=300,compression="lzw")
p2
dev.off()

l2 <- list(
  t1,
  geom_path(aes(x=R,y=P,color=M),alpha=0.5),
  scale_color_viridis(discrete=T),
  xlab('Recall'),
  ylab('Precision')
)
l3 <- list(
  t1,
  geom_path(aes(x=F,y=T,color=M),alpha=0.5),
  scale_color_viridis(discrete=T),
  xlab('False Positive Rate'),
  ylab('True Positive Rate')
)

p3 <- ggplot(prc)+l2
p4 <- ggplot(roc)+l3

tiff("RS_PRC.tiff",height=4,width=6,units='in',res=300,compression="lzw")
p3
dev.off()
tiff("RS_ROC.tiff",height=4,width=6,units='in',res=300,compression="lzw")
p4
dev.off()