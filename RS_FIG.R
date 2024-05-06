library(tidyverse)
library(MESS)
library(scales)
library(viridis)
library(ggpubr)

# Read in metrics, ROC, PRC

met <- read.csv("RS_MULTI_MET.csv",header=TRUE,fileEncoding='UTF-8-BOM')
met$Model <- as.factor(met$Model)

prc <- read.csv("RS_MULTI_PRC.csv",header=TRUE,fileEncoding='UTF-8-BOM')
prc$M <- as.factor(prc$M)
prc$M2 <- as.factor(sub("\\..*", "", prc$M))

roc <- read.csv("RS_MULTI_ROC.csv",header=TRUE,fileEncoding='UTF-8-BOM')
roc$M <- as.factor(roc$M)
roc$M2 <- as.factor(sub("\\..*", "", roc$M))

# Calculate and record AUC values

roc.auc <- roc %>%
  group_by(M) %>%
  summarize(AUC = auc(F,T,type='linear'), .groups='keep')
prc.auc <- prc %>%
  group_by(M) %>%
  summarize(AUC = auc(R,P,type='linear'), .groups='keep')
write.csv(roc.auc,file='temp_out/roc.auc.csv')
write.csv(prc.auc,file='temp_out/prc.auc.csv')

# Merge ROC and PRC with AUC values

roc <- roc %>%
  group_by(M) %>%
  mutate(AUC = round(auc(F,T,type='linear'),digits=3))
prc <- prc %>%
  group_by(M) %>%
  mutate(AUC = round(auc(R,P,type='linear'),digits=3))
roc <- roc %>%
  group_by(M2) %>%
  mutate(AUC = max(AUC))
prc <- prc %>%
  group_by(M2) %>%
  mutate(AUC = max(AUC))

# Themes

t1 <- theme_bw()+
  theme(text=element_text(size=10,color="black"),
        legend.text=element_text(size=10,color="black"),
        axis.title.x=element_text(size=10,color="black"),
        axis.title.y=element_text(size=10,color="black"),
        axis.text.y=element_text(size=10,color="black"),
        axis.text.x=element_text(size=10,color="black",angle=45,vjust=0.7),
        strip.background=element_blank()
        )

# Recall-Precision and Accuracy-F1 plots

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

# Performance curves

x <- seq(0,1,0.1)
y <- seq(0,1,0.1)
rcl1 <- data.frame(x,y)
x <- seq(0,1,0.1)
y <- rep(55/158,times=11)
rcl2 <- data.frame(x,y)

l2 <- list(
  t1,
  theme(legend.position='none',
        panel.grid.minor=element_blank()),
  scale_x_continuous(breaks=c(0,0.5,1)),
  scale_y_continuous(breaks=c(0,0.5,1)),
  scale_color_viridis(discrete=T,end=0.9),
  facet_wrap(M2~.,ncol=5)
)
l3 <- list(
  geom_line(data=rcl2,aes(x=x,y=y),lty=2,linewidth=0.5,color='black'),
  geom_path(aes(x=R,y=P,color=M2,group=M),linewidth=1),
  xlab('Recall'),
  ylab('Precision'),
  geom_text(aes(label=AUC,color=M2),x=0.1,y=0.45,size=5,hjust=0)
)
l4 <- list(
  geom_line(data=rcl1,aes(x=x,y=y),lty=2,linewidth=0.5,color='black'),
  geom_path(aes(x=F,y=T,color=M2,group=M),linewidth=1),
  xlab('False Positive Rate'),
  ylab('True Positive Rate'),
  geom_text(aes(label=AUC,color=M2),x=0.5,y=0.15,size=5,hjust=0)
)

p3 <- ggplot(prc)+l2+l3
p4 <- ggplot(roc)+l2+l4

tiff("RS_CURVES.tiff",height=4,width=6.5,units='in',res=300,compression="lzw")
ggarrange(p3,p4,nrow=2,labels=c("A","B"))
dev.off()

# Plots focusing on ANNs

ann.met <- met[which(met$Model=='ANN'),-c(1,4,9,10)]
ann.met$Param_1 <- as.factor(ann.met$Param_1)
ann.met$Param_2 <- as.factor(ann.met$Param_2)
ann.met$Data <- as.factor(ann.met$Data)
colnames(ann.met) <- c('Epochs','Dropout','acc','pre','rec','f1s','Input')
levels(ann.met$Input) <- c('Reduced','Filtered/Scaled')

l5 <- list(
  geom_point(aes(fill=Epochs,shape=Dropout,alpha=Input),
             color='black',stroke=1,size=5),
  scale_shape_manual(values=c(21,22,24)),
  scale_alpha_manual(values=c(0.5,1)),
  guides(fill=guide_legend(override.aes=list(shape=21)))
)

p5 <- ggplot(ann.met,aes(x=acc,y=f1s))+t1+l5+
  xlab('Accuracy')+ylab('F1 Score')+
  scale_x_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.675,0.875),breaks=c(0.7,0.8))+
  scale_y_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.2,0.85),breaks=c(0.2,0.4,0.6,0.8))

tiff("ANN_EX.tiff",height=4,width=6,units='in',res=300,compression="lzw")
p5
dev.off()