library(tidyverse)
library(MESS)
library(scales)
library(viridis)
library(ggpubr)
library(ggsignif)

# Read in metrics, ROC, PRC

met <- read.csv("RS_MULTI_MET.csv",header=TRUE,fileEncoding='UTF-8-BOM')
met$Model <- as.factor(met$Model)
met$Model <- factor(met$Model,levels=c('LR','SVM','RF','GBT','PFN','MLP'))
met$Data <- as.factor(met$Data)

prc <- read.csv("RS_MULTI_PRC.csv",header=TRUE,fileEncoding='UTF-8-BOM')
prc.mlp <- read.csv("RS_MLP_PRC.csv",header=TRUE,fileEncoding='UTF-8-BOM')
prc <- rbind(prc, prc.mlp)
prc$M <- as.factor(prc$M)
prc$M <- factor(prc$M,levels=c('LR','SVM','RF','GBT','PFN','MLP'))
prc$M2 <- as.factor(sub("\\..*", "", prc$M))

roc <- read.csv("RS_MULTI_ROC.csv",header=TRUE,fileEncoding='UTF-8-BOM')
roc.mlp <- read.csv("RS_MLP_ROC.csv",header=TRUE,fileEncoding='UTF-8-BOM')
roc <- rbind(roc, roc.mlp)
roc$M <- factor(roc$M,levels=c('LR','SVM','RF','GBT','PFN','MLP'))
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
  scale_fill_viridis(discrete=T),
  guides(fill=guide_legend(ncol=2))
)

p1 <- ggplot(met,aes(fill=Model,x=Recall,y=Precision))+l1+
  scale_x_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.2,1),breaks=c(0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.6,1),breaks=c(0.6,0.7,0.8,0.9,1))+
  xlab('Recall')+ylab('Precision')
  

p2 <- ggplot(met,aes(fill=Model,x=Accuracy,y=F1_Score))+l1+
  scale_x_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.6,1),breaks=c(0.6,0.7,0.8,0.9,1))+
  scale_y_continuous(expand=c(0,0),labels=scales::number_format(accuracy=0.1),
                     limits=c(0.4,1),breaks=c(0.4,0.6,0.8,1))+
  xlab('Accuracy')+ylab('F1 Score')

tiff("RS_REC-PRE.tiff",height=2,width=3.6,units='in',res=300,compression="lzw")
p1
dev.off()

tiff("RS_ACC-F1S.tiff",height=2,width=3.6,units='in',res=300,compression="lzw")
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
  facet_wrap(M2~.,ncol=3)
)
l3 <- list(
  geom_line(data=rcl2,aes(x=x,y=y),lty=2,linewidth=0.5,color='black'),
  geom_path(aes(x=R,y=P,color=M2,group=M),linewidth=1),
  xlab('Recall'),
  ylab('Precision'),
  geom_text(aes(label=AUC,color=M2),x=0.5,y=0.15,size=5,hjust=0)
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

tiff("RS_CURVES.tiff",height=6,width=4,units='in',res=300,compression="lzw")
ggarrange(p3,p4,nrow=2,labels=c("A","B"))
dev.off()

# Parameter comparisons

# Comparing datatypes
l5 <- list(
  t1,
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()),
  ylab('F1 Score'),
  geom_violin(aes(x=Data,y=F1_Score,fill=Data),linewidth=1),
  scale_y_continuous(limits=c(0.2,1.2),breaks=c(0.2,0.6,1.0))
)
p5 <- ggplot(met)+l5

# Deleted vs Imputed
met2 <- met
met2$Data <- fct_collapse(met2$Data, Deleted=c('S','SR','SO','SRO'),
                          Imputed=c('IS','ISR','ISO','ISRO'))
met2$Data <- factor(met2$Data, levels=c('Deleted','Imputed'))
p6 <- ggplot(met2)+l5+
  geom_signif(aes(x=Data,y=F1_Score),map_signif_level=TRUE,
              test=t.test,test.args=list(paired=T),y_position=1.05,
              comparisons=list(c('Deleted','Imputed')))
t.test(met2$'F1_Score'[met2$'Data'=='Deleted'],
       met2$'F1_Score'[met2$'Data'=='Imputed'],
       paired=TRUE)$p.value

# Imbalanced vs Balanced
met3 <- met
met3$Data <- fct_collapse(met3$Data, Imbalanced=c('S','SR','IS','ISR'),
                          Balanced=c('SO','SRO','ISO','ISRO'))
p7 <- ggplot(met3)+l5+
  geom_signif(aes(x=Data,y=F1_Score),map_signif_level=TRUE,
              test=t.test,test.args=list(paired=T),,y_position=1.05,
              comparisons=list(c('Imbalanced','Balanced')))
t.test(met3$'F1_Score'[met3$'Data'=='Imbalanced'],
       met3$'F1_Score'[met3$'Data'=='Balanced'],
       paired=TRUE)$p.value

# Unaltered vs Reduced
met4 <- met
met4 <- droplevels(met4[!met4$Model=='PFN',])
met4$Data <- fct_collapse(met4$Data, Unaltered=c('S','IS','SO','ISO'),
                          Reduced=c('SR','SRO','ISR','ISRO'))
p8 <- ggplot(met4)+l5+
  geom_signif(aes(x=Data,y=F1_Score),map_signif_level=TRUE,
              test=t.test,test.args=list(paired=T),,y_position=1.05,
              comparisons=list(c('Unaltered','Reduced')))
t.test(met4$'F1_Score'[met4$'Data'=='Unaltered'],
       met4$'F1_Score'[met4$'Data'=='Reduced'],
       paired=TRUE)$p.value

tiff("RS_DT.tiff",height=4,width=3,units='in',res=300,compression="lzw")
ggarrange(p6,p7,p8,nrow=3,labels=c("A","B","C"),align='v')
dev.off()