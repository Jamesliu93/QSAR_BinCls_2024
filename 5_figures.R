library(tidyverse)
library(MESS)
library(scales)
library(viridis)
library(ggpubr)
library(ggsignif)

# Set project

proj <- 'LSI'
proj <- 'RS'

# Read in metrics, ROC, PRC

met <- read.csv(paste(proj,'_MULTI_MET.csv',sep=''),header=TRUE,fileEncoding='UTF-8-BOM')
met$Model <- as.factor(met$Model)
met$Model <- factor(met$Model,levels=c('LR','SVM','RF','GBT','PFN','MLP'))
met$Data <- as.factor(met$Data)

prc <- read.csv(paste(proj,'_MULTI_PRC.csv',sep=''),header=TRUE,fileEncoding='UTF-8-BOM')
prc$M <- as.factor(prc$M)
prc$M2 <- as.factor(sub("\\..*", "", prc$M))
prc$M2 <- factor(prc$M2,levels=c('LR','SVM','RF','GBT','PFN','MLP'))

roc <- read.csv(paste(proj,'_MULTI_ROC.csv',sep=''),header=TRUE,fileEncoding='UTF-8-BOM')
roc$M <- as.factor(roc$M)
roc$M2 <- as.factor(sub("\\..*", "", roc$M))
roc$M2 <- factor(roc$M2,levels=c('LR','SVM','RF','GBT','PFN','MLP'))

# Calculate and record AUC values

roc.auc <- roc %>%
  group_by(M) %>%
  summarize(AUC = auc(F,T,type='linear'), .groups='keep')
prc.auc <- prc %>%
  group_by(M) %>%
  summarize(AUC = auc(R,P,type='linear'), .groups='keep')
write.csv(roc.auc,file=paste('temp_out/',proj,'.roc.auc.csv',sep=''))
write.csv(prc.auc,file=paste('temp_out/',proj,'.prc.auc.csv',sep=''))

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

tiff(paste('temp_out/',proj,'_REC-PRE.tiff',sep=''),height=2,width=3.6,units='in',res=300,compression="lzw")
p1
dev.off()

tiff(paste('temp_out/',proj,'_ACC-F1S.tiff',sep=''),height=2,width=3.6,units='in',res=300,compression="lzw")
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

tiff(paste('temp_out/',proj,'_CURVES.tiff',sep=''),height=6,width=4,units='in',res=300,compression="lzw")
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

tiff(paste('temp_out/',proj,'_DT.tiff',sep=''),height=4,width=3,units='in',res=300,compression="lzw")
ggarrange(p6,p7,p8,nrow=3,labels=c("A","B","C"),align='v')
dev.off()

# Comparing hyperparameters
l6 <- list(
  t1,
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()),
  ylab('F1 Score'),
  geom_violin(linewidth=1),
  scale_y_continuous(limits=c(0.2,1),breaks=c(0.2,0.6,1.0))
)
met5 <- met[which(met$Model=='RF'),c(2,3,8,10)]
met5$Param_1 <- as.factor(met5$Param_1)
met5$Param_2 <- as.factor(met5$Param_2)
met5$Param_1 <- factor(met5$Param_1,levels=c('50','100','300'))
p9 <- ggplot(met5,aes(x=Param_1,y=F1_Score,fill=Param_1))+l6+
  guides(fill=guide_legend(title='# of Estimators'))
p10 <- ggplot(met5,aes(x=Param_2,y=F1_Score,fill=Param_2))+l6+
  guides(fill=guide_legend(title='Max. Depth'))+
  geom_signif(map_signif_level=TRUE,
              test=t.test,test.args=list(paired=T),,y_position=0.8,
              comparisons=list(c('5','10')))

tiff(paste('temp_out/',proj,'_HP.tiff',sep=''),height=4,width=3,units='in',res=300,compression="lzw")
ggarrange(p9,p10,nrow=2,labels=c("A","B"),align='v')
dev.off()