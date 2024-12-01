# regression analysis of WM tracts and extracted clusters with WHR and diet
#
# Daria EA Jensen Nov 2022


# load packages, note that you need to use install.packages("name") first ----
library(clv)
library(methods)
library(rgl)
library(longitudinalData)
library(kml)
library(ggplot2)
library(pracma)
library(readr)
library(data.table)
library(cluster)
library(class)
library(psych)
library(misc3d)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library("ggpubr")


# load data ----
setwd("XX/scripts/") # set path

# load DTI participant groups:
ahei_data <- read.csv("XX.csv")
metab_data <- read.csv("XX.csv")

# bilateral WM tracts -----
metab_data$MD_WM_ILF = (metab_data$MD_WM_R_ILF + metab_data$MD_WM_L_ILF)/2;
ahei_data$MD_WM_ILF = (ahei_data$MD_WM_R_ILF + ahei_data$MD_WM_L_ILF)/2;
metab_data$RD_WM_ILF = (metab_data$RD_WM_R_ILF + metab_data$RD_WM_L_ILF)/2;
ahei_data$RD_WM_ILF = (ahei_data$RD_WM_R_ILF + ahei_data$RD_WM_L_ILF)/2;
metab_data$FA_WM_ILF = (metab_data$FA_WM_R_ILF + metab_data$FA_WM_L_ILF)/2;
ahei_data$FA_WM_ILF = (ahei_data$FA_WM_R_ILF + ahei_data$FA_WM_L_ILF)/2;
metab_data$L1_WM_ILF = (metab_data$L1_WM_R_ILF + metab_data$L1_WM_L_ILF)/2;
ahei_data$L1_WM_ILF = (ahei_data$L1_WM_R_ILF + ahei_data$L1_WM_L_ILF)/2;
metab_data$MD_WM_CINGULUM = (metab_data$MD_WM_R_CINGULUM + metab_data$MD_WM_L_CINGULUM)/2;
ahei_data$MD_WM_CINGULUM = (ahei_data$MD_WM_R_CINGULUM + ahei_data$MD_WM_L_CINGULUM)/2;
metab_data$RD_WM_CINGULUM = (metab_data$RD_WM_R_CINGULUM + metab_data$RD_WM_L_CINGULUM)/2;
ahei_data$RD_WM_CINGULUM = (ahei_data$RD_WM_R_CINGULUM + ahei_data$RD_WM_L_CINGULUM)/2;
metab_data$FA_WM_CINGULUM = (metab_data$FA_WM_R_CINGULUM + metab_data$FA_WM_L_CINGULUM)/2;
ahei_data$FA_WM_CINGULUM = (ahei_data$FA_WM_R_CINGULUM + ahei_data$FA_WM_L_CINGULUM)/2;
metab_data$L1_WM_CINGULUM = (metab_data$L1_WM_R_CINGULUM + metab_data$L1_WM_L_CINGULUM)/2;
ahei_data$L1_WM_CINGULUM = (ahei_data$L1_WM_R_CINGULUM + ahei_data$L1_WM_L_CINGULUM)/2;

# multiply each MD, RD and L1 tract x 1000
metab_data$MD_WM_ILF_1000 = metab_data$MD_WM_ILF * 1000;
ahei_data$MD_WM_ILF_1000 = ahei_data$MD_WM_ILF * 1000;
metab_data$RD_WM_ILF_1000 = metab_data$RD_WM_ILF* 1000;
ahei_data$RD_WM_ILF_1000 = ahei_data$RD_WM_ILF* 1000;
metab_data$L1_WM_ILF_1000 = metab_data$L1_WM_ILF * 1000;
ahei_data$L1_WM_ILF_1000 = ahei_data$L1_WM_ILF* 1000;
metab_data$MD_WM_CINGULUM_1000 = metab_data$MD_WM_CINGULUM* 1000;
ahei_data$MD_WM_CINGULUM_1000 = ahei_data$MD_WM_CINGULUM* 1000;
metab_data$RD_WM_CINGULUM_1000 = metab_data$RD_WM_CINGULUM* 1000;
ahei_data$RD_WM_CINGULUM_1000 = ahei_data$RD_WM_CINGULUM * 1000;
metab_data$L1_WM_CINGULUM_1000 = metab_data$L1_WM_CINGULUM* 1000;
ahei_data$L1_WM_CINGULUM_1000 = ahei_data$L1_WM_CINGULUM* 1000;
metab_data$MD_WM_FORNIX_1000 = metab_data$MD_WM_FORNIX * 1000;
ahei_data$MD_WM_FORNIX_1000 = ahei_data$MD_WM_FORNIX * 1000;
metab_data$RD_WM_FORNIX_1000 = metab_data$RD_WM_FORNIX* 1000;
ahei_data$RD_WM_FORNIX_1000 = ahei_data$RD_WM_FORNIX* 1000;
metab_data$L1_WM_FORNIX_1000 = metab_data$L1_WM_FORNIX * 1000;
ahei_data$L1_WM_FORNIX_1000 = ahei_data$L1_WM_FORNIX* 1000;


# save file with art_pres and adjusted tracts: ----
write.csv(ahei_data,'XX.csv');
write.csv(metab_data,'XX.csv');

# Supplementary 1.7 ----
# mean and SD for each: diffusivity * 1000
# for ahei
#global WM
FA <- cbind(round(mean(ahei_data$FA,na.rm = TRUE),digits=2),round(sd(ahei_data$FA,na.rm = TRUE),digits=2))
MD <- cbind(round(mean(ahei_data$MD*1000,na.rm = TRUE),digits=2),round(sd(ahei_data$MD*1000,na.rm = TRUE),digits=2))
RD <- cbind(round(mean(ahei_data$RD*1000,na.rm = TRUE),digits=2),round(sd(ahei_data$RD*1000,na.rm = TRUE),digits=2))
AD <- cbind(round(mean(ahei_data$L1*1000,na.rm = TRUE),digits=2),round(sd(ahei_data$L1*1000,na.rm = TRUE),digits=2))
#tracts:
FA_ILF <- cbind(round(mean(ahei_data$FA_WM_ILF,na.rm = TRUE),digits=2),round(sd(ahei_data$FA_WM_ILF,na.rm = TRUE),digits=2))
FA_CINGULUM <- cbind(round(mean(ahei_data$FA_WM_CINGULUM,na.rm = TRUE),digits=2),round(sd(ahei_data$FA_WM_CINGULUM,na.rm = TRUE),digits=2))
FA_FORNIX <- cbind(round(mean(ahei_data$FA_WM_FORNIX,na.rm = TRUE),digits=2),round(sd(ahei_data$FA_WM_FORNIX,na.rm = TRUE),digits=2))
MD_ILF <- cbind(round(mean(ahei_data$MD_WM_ILF_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$MD_WM_ILF_1000,na.rm = TRUE),digits=2))
MD_CINGULUM <- cbind(round(mean(ahei_data$MD_WM_CINGULUM_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$MD_WM_CINGULUM_1000,na.rm = TRUE),digits=2))
MD_FORNIX <- cbind(round(mean(ahei_data$MD_WM_FORNIX_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$MD_WM_FORNIX_1000,na.rm = TRUE),digits=2))
RD_ILF <- cbind(round(mean(ahei_data$RD_WM_ILF_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$RD_WM_ILF_1000,na.rm = TRUE),digits=2))
RD_CINGULUM <- cbind(round(mean(ahei_data$RD_WM_CINGULUM_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$RD_WM_CINGULUM_1000,na.rm = TRUE),digits=2))
RD_FORNIX <- cbind(round(mean(ahei_data$RD_WM_FORNIX_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$RD_WM_FORNIX_1000,na.rm = TRUE),digits=2))
AD_ILF <- cbind(round(mean(ahei_data$L1_WM_ILF_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$L1_WM_ILF_1000,na.rm = TRUE),digits=2))
AD_CINGULUM <- cbind(round(mean(ahei_data$L1_WM_CINGULUM_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$L1_WM_CINGULUM_1000,na.rm = TRUE),digits=2))
AD_FORNIX <- cbind(round(mean(ahei_data$L1_WM_FORNIX_1000,na.rm = TRUE),digits=2),round(sd(ahei_data$L1_WM_FORNIX_1000,na.rm = TRUE),digits=2))
#table:
rbind(FA,MD,RD,AD)
rbind(FA_ILF,FA_CINGULUM,FA_FORNIX,MD_ILF,MD_CINGULUM,MD_FORNIX,RD_ILF,RD_CINGULUM,RD_FORNIX,AD_ILF,AD_CINGULUM,AD_FORNIX)

# for WHR:
#global WM
FA <- cbind(round(mean(metab_data$FA,na.rm = TRUE),digits=2),round(sd(metab_data$FA,na.rm = TRUE),digits=2))
MD <- cbind(round(mean(metab_data$MD*1000,na.rm = TRUE),digits=2),round(sd(metab_data$MD*1000,na.rm = TRUE),digits=2))
RD <- cbind(round(mean(metab_data$RD*1000,na.rm = TRUE),digits=2),round(sd(metab_data$RD*1000,na.rm = TRUE),digits=2))
AD <- cbind(round(mean(metab_data$L1*1000,na.rm = TRUE),digits=2),round(sd(metab_data$L1*1000,na.rm = TRUE),digits=2))
#tracts:
FA_ILF <- cbind(round(mean(metab_data$FA_WM_ILF,na.rm = TRUE),digits=2),round(sd(metab_data$FA_WM_ILF,na.rm = TRUE),digits=2))
FA_CINGULUM <- cbind(round(mean(metab_data$FA_WM_CINGULUM,na.rm = TRUE),digits=2),round(sd(metab_data$FA_WM_CINGULUM,na.rm = TRUE),digits=2))
FA_FORNIX <- cbind(round(mean(metab_data$FA_WM_FORNIX,na.rm = TRUE),digits=2),round(sd(metab_data$FA_WM_FORNIX,na.rm = TRUE),digits=2))
MD_ILF <- cbind(round(mean(metab_data$MD_WM_ILF_1000,na.rm = TRUE),digits=2),round(sd(metab_data$MD_WM_ILF_1000,na.rm = TRUE),digits=2))
MD_CINGULUM <- cbind(round(mean(metab_data$MD_WM_CINGULUM_1000,na.rm = TRUE),digits=2),round(sd(metab_data$MD_WM_CINGULUM_1000,na.rm = TRUE),digits=2))
MD_FORNIX <- cbind(round(mean(metab_data$MD_WM_FORNIX_1000,na.rm = TRUE),digits=2),round(sd(metab_data$MD_WM_FORNIX_1000,na.rm = TRUE),digits=2))
RD_ILF <- cbind(round(mean(metab_data$RD_WM_ILF_1000,na.rm = TRUE),digits=2),round(sd(metab_data$RD_WM_ILF_1000,na.rm = TRUE),digits=2))
RD_CINGULUM <- cbind(round(mean(metab_data$RD_WM_CINGULUM_1000,na.rm = TRUE),digits=2),round(sd(metab_data$RD_WM_CINGULUM_1000,na.rm = TRUE),digits=2))
RD_FORNIX <- cbind(round(mean(metab_data$RD_WM_FORNIX_1000,na.rm = TRUE),digits=2),round(sd(metab_data$RD_WM_FORNIX_1000,na.rm = TRUE),digits=2))
AD_ILF <- cbind(round(mean(metab_data$L1_WM_ILF_1000,na.rm = TRUE),digits=2),round(sd(metab_data$L1_WM_ILF_1000,na.rm = TRUE),digits=2))
AD_CINGULUM <- cbind(round(mean(metab_data$L1_WM_CINGULUM_1000,na.rm = TRUE),digits=2),round(sd(metab_data$L1_WM_CINGULUM_1000,na.rm = TRUE),digits=2))
AD_FORNIX <- cbind(round(mean(metab_data$L1_WM_FORNIX_1000,na.rm = TRUE),digits=2),round(sd(metab_data$L1_WM_FORNIX_1000,na.rm = TRUE),digits=2))
#table:
rbind(FA,MD,RD,AD)
rbind(FA_ILF,FA_CINGULUM,FA_FORNIX,MD_ILF,MD_CINGULUM,MD_FORNIX,RD_ILF,RD_CINGULUM,RD_FORNIX,AD_ILF,AD_CINGULUM,AD_FORNIX)



# TABLE 3: ----
# estimate associations between WHR/diet and WM tracts.
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# WHR
x <- metab_data$intercept
#x <- metab_data$t # n.s.

lm1_WM1 <- lm(x ~ FA_WM_FORNIX + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm2_WM1 <- lm(x ~ FA_WM_ILF + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm3_WM1 <- lm(x ~ FA_WM_CINGULUM + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm1_WM2 <- lm(x ~ MD_WM_FORNIX_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm2_WM2 <- lm(x ~ MD_WM_ILF_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm3_WM2 <- lm(x ~ MD_WM_CINGULUM_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm1_WM3 <- lm(x ~ RD_WM_FORNIX_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm2_WM3 <- lm(x ~ RD_WM_ILF_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm3_WM3 <- lm(x ~ RD_WM_CINGULUM_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm1_WM4 <- lm(x ~ L1_WM_FORNIX_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm2_WM4 <- lm(x ~ L1_WM_ILF_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)
lm3_WM4 <- lm(x ~ L1_WM_CINGULUM_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA, data=metab_data)

# check specific values with:
#summary(lm1_WM1)
#confint(lm1_WM1)

# extract Beta, CI and p-value from all regression models:
tab_model(lm1_WM1,lm1_WM2,lm1_WM3,lm1_WM4,lm2_WM1,lm2_WM2,lm2_WM3,lm2_WM4,lm3_WM1,lm3_WM2,lm3_WM3,lm3_WM4,
          auto.label = FALSE,show.est=TRUE,show.stat=FALSE,show.intercept =FALSE,collapse.se = TRUE,show.obs = FALSE,show.r2 = FALSE,
          digits = 2,
          title = "WHR intercept",
          #dv.labels = c("FA_Fornix","MD_Fornix","RD_Fornix","AD_Fornix",...), 
          string.est = "Beta",string.p = "p-value",
          p.style = "numeric", digits.p = 3,
          rm.terms = c("sex","scanner","OX.AGE","OX.CHAM_MA","OX.EDUC_MOCA"))

# AHEI
x <- ahei_data$slopeAHEI
#x <- ahei_data$interceptAHEI #n.s.

lm1_WM1 <- lm(x ~ FA_WM_FORNIX + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm2_WM1 <- lm(x ~ FA_WM_ILF + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm3_WM1 <- lm(x ~ FA_WM_CINGULUM + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm1_WM2 <- lm(x ~ MD_WM_FORNIX_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm2_WM2 <- lm(x ~ MD_WM_ILF_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm3_WM2 <- lm(x ~ MD_WM_CINGULUM_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm1_WM3 <- lm(x ~ RD_WM_FORNIX_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm2_WM3 <- lm(x ~ RD_WM_ILF_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm3_WM3 <- lm(x ~ RD_WM_CINGULUM_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm1_WM4 <- lm(x ~ L1_WM_FORNIX_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm2_WM4 <- lm(x ~ L1_WM_ILF_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)
lm3_WM4 <- lm(x ~ L1_WM_CINGULUM_1000 + sex + scanner + OX.AGE + OX.EDUC_MOCA + OX.CHAM_MA + OX.BMI +mean.energy.intake, data=ahei_data)

# check specific values with:
#summary(lm1_WM1)
#confint(lm1_WM1)

# extract Beta, CI and p-value from all regression models:
tab_model(lm1_WM1,lm1_WM2,lm1_WM3,lm1_WM4,lm2_WM1,lm2_WM2,lm2_WM3,lm2_WM4,lm3_WM1,lm3_WM2,lm3_WM3,lm3_WM4,
          auto.label = FALSE,show.est=TRUE,show.stat=FALSE,show.intercept =FALSE,collapse.se = TRUE,show.obs = FALSE,show.r2 = FALSE,
          title = "AHEI slope",
          digits = 2,
          #dv.labels = c("FA_Fornix","MD_Fornix","RD_Fornix","AD_Fornix",...), 
          string.est = "Beta",string.p = "p-value",
          p.style = "numeric", digits.p = 3,
          rm.terms = c("sex","scanner","OX.AGE","OX.CHAM_MA","OX.BMI","OX.EDUC_MOCA","mean.energy.intake"))


# SCATTERPLOTS Figure 2 and 3 ----
library(devtools)
library(moonBook) 
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)

# previously Figure 2 and Figure 3 now. Supplementary figures in 4
# extracted cluster of global WM effects
x <- ahei_data$slopeAHEI
p1 <- ggplot(ahei_data,aes(y=cluster_FA_R1,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("slope AHEI-2010") + ylab("Cluster of global FA")
p2 <- ggplot(ahei_data,aes(y=cluster_MD_R1,x=x))+geom_point()+geom_smooth(method="lm",color="darkblue", fill="grey")+theme_minimal() + ggtitle("") + xlab("slope AHEI-2010") + ylab("Cluster of global MD")
p3 <- ggplot(ahei_data,aes(y=cluster_L1_R1,x=x))+geom_point()+geom_smooth(method="lm",color="darkblue", fill="grey")+theme_minimal() + ggtitle("") + xlab("slope AHEI-2010") + ylab("Cluster of global AD")
# save plot:
tmp=sprintf(paste("AHEIslope_WMglobal_scatter_R1.svg",sep ="", collapse=NULL))
g1 <- grid.arrange(p1, p2, p3, nrow=1)# combine in one plot
ggsave(file=tmp, g1, width = 6, height = 2)
dev.off()


# previous : Figure 3
# extracted cluster of global WM effects
x <- metab_data$intercept
p1 <- ggplot(metab_data,aes(y=cluster_MD_R1,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("Cluster of global MD")
p2 <- ggplot(metab_data,aes(y=cluster_RD_R1,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("Cluster of global RD")
p3 <- ggplot(metab_data,aes(y=cluster_FA_R1,x=x))+geom_point()+geom_smooth(method="lm",color="darkblue", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("Cluster of global FA")
p4 <- ggplot(metab_data,aes(y=cluster_MDneg_R1,x=x))+geom_point()+geom_smooth(method="lm",color="darkblue", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("Cluster of global MD")
# save plot:
tmp=sprintf(paste("WHRintercept_WMglobal_scatter_R1.svg",sep ="", collapse=NULL))
g2 <- grid.arrange(p1, p2,p3,p4,nrow=2)# combine in one plot
ggsave(file=tmp, g2, width = 4, height = 2)
dev.off()




# For: AHEI slope ...
x <- ahei_data$slopeAHEI
p1 <- ggplot(ahei_data,aes(y=L1_WM_FORNIX_1000,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("slope AHEI-2010") + ylab("AD of Fornix")
# save plot:
tmp=sprintf(paste("AHEIslope_WMtracts_scatter_R1.svg",sep ="", collapse=NULL))
ggsave(file=tmp, p1, width = 2, height = 2)
dev.off()


# For: WHR intercept ILF of FA, MD, RD and Cingulum MD RD
x <- metab_data$intercept
p1 <- ggplot(metab_data,aes(y=FA_WM_ILF,x=x))+geom_point()+geom_smooth(method="lm",color="darkblue", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("FA of ILF")
p2 <- ggplot(metab_data,aes(y=FA_WM_CINGULUM,x=x))+geom_point()+geom_smooth(method="lm",color="darkblue", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("FA of Cingulum")
p3 <- ggplot(metab_data,aes(y=RD_WM_ILF_1000,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("RD of ILF")
p4 <- ggplot(metab_data,aes(y=MD_WM_CINGULUM_1000,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("MD of Cingulum")
p5 <- ggplot(metab_data,aes(y=RD_WM_CINGULUM_1000,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept WHR") + ylab("RD of Cingulum")
# save plot:
tmp=sprintf(paste("WHRintercept_WMtracts_scatter_R1.svg",sep ="", collapse=NULL))
g <- grid.arrange(p1, p2, p3, p4, p5, nrow=2)# combine in one plot
ggsave(file=tmp, g, width = 6, height = 4)
dev.off()

