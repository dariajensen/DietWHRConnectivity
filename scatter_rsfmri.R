# scatterplot for ahei slope - L and R hippo cluster extraction from rsfmri seed based correlation analysis
#
# Daria Jensen Nov 2024

# load packages, note that you need to use install.packages("name") first
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
library(jtools)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# load data ----
setwd("XX/scripts/") # set path for the Whitehall II cohort
ahei_data <- read.csv("XX.csv")

#scatter
x <- ahei_data$interceptAHEI
p1 <- ggplot(ahei_data,aes(y=Rhippo2all,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept AHEI-2010") + ylab("Cluster of R Hippocampus seed")
p2 <- ggplot(ahei_data,aes(y=Lhippo2all,x=x))+geom_point()+geom_smooth(method="lm",color="darkred", fill="grey")+theme_minimal() + ggtitle("") + xlab("intercept AHEI-2010") + ylab("Cluster of L Hippocampus seed")

# save plot:
tmp=sprintf(paste("AHEIintercept_RLhipposeed_scatter.svg",sep ="", collapse=NULL))
g1 <- grid.arrange(p1, p2, nrow=1)# combine in one plot
ggsave(file=tmp, g1, width = 4, height = 2)
dev.off()


