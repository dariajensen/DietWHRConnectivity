# script is testing cognitive performance outcome associations with WHR/diet intercept and slope
# Table information and scatter plot for the Paper
#
# Daria Jensen Sep 2022

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
library(jtools)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# load data ----
setwd("XX/scripts/") # set path for the Whitehall II cohort
ahei_data <- read.csv("XX.csv")
metab_data <- read.csv("XX.csv")

# if you want to calculate relationship in the smaller DTI sub-dataset instead, then use:
ahei_data_dti <- read.csv("XX.csv")
metab_data_dti <- read.csv("XX.csv")

ethnicity_data <- read.csv("XX.csv")

# -------- MEAN COGNITIVE OUTCOMES  --------
# see Supplemenatry 8 - Table 

# AHEI  ----
# for rsfmri data sample:
mean_cog_HVLT <- mean(ahei_data$OX.HVLT_TR,na.rm = TRUE)
sd_cog_HVLT <- sd(ahei_data$OX.HVLT_TR,na.rm = TRUE)
mean_cog_DST <- mean(ahei_data$OX.DST,na.rm = TRUE)
sd_cog_DST <- sd(ahei_data$OX.DST,na.rm = TRUE)
mean_cog_FLU_C <- mean(ahei_data$OX.FLU_C,na.rm = TRUE)
sd_cog_FLU_C <- sd(ahei_data$OX.FLU_C,na.rm = TRUE)
mean_cog_FLU_L <- mean(ahei_data$OX.FLU_L,na.rm = TRUE)
sd_cog_FLU_L <- sd(ahei_data$OX.FLU_L,na.rm = TRUE)
mean_cog_DCOD <- mean(ahei_data$OX.DCOD,na.rm = TRUE)
sd_cog_DCOD <- sd(ahei_data$OX.DCOD,na.rm = TRUE)
mean_cog_tmtba <- mean(ahei_data$tmtba,na.rm = TRUE)
sd_cog_tmtba <- sd(ahei_data$tmtba,na.rm = TRUE)
# for dti data sample:
Dmean_cog_HVLT <- mean(ahei_data_dti$OX.HVLT_TR,na.rm = TRUE)
Dsd_cog_HVLT <- sd(ahei_data_dti$OX.HVLT_TR,na.rm = TRUE)
Dmean_cog_DST <- mean(ahei_data_dti$OX.DST,na.rm = TRUE)
Dsd_cog_DST <- sd(ahei_data_dti$OX.DST,na.rm = TRUE)
Dmean_cog_FLU_C <- mean(ahei_data_dti$OX.FLU_C,na.rm = TRUE)
Dsd_cog_FLU_C <- sd(ahei_data_dti$OX.FLU_C,na.rm = TRUE)
Dmean_cog_FLU_L <- mean(ahei_data_dti$OX.FLU_L,na.rm = TRUE)
Dsd_cog_FLU_L <- sd(ahei_data_dti$OX.FLU_L,na.rm = TRUE)
Dmean_cog_DCOD <- mean(ahei_data_dti$OX.DCOD,na.rm = TRUE)
Dsd_cog_DCOD <- sd(ahei_data_dti$OX.DCOD,na.rm = TRUE)
Dmean_cog_tmtba <- mean(ahei_data_dti$tmtba,na.rm = TRUE)
Dsd_cog_tmtba <- sd(ahei_data_dti$tmtba,na.rm = TRUE)
#table
cbind(mean_cog_HVLT,sd_cog_HVLT,mean_cog_DST,sd_cog_DST,mean_cog_FLU_C,sd_cog_FLU_C,mean_cog_FLU_L,sd_cog_FLU_L,mean_cog_DCOD,sd_cog_DCOD,mean_cog_tmtba,sd_cog_tmtba) 
cbind(Dmean_cog_HVLT,Dsd_cog_HVLT,Dmean_cog_DST,Dsd_cog_DST,Dmean_cog_FLU_C,Dsd_cog_FLU_C,Dmean_cog_FLU_L,Dsd_cog_FLU_L,Dmean_cog_DCOD,Dsd_cog_DCOD,Dmean_cog_tmtba,Dsd_cog_tmtba) 

# WHR  ----
# for rsfmri data sample:
mean_cog_HVLT <- mean(metab_data$OX.HVLT_TR,na.rm = TRUE)
sd_cog_HVLT <- sd(metab_data$OX.HVLT_TR,na.rm = TRUE)
mean_cog_DST <- mean(metab_data$OX.DST,na.rm = TRUE)
sd_cog_DST <- sd(metab_data$OX.DST,na.rm = TRUE)
mean_cog_FLU_C <- mean(metab_data$OX.FLU_C,na.rm = TRUE)
sd_cog_FLU_C <- sd(metab_data$OX.FLU_C,na.rm = TRUE)
mean_cog_FLU_L <- mean(metab_data$OX.FLU_L,na.rm = TRUE)
sd_cog_FLU_L <- sd(metab_data$OX.FLU_L,na.rm = TRUE)
mean_cog_DCOD <- mean(metab_data$OX.DCOD,na.rm = TRUE)
sd_cog_DCOD <- sd(metab_data$OX.DCOD,na.rm = TRUE)
mean_cog_tmtba <- mean(metab_data$tmtba,na.rm = TRUE)
sd_cog_tmtba <- sd(metab_data$tmtba,na.rm = TRUE)
# for dti data sample:
Dmean_cog_HVLT <- mean(metab_data_dti$OX.HVLT_TR,na.rm = TRUE)
Dsd_cog_HVLT <- sd(metab_data_dti$OX.HVLT_TR,na.rm = TRUE)
Dmean_cog_DST <- mean(metab_data_dti$OX.DST,na.rm = TRUE)
Dsd_cog_DST <- sd(metab_data_dti$OX.DST,na.rm = TRUE)
Dmean_cog_FLU_C <- mean(metab_data_dti$OX.FLU_C,na.rm = TRUE)
Dsd_cog_FLU_C <- sd(metab_data_dti$OX.FLU_C,na.rm = TRUE)
Dmean_cog_FLU_L <- mean(metab_data_dti$OX.FLU_L,na.rm = TRUE)
Dsd_cog_FLU_L <- sd(metab_data_dti$OX.FLU_L,na.rm = TRUE)
Dmean_cog_DCOD <- mean(metab_data_dti$OX.DCOD,na.rm = TRUE)
Dsd_cog_DCOD <- sd(metab_data_dti$OX.DCOD,na.rm = TRUE)
Dmean_cog_tmtba <- mean(metab_data_dti$tmtba,na.rm = TRUE)
Dsd_cog_tmtba <- sd(metab_data_dti$tmtba,na.rm = TRUE)
#table
cbind(mean_cog_HVLT,sd_cog_HVLT,mean_cog_DST,sd_cog_DST,mean_cog_FLU_C,sd_cog_FLU_C,mean_cog_FLU_L,sd_cog_FLU_L,mean_cog_DCOD,sd_cog_DCOD,mean_cog_tmtba,sd_cog_tmtba) 
cbind(Dmean_cog_HVLT,Dsd_cog_HVLT,Dmean_cog_DST,Dsd_cog_DST,Dmean_cog_FLU_C,Dsd_cog_FLU_C,Dmean_cog_FLU_L,Dsd_cog_FLU_L,Dmean_cog_DCOD,Dsd_cog_DCOD,Dmean_cog_tmtba,Dsd_cog_tmtba) 


# also age and N  ----
N <- cbind(max(ahei_data$X),round(sum(ahei_data$sex == 2,na.rm = TRUE)/max(ahei_data$X)*100,digits = 2))
Age <- cbind(round(mean(ahei_data$OX.AGE,na.rm = TRUE),digits=2),round(sd(ahei_data$OX.AGE,na.rm = TRUE),digits=2))
rbind(N,Age)
#for dti
N <- cbind(max(ahei_data_dti$X),round(sum(ahei_data_dti$sex == 2)/max(ahei_data_dti$X)*100,digits = 2))
Age <- cbind(round(mean(ahei_data_dti$OX.AGE,na.rm = TRUE),digits=2),round(sd(ahei_data_dti$OX.AGE,na.rm = TRUE),digits=2))
rbind(N,Age)
#whr
N <- cbind(max(metab_data$X),round(sum(metab_data$sex == 2)/max(metab_data$X)*100,digits = 2))
Age <- cbind(round(mean(metab_data$OX.AGE,na.rm = TRUE),digits=2),round(sd(metab_data$OX.AGE,na.rm = TRUE),digits=2))
rbind(N,Age)
#for dti
N <- cbind(max(metab_data_dti$X,na.rm = TRUE),round(sum(metab_data_dti$sex == 2,na.rm = TRUE)/max(metab_data_dti$X,na.rm = TRUE)*100,digits = 2))
Age <- cbind(round(mean(metab_data_dti$OX.AGE,na.rm = TRUE),digits=2),round(sd(metab_data_dti$OX.AGE,na.rm = TRUE),digits=2))
rbind(N,Age)


# smoking status at MRI timepoint ---- 
smoke <- ahei_data$OX.SMOK_00
smoke_counts <- table(smoke)# Counting the occurrences
print(smoke_counts)# Display the counts

smoke <- metab_data$OX.SMOK_00
smoke_counts <- table(smoke)# Counting the occurrences
print(smoke_counts)# Display the counts

# ethnicity values: ----

# Merge ethnicity data with ahei_data
ahei_data <- merge(ahei_data, 
                   ethnicity_data[, c("df.OX.MRI_ID.x", "df.ethn_ds.x")], 
                   by.x = "OX.MRI_ID", 
                   by.y = "df.OX.MRI_ID.x", 
                   all.x = TRUE)

# Rename the ethnicity column in ahei_data
colnames(ahei_data)[which(names(ahei_data) == "df.ethn_ds.x")] <- "ethn"

# Merge ethnicity data with metab_data
metab_data <- merge(metab_data, 
                    ethnicity_data[, c("df.OX.MRI_ID.x", "df.ethn_ds.x")], 
                    by.x = "OX.MRI_ID", 
                    by.y = "df.OX.MRI_ID.x", 
                    all.x = TRUE)

# Rename the ethnicity column in metab_data
colnames(metab_data)[which(names(metab_data) == "df.ethn_ds.x")] <- "ethn"

# Count occurrences of each ethnicity in ahei_data
ahei_ethn_counts <- table(ahei_data$ethn)
print("Ethnicity counts in ahei_data:")
print(ahei_ethn_counts)

# Count occurrences of each ethnicity in metab_data
metab_ethn_counts <- table(metab_data$ethn)
print("Ethnicity counts in metab_data:")
print(metab_ethn_counts)
# sex  ----
# Count occurrences of each ethnicity in ahei_data
ahei_ethn_counts <- table(ahei_data$sex)
print(ahei_ethn_counts)
# Count occurrences of each ethnicity in metab_data
metab_ethn_counts <- table(metab_data$sex)
print(metab_ethn_counts)

# per wave WHR and AHEI ----
# Calculate mean and SD for AHEI columns in ahei_datab
ahei_summary <- ahei_data %>%
  summarise(
    AHEI_P3_mean = round(mean(AHEI_P3, na.rm = TRUE), 2),
    AHEI_P3_sd = round(sd(AHEI_P3, na.rm = TRUE), 2),
    AHEI_P5_mean = round(mean(AHEI_P5, na.rm = TRUE), 2),
    AHEI_P5_sd = round(sd(AHEI_P5, na.rm = TRUE), 2),
    AHEI_P7_mean = round(mean(AHEI_P7, na.rm = TRUE), 2),
    AHEI_P7_sd = round(sd(AHEI_P7, na.rm = TRUE), 2)
  )

ahei_summary_by_sex <- ahei_data %>%
  group_by(sex) %>%
  summarise(
    AHEI_P3_mean = round(mean(AHEI_P3, na.rm = TRUE), 2),
    AHEI_P3_sd = round(sd(AHEI_P3, na.rm = TRUE), 2),
    AHEI_P5_mean = round(mean(AHEI_P5, na.rm = TRUE), 2),
    AHEI_P5_sd = round(sd(AHEI_P5, na.rm = TRUE), 2),
    AHEI_P7_mean = round(mean(AHEI_P7, na.rm = TRUE), 2),
    AHEI_P7_sd = round(sd(AHEI_P7, na.rm = TRUE), 2)
  )

print("AHEI Summary for all participants:")
print(ahei_summary)

print("AHEI Summary by sex:")
print(ahei_summary_by_sex)

# Calculate mean and SD for WHR columns in metab_data, rounded to 2 digits
metab_summary <- metab_data %>%
  summarise(
    WHR_P3_mean = round(mean(WHR_P3, na.rm = TRUE), 2),
    WHR_P3_sd = round(sd(WHR_P3, na.rm = TRUE), 2),
    WHR_P5_mean = round(mean(WHR_P5, na.rm = TRUE), 2),
    WHR_P5_sd = round(sd(WHR_P5, na.rm = TRUE), 2),
    WHR_P7_mean = round(mean(WHR_P7, na.rm = TRUE), 2),
    WHR_P7_sd = round(sd(WHR_P7, na.rm = TRUE), 2),
    WHR_P9_mean = round(mean(WHR_P9, na.rm = TRUE), 2),
    WHR_P9_sd = round(sd(WHR_P9, na.rm = TRUE), 2),
    WHR_P11_mean = round(mean(WHR_P11, na.rm = TRUE), 2),
    WHR_P11_sd = round(sd(WHR_P11, na.rm = TRUE), 2)
  )

metab_summary_by_sex <- metab_data %>%
  group_by(sex) %>%
  summarise(
    WHR_P3_mean = round(mean(WHR_P3, na.rm = TRUE), 2),
    WHR_P3_sd = round(sd(WHR_P3, na.rm = TRUE), 2),
    WHR_P5_mean = round(mean(WHR_P5, na.rm = TRUE), 2),
    WHR_P5_sd = round(sd(WHR_P5, na.rm = TRUE), 2),
    WHR_P7_mean = round(mean(WHR_P7, na.rm = TRUE), 2),
    WHR_P7_sd = round(sd(WHR_P7, na.rm = TRUE), 2),
    WHR_P9_mean = round(mean(WHR_P9, na.rm = TRUE), 2),
    WHR_P9_sd = round(sd(WHR_P9, na.rm = TRUE), 2),
    WHR_P11_mean = round(mean(WHR_P11, na.rm = TRUE), 2),
    WHR_P11_sd = round(sd(WHR_P11, na.rm = TRUE), 2)
  )

print("WHR Summary for all participants:")
print(metab_summary)

print("WHR Summary by sex:")
print(metab_summary_by_sex)



# ------- Control plots also included in the revision: ----

# 1. Correlation betweenn AHEI and WHR ----
# Define the mapping from time points to wave names
wave_names <- c("P3" = "Wave 3", "P5" = "Wave 5", "P7" = "Wave 7")
# Function to format p-value
format_p_value <- function(p_value) {
  if (p_value < 0.001) {
    return("<0.001")
  } else {
    return(sprintf("%.2f", p_value))
  }
}
# Function to create a scatterplot with correlation statistics
scatter_plot <- function(data, x_var, y_var, time_point) {
  # Calculate correlation
  correlation_result <- cor.test(data[[x_var]], data[[y_var]], use = "complete.obs")
  
  # Get the wave name
  wave_name <- wave_names[time_point]
  
  # Create plot
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = sprintf("Scatterplot for %s: r = %.2f, p = %s", 
                         wave_name, 
                         correlation_result$estimate, 
                         format_p_value(correlation_result$p.value)),
         x = paste("AHEI", wave_name),
         y = paste("WHR", wave_name)) +
    theme_minimal() +
    theme(text = element_text(size = 14))
}
# Create scatterplots for each time point
plots <- list()
time_points <- c("P3", "P5", "P7")
for (time_point in time_points) {
  ahei_var <- paste("AHEI", time_point, sep = "_")
  metab_var <- paste("WHR", time_point, sep = "_")
  merged_data <- merge(ahei_data[c("OX.MRI_ID", ahei_var)], metab_data[c("OX.MRI_ID", metab_var)], by = "OX.MRI_ID")
  plots[[time_point]] <- scatter_plot(merged_data, ahei_var, metab_var, time_point)
}
# Arrange the plots in a grid
tmp = sprintf("WHRAHEIAtWavescorr_mergedrsfmriN_n509.pdf")
pdf(file = tmp, width = 15, height = 5)
grid.arrange(grobs = plots, ncol = 3)
dev.off()



# 2. Correlation between Intercept and Slope for AHEI and WHR ----
# Merge the data frames on a common identifier, such as OX.MRI_ID
merged_data <- merge(ahei_data, metab_data, by = "OX.MRI_ID")

# Check if the merged data contains the necessary columns
if (!all(c("interceptAHEI", "intercept", "slopeAHEI", "t") %in% colnames(merged_data))) {
  stop("Required columns are missing in the merged data.")
}

# Correlate intercepts
intercept_correlation <- cor.test(merged_data$interceptAHEI, merged_data$intercept, use = "complete.obs")
# Correlate slopes
slope_correlation <- cor.test(merged_data$slopeAHEI, merged_data$t, use = "complete.obs")

# Plot intercept correlation
p1 <- ggplot(data.frame(interceptAHEI = merged_data$interceptAHEI, intercept = merged_data$intercept), 
       aes(x = interceptAHEI, y = intercept)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = sprintf("Intercept's: r = %.2f, p = %.2f", intercept_correlation$estimate, intercept_correlation$p.value),
       x = "Intercept AHEI",
       y = "Intercept WHR")
# Plot slope correlation
p2 <- ggplot(data.frame(slopeAHEI = merged_data$slopeAHEI, t = merged_data$t), 
       aes(x = slopeAHEI, y = t)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = sprintf("Slope's: r = %.2f, p = %.2f", slope_correlation$estimate, slope_correlation$p.value),
       x = "Slope AHEI",
       y = "Slope WHR")

# Arrange the plots in a grid
tmp = sprintf(paste("WHRAHEIcorr_mergedrsfmriN_n509.pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
p <- grid.arrange(p1, p2, ncol = 2)
# Plot and save to PDF
grid.draw(p)
dev.off()



# 3. Correlation between WHR_P11/ slope and intercept and BMI  ----

# Plotting WHR_P11 and BMI correlation
whr_bmi_correlation <- cor.test(metab_data$WHR_P11, metab_data$OX.BMI, use = "complete.obs")
p<- ggplot(data.frame(metab_data$WHR_P11, metab_data$OX.BMI), aes(x = metab_data.WHR_P11, y = metab_data.OX.BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = sprintf("WHR (Wave 11) and BMI: r = %.2f, p = %.2f", whr_bmi_correlation$estimate, whr_bmi_correlation$p.value),
       x = "WHR (Wave 11)",
       y = "BMI (at MRI)")
tmp = sprintf("WHRBMI_Wave11_corr_n665.pdf")
pdf(file = tmp, width = 10, height = 5)
grid.arrange(p, ncol = 2)
dev.off()


# Correlation between intercept and BMI
# Function to format p-value
format_p_value <- function(p_value) {
  if (p_value < 0.001) {
    return("<0.001")
  } else {
    return(sprintf("%.3f", p_value))
  }
}
# Correlation between intercept and BMI
intercept_bmi_correlation <- cor.test(metab_data$intercept, metab_data$OX.BMI, use = "complete.obs")
# Plot for intercept and BMI
p1 <- ggplot(data.frame(intercept = metab_data$intercept, BMI = metab_data$OX.BMI), aes(x = intercept, y = BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = sprintf("Intercept WHR and BMI: r = %.2f, p = %s", 
                       intercept_bmi_correlation$estimate, format_p_value(intercept_bmi_correlation$p.value)),
       x = "Intercept",
       y = "BMI")
# Correlation between slope (t) and BMI
slope_bmi_correlation <- cor.test(metab_data$t, metab_data$OX.BMI, use = "complete.obs")
# Plot for slope (t) and BMI
p2 <- ggplot(data.frame(slope = metab_data$t, BMI = metab_data$OX.BMI), aes(x = slope, y = BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = sprintf("Slope (t) and BMI: r = %.2f, p = %s", 
                       slope_bmi_correlation$estimate, format_p_value(slope_bmi_correlation$p.value)),
       x = "Slope (t)",
       y = "BMI")
# Arrange the plots in a grid
tmp = sprintf("WHRBMIcorr_n665.pdf")
pdf(file = tmp, width = 10, height = 5)
grid.arrange(p1, p2, ncol = 2)
dev.off()













# -------- COGNITIVE OUTCOMES  --------
# see Supplementary Table 1.7
# estimate associations between WHR/diet and cognitive performance measures.

#check mean MoCA:
mean(ahei_data$OX.MOCA_TS)
sd(ahei_data$OX.MOCA_TS)
mean(metab_data$OX.MOCA_TS)
sd(metab_data$OX.MOCA_TS)

d <- (metab_data$OX.MOCA_TS)
d <- d[!is.na(d)]
sum(d<21)
sum((ahei_data$OX.MOCA_TS)<21,na.rm=FALSE)
min(metab_data$OX.MOCA_TS)
max(metab_data$OX.MOCA_TS)

#for tabel 1: woman and men AHEI and WHR
sex_mean <- round(mean(ahei_data$AHEI_P3, na.rm = TRUE),digits = 2)
sex_sd <- round(sd(ahei_data$AHEI_P3, na.rm = TRUE),digits = 2)
cbind(sex_mean,sex_sd)

#ahei
sex_mean <- round(aggregate(ahei_data$AHEI_P3, list(ahei_data$sex), FUN=mean, na.rm = TRUE),digits = 2)
sex_sd <- round(aggregate(ahei_data$AHEI_P3, list(ahei_data$sex), FUN=sd, na.rm = TRUE),digits = 2)
P3 <- cbind(sex_mean,sex_sd)
sex_mean <- round(aggregate(ahei_data$AHEI_P5, list(ahei_data$sex), FUN=mean, na.rm = TRUE),digits = 2)
sex_sd <- round(aggregate(ahei_data$AHEI_P5, list(ahei_data$sex), FUN=sd, na.rm = TRUE),digits = 2)
P5 <- cbind(sex_mean,sex_sd)
sex_mean <- round(aggregate(ahei_data$AHEI_P7, list(ahei_data$sex), FUN=mean, na.rm = TRUE),digits = 2)
sex_sd <- round(aggregate(ahei_data$AHEI_P7, list(ahei_data$sex), FUN=sd, na.rm = TRUE),digits = 2)
P7 <- cbind(sex_mean,sex_sd)
#table
cbind(P3,P5,P7)
#whr
sex_mean <- round(aggregate(metab_data$WHR_P3, list(metab_data$sex), FUN=mean, na.rm = TRUE),digits = 2)
sex_sd <- round(aggregate(metab_data$WHR_P3, list(metab_data$sex), FUN=sd, na.rm = TRUE),digits = 2)
P3 <- cbind(sex_mean,sex_sd)
sex_mean <- round(aggregate(metab_data$WHR_P5, list(metab_data$sex), FUN=mean, na.rm = TRUE),digits = 2)
sex_sd <- round(aggregate(metab_data$WHR_P5, list(metab_data$sex), FUN=sd, na.rm = TRUE),digits = 2)
P5 <- cbind(sex_mean,sex_sd)
sex_mean <- round(aggregate(metab_data$WHR_P7, list(metab_data$sex), FUN=mean, na.rm = TRUE),digits = 2)
sex_sd <- round(aggregate(metab_data$WHR_P7, list(metab_data$sex), FUN=sd, na.rm = TRUE),digits = 2)
P7 <- cbind(sex_mean,sex_sd)
sex_mean <- round(aggregate(metab_data$WHR_P9, list(metab_data$sex), FUN=mean, na.rm = TRUE),digits = 2)
sex_sd <- round(aggregate(metab_data$WHR_P9, list(metab_data$sex), FUN=sd, na.rm = TRUE),digits = 2)
P9 <- cbind(sex_mean,sex_sd)
sex_mean <- round(aggregate(metab_data$WHR_P11, list(metab_data$sex), FUN=mean, na.rm = TRUE),digits = 2)
sex_sd <- round(aggregate(metab_data$WHR_P11, list(metab_data$sex), FUN=sd, na.rm = TRUE),digits = 2)
P11 <- cbind(sex_mean,sex_sd)
#table
cbind(P3,P5,P7,P9,P11)


# ----- 1. WHR intercept - cognition -----
#  WHR intercept and slope from cubic spline LME:

x <- metab_data_dti$intercept -------
  
x <- metab_data_dti$t -------

# -----------------------------------------------------#

# 10 variables to consider: 

#Verbal episodic memory (OX.HVLT_TR) 
lm1 <-lm(x ~ OX.HVLT_TR+sex+OX.AGE+OX.CHAM_MA+OX.EDUC_MOCA,data=metab_data_dti)
#Working memory (digit span total) (OX.DST)
lm2 <-lm(x ~OX.DST+sex+OX.AGE+OX.CHAM_MA+OX.EDUC_MOCA,data=metab_data_dti)
#Semantic fluency (OX.FLU_C)
lm6 <-lm(x ~OX.FLU_C+sex+OX.AGE+OX.CHAM_MA+OX.EDUC_MOCA,data=metab_data_dti)
#Lexical fluency (OX.FLU_L)
lm7 <-lm(x ~OX.FLU_L+sex+OX.AGE+OX.CHAM_MA+OX.EDUC_MOCA,data=metab_data_dti)
#excecutive (digit coding/substitution) (OX.DCOD)
lm8 <-lm(x ~OX.DCOD+sex+OX.AGE+OX.CHAM_MA+OX.EDUC_MOCA,data=metab_data_dti)
#Executive function (trail making) (TMT B-A)
lm9 <-lm(x ~tmtba+sex+OX.AGE+OX.CHAM_MA+OX.EDUC_MOCA,data=metab_data_dti)


# check specific values with:
#summary(lm1)
#confint(lm1)
# extract Beta, CI and p-value from all regression models:sum()
tab_model(lm1,lm2,lm6,lm7,lm8,lm9,
          auto.label = FALSE,show.est=TRUE,show.stat=FALSE,show.intercept =FALSE,collapse.se = TRUE,
          digits = 3,
          dv.labels = c("HVLT_TR","DST","FLU_C","FLU_L","DCOD","TMTBA"),
          string.est = "Beta",string.p = "p-value",
          p.style = "numeric", digits.p = 3,
          title = "WHR intercept", #title = "WHR slope",
          rm.terms = c("sex", "OX.AGE","OX.CHAM_MA","OX.EDUC_MOCA"))

# ----------------------------------------------------- #
# Extract p-values from the summary of each linear model
p_values <- c(
  summary(lm1)$coefficients["OX.HVLT_TR", 4],
  summary(lm2)$coefficients["OX.DST", 4],
  summary(lm6)$coefficients["OX.FLU_C", 4],
  summary(lm7)$coefficients["OX.FLU_L", 4],
  summary(lm8)$coefficients["OX.DCOD", 4],
  summary(lm9)$coefficients["tmtba", 4]
)
# Adjust the p-values using Benjamini-Hochberg method
p_adjusted <- p.adjust(p_values, method = "BH")
# Output the adjusted p-values
names(p_adjusted) <- c("Verbal episodic memory (OX.HVLT_TR)", 
                       "Working memory (digit span total) (OX.DST)", 
                       "Semantic fluency (OX.FLU_C)", 
                       "Lexical fluency (OX.FLU_L)", 
                       "Executive (digit coding/substitution) (OX.DCOD)", 
                       "Executive function (trail making) (TMT B-A)")
# Print the adjusted p-values
#print(p_adjusted)
# Manually extract estimates, standard errors, original p-values, confidence intervals, and use the adjusted p-values, then round to 4 digits
results <- data.frame(
  Variable = c("HVLT_TR", "DST", "FLU_C", "FLU_L", "DCOD", "TMTBA"),
  Estimate = round(c(coef(summary(lm1))["OX.HVLT_TR", "Estimate"],
                     coef(summary(lm2))["OX.DST", "Estimate"],
                     coef(summary(lm6))["OX.FLU_C", "Estimate"],
                     coef(summary(lm7))["OX.FLU_L", "Estimate"],
                     coef(summary(lm8))["OX.DCOD", "Estimate"],
                     coef(summary(lm9))["tmtba", "Estimate"]), 4),
  SE = round(c(coef(summary(lm1))["OX.HVLT_TR", "Std. Error"],
               coef(summary(lm2))["OX.DST", "Std. Error"],
               coef(summary(lm6))["OX.FLU_C", "Std. Error"],
               coef(summary(lm7))["OX.FLU_L", "Std. Error"],
               coef(summary(lm8))["OX.DCOD", "Std. Error"],
               coef(summary(lm9))["tmtba", "Std. Error"]), 4),
  `Lower CI` = round(c(confint(lm1)["OX.HVLT_TR", 1],
                       confint(lm2)["OX.DST", 1],
                       confint(lm6)["OX.FLU_C", 1],
                       confint(lm7)["OX.FLU_L", 1],
                       confint(lm8)["OX.DCOD", 1],
                       confint(lm9)["tmtba", 1]), 5),
  `Upper CI` = round(c(confint(lm1)["OX.HVLT_TR", 2],
                       confint(lm2)["OX.DST", 2],
                       confint(lm6)["OX.FLU_C", 2],
                       confint(lm7)["OX.FLU_L", 2],
                       confint(lm8)["OX.DCOD", 2],
                       confint(lm9)["tmtba", 2]), 4),
  `Original P-Value` = round(c(summary(lm1)$coefficients["OX.HVLT_TR", "Pr(>|t|)"],
                               summary(lm2)$coefficients["OX.DST", "Pr(>|t|)"],
                               summary(lm6)$coefficients["OX.FLU_C", "Pr(>|t|)"],
                               summary(lm7)$coefficients["OX.FLU_L", "Pr(>|t|)"],
                               summary(lm8)$coefficients["OX.DCOD", "Pr(>|t|)"],
                               summary(lm9)$coefficients["tmtba", "Pr(>|t|)"]), 4),
  `FDR P-Value` = round(p_adjusted, 4)
)
# Print the results table
print(results, row.names = FALSE)


# ----------------------------------------------------- #
# ----- correlate cog values:------

# Selecting the columns you want to correlate
cog_columns <- metab_data_dti[, c("OX.HVLT_TR", "OX.DST", "OX.FLU_C", "OX.FLU_L", "OX.DCOD", "tmtba")]
# Calculating the correlation matrix
correlation_matrix <- cor(cog_columns, use = "complete.obs")  # 'use' argument handles missing values
print(correlation_matrix)# Display the correlation matrix
variables <- c("OX.HVLT_TR", "OX.DST", "OX.FLU_C", "OX.FLU_L", "OX.DCOD", "tmtba")
# Initialize a matrix to store p-values
p_value_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables), dimnames = list(variables, variables))
# Loop over all pairs of variables to fill the matrix
for(i in 1:(length(variables)-1)) {
  for(j in (i+1):length(variables)) {
    # Perform correlation test
    test_result <- cor.test(metab_data_dti[[variables[i]]], metab_data_dti[[variables[j]]], method = "pearson")
    
    # Store p-value in the matrix, rounded to three decimal places
    p_value_matrix[i, j] <- round(test_result$p.value, 5)
    p_value_matrix[j, i] <- round(test_result$p.value, 5)  # Mirror the matrix since it's symmetric
  }
}
# Diagonal elements are self-correlations, so we set them to NA or 1 as they are not applicable
diag(p_value_matrix) <- NA
# Print the p-value matrix
print(p_value_matrix)


# ----------------------------------------------------- #
# ----- 2. slope of AHEI associated with cognition: -------
x <- ahei_data_dti$slopeAHEI #

# ----- 3. intercept of AHEI associated with cognition: -------
x <- ahei_data_dti$interceptAHEI # n.s. but included still in table ED 9 / 1.9

# 10 variables to consider: -------
#Verbal episodic memory (OX.HVLT_TR) 
lm1 <-lm(x ~ OX.HVLT_TR+sex+OX.AGE+OX.CHAM_MA+OX.BMI+OX.EDUC_MOCA+mean.energy.intake,data=ahei_data_dti)
#Working memory (digit span total) (OX.DST)
lm2 <-lm(x ~OX.DST+sex+OX.AGE+OX.CHAM_MA+OX.BMI+OX.EDUC_MOCA+mean.energy.intake,data=ahei_data_dti)
#Semantic fluency (OX.FLU_C)
lm6 <-lm(x ~OX.FLU_C+sex+OX.AGE+OX.CHAM_MA+OX.BMI+OX.EDUC_MOCA+mean.energy.intake,data=ahei_data_dti)
#Lexical fluency (OX.FLU_L)
lm7 <-lm(x ~OX.FLU_L+sex+OX.AGE+OX.CHAM_MA+OX.BMI+OX.EDUC_MOCA+mean.energy.intake,data=ahei_data_dti)
#excecutive (digit coding/substitution) (OX.DCOD)
lm8 <-lm(x ~OX.DCOD+sex+OX.AGE+OX.CHAM_MA+OX.BMI+OX.EDUC_MOCA+mean.energy.intake,data=ahei_data_dti)
#Executive function (trail making) (TMT B-A)
lm9 <-lm(x ~tmtba+sex+OX.AGE+OX.CHAM_MA+OX.BMI+OX.EDUC_MOCA+mean.energy.intake,data=ahei_data_dti)

# check specific values with:
#summary(lm1)
#confint(lm1)

# extract Beta, CI and p-value from all regression models:
tab_model(lm1,lm2,lm6,lm7,lm8,lm9,
          auto.label = FALSE,show.est=TRUE,show.stat=FALSE,show.intercept =FALSE,collapse.se = TRUE,
          digits = 4,
          dv.labels = c("HVLT_TR","DST","FLU_C","FLU_L","DCOD","TMTBA"),
          string.est = "Beta",string.p = "p-value",
          p.style = "numeric", digits.p = 3,
          rm.terms = c("sex", "OX.AGE","OX.CHAM_MA","OX.BMI","OX.EDUC_MOCA","mean.energy.intake"))

# Extract p-values from the summary of each linear model
p_values <- c(
  summary(lm1)$coefficients["OX.HVLT_TR", 4],
  summary(lm2)$coefficients["OX.DST", 4],
  summary(lm6)$coefficients["OX.FLU_C", 4],
  summary(lm7)$coefficients["OX.FLU_L", 4],
  summary(lm8)$coefficients["OX.DCOD", 4],
  summary(lm9)$coefficients["tmtba", 4]
)
# Adjust the p-values using Benjamini-Hochberg method
p_adjusted <- p.adjust(p_values, method = "BH")
# Output the adjusted p-values
names(p_adjusted) <- c("Verbal episodic memory (OX.HVLT_TR)", 
                       "Working memory (digit span total) (OX.DST)", 
                       "Semantic fluency (OX.FLU_C)", 
                       "Lexical fluency (OX.FLU_L)", 
                       "Executive (digit coding/substitution) (OX.DCOD)", 
                       "Executive function (trail making) (TMT B-A)")
# Print the adjusted p-values
#print(p_adjusted)

# Manually extract estimates, standard errors, original p-values, confidence intervals, and use the adjusted p-values, then round to 4 digits
results <- data.frame(
  Variable = c("HVLT_TR", "DST", "FLU_C", "FLU_L", "DCOD", "TMTBA"),
  Estimate = round(c(coef(summary(lm1))["OX.HVLT_TR", "Estimate"],
                     coef(summary(lm2))["OX.DST", "Estimate"],
                     coef(summary(lm6))["OX.FLU_C", "Estimate"],
                     coef(summary(lm7))["OX.FLU_L", "Estimate"],
                     coef(summary(lm8))["OX.DCOD", "Estimate"],
                     coef(summary(lm9))["tmtba", "Estimate"]), 4),
  SE = round(c(coef(summary(lm1))["OX.HVLT_TR", "Std. Error"],
               coef(summary(lm2))["OX.DST", "Std. Error"],
               coef(summary(lm6))["OX.FLU_C", "Std. Error"],
               coef(summary(lm7))["OX.FLU_L", "Std. Error"],
               coef(summary(lm8))["OX.DCOD", "Std. Error"],
               coef(summary(lm9))["tmtba", "Std. Error"]), 4),
  `Lower CI` = round(c(confint(lm1)["OX.HVLT_TR", 1],
                       confint(lm2)["OX.DST", 1],
                       confint(lm6)["OX.FLU_C", 1],
                       confint(lm7)["OX.FLU_L", 1],
                       confint(lm8)["OX.DCOD", 1],
                       confint(lm9)["tmtba", 1]), 4),
  `Upper CI` = round(c(confint(lm1)["OX.HVLT_TR", 2],
                       confint(lm2)["OX.DST", 2],
                       confint(lm6)["OX.FLU_C", 2],
                       confint(lm7)["OX.FLU_L", 2],
                       confint(lm8)["OX.DCOD", 2],
                       confint(lm9)["tmtba", 2]), 4),
  `Original P-Value` = round(c(summary(lm1)$coefficients["OX.HVLT_TR", "Pr(>|t|)"],
                               summary(lm2)$coefficients["OX.DST", "Pr(>|t|)"],
                               summary(lm6)$coefficients["OX.FLU_C", "Pr(>|t|)"],
                               summary(lm7)$coefficients["OX.FLU_L", "Pr(>|t|)"],
                               summary(lm8)$coefficients["OX.DCOD", "Pr(>|t|)"],
                               summary(lm9)$coefficients["tmtba", "Pr(>|t|)"]), 4),
  `FDR P-Value` = round(p_adjusted, 4)
)
# Print the results table
print(results, row.names = FALSE)


cog_columns <- ahei_data_dti[, c("OX.HVLT_TR", "OX.DST", "OX.FLU_C", "OX.FLU_L", "OX.DCOD", "tmtba")]
# Calculating the correlation matrix
correlation_matrix <- cor(cog_columns, use = "complete.obs")  # 'use' argument handles missing values
print(correlation_matrix)# Display the correlation matrix
variables <- c("OX.HVLT_TR", "OX.DST", "OX.FLU_C", "OX.FLU_L", "OX.DCOD", "tmtba")
# Initialize a matrix to store p-values
p_value_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables), dimnames = list(variables, variables))
# Loop over all pairs of variables to fill the matrix
for(i in 1:(length(variables)-1)) {
  for(j in (i+1):length(variables)) {
    # Perform correlation test
    test_result <- cor.test(ahei_data_dti[[variables[i]]], ahei_data_dti[[variables[j]]], method = "pearson")
    
    # Store p-value in the matrix, rounded to three decimal places
    p_value_matrix[i, j] <- round(test_result$p.value, 5)
    p_value_matrix[j, i] <- round(test_result$p.value, 5)  # Mirror the matrix since it's symmetric
  }
}
# Diagonal elements are self-correlations, so we set them to NA or 1 as they are not applicable
diag(p_value_matrix) <- NA
# Print the p-value matrix
print(p_value_matrix)


# ----- FIGURE 5A: SCATTERPLOT -----
# Plot associations which are significant also after correction for multiple comparisons.
# predicted cognitive performance plot

#install.packages("devtools")
#devtools::install_github("cardiomoon/ggiraphExtra")
library(devtools)
library(moonBook)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)

# Function to create a plot with specified x-axis range, y-axis ticks, and styling
create_plot <- function(data, y_label) {
  ggplot(data, aes(x = intercept, y = predicted_hvlt)) +
    geom_smooth(method = "lm", color = "darkred", fill = "grey", alpha = 0.5) + # CI line grey-ish red and half transparent
    theme_minimal() +
    geom_point(color = "darkgrey") + # Points dark grey
    stat_smooth(method = "lm", fill = NA, linetype = 4, geom = "ribbon") +
    scale_color_brewer(palette = "Set2") +
    labs(y = y_label, x = "WHR Intercept") +
    scale_x_continuous(limits = c(0.5,1.2)) #+ # Set x-axis range
}

# Predicted cognitive performance plots
m8 <- lm(OX.HVLT_TR ~ intercept + sex + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA, data = metab_data_dti)
m8_model_df <- m8$model
m8_model_df$predicted_hvlt <- predict(m8)
p1 <- create_plot(m8_model_df, "Predicted episodic memory")
p1 + scale_y_continuous(limits = c(22.5, 32.5))

m8 <- lm(OX.DST ~ intercept + sex + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA, data = metab_data_dti)
m8_model_df <- m8$model
m8_model_df$predicted_hvlt <- predict(m8)
p2 <- create_plot(m8_model_df, "Predicted digit span")

m8 <- lm(OX.FLU_C ~ intercept + sex + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA, data = metab_data_dti)
m8_model_df <- m8$model
m8_model_df$predicted_hvlt <- predict(m8)
p3 <- create_plot(m8_model_df, "Predicted semantic fluency")
p3 + scale_y_continuous(limits = c(15, 27.5))

m8 <- lm(OX.DCOD ~ intercept + sex + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA, data = metab_data_dti)
m8_model_df <- m8$model
m8_model_df$predicted_hvlt <- predict(m8)
p4 <- create_plot(m8_model_df, "Predicted digit coding")
p4 + scale_y_continuous(limits = c(45, 75))

m8 <- lm(tmtba ~ intercept + sex + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA, data = metab_data_dti)
m8_model_df <- m8$model
m8_model_df$predicted_hvlt <- predict(m8)
p5 <- create_plot(m8_model_df, "Predicted trail making test")

# Combine plots
p <- grid.arrange(p1, p2, p3, p4, p5, nrow = 1)

# Save all plots as SVG files
ggsave("WHRintercept_HVLT_R1.svg", plot = p1, width = 2, height = 2)
ggsave("WHRintercept_DST_R1.svg", plot = p2, width = 2, height = 2)
ggsave("WHRintercept_FLU_C_R1.svg", plot = p3, width = 2, height = 2)
ggsave("WHRintercept_DCOD_R1.svg", plot = p4, width = 2, height = 2)
ggsave("WHRintercept_TMTBA.svg", plot = p5, width = 2, height = 2)

