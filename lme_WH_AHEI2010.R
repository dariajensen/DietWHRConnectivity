#lme  with random interncept and slopes on AHEI-2010 data
#We used the NLME package; as this is the only package that allows to take into account correlation between timepoints.
#The correlation=CORCAR1 allows for this (continous autoregressive process for a continuous time covariate)


# load packages, note that you need to use install.packages("name") first ---------
library(nlme)
library(lme4)
library(dplyr)
library(stargazer)
library(pracma)
library(readr)
library(data.table)
library(car)
library(sjPlot)
library(haven)
library(ggplot2)
library(sjmisc)
library(devtools)
library(ggeffects)
library(splines)
library(lmerTest)
library(RColorBrewer)
library(gridExtra)
library(grid)
#for the plots: install R package ggiraphExtra from github.
#install.packages("devtools")
#devtools::install_github("cardiomoon/ggiraphExtra")

# load data --------------------------------------------------------------------
setwd("XX/scripts/") # set path
WH_long <- read.csv("XX.csv") 
output=("AHEI_XX")

# NaNs and controls --------------------------------------------------------------------
# Any NANs?
any(is.na(WH_long)) # FALSE

# note: think about all the variables you need here and to control for and add those in the model too (e.g., already correcting for age). 
# long table includudes:
# y = value , fixed (AHEI-2010 index value)
# x = time t, random (age related to baseline in phase 1 in 5 phases (3,5,7,9,11))
# subjects = oxf_id

# factors --------------------------------------------------------------------
# for LME the phase, oxf_id and metabolic factors need to be factors; 
WH_long$phase <- as.factor(WH_long$phase)
WH_long$oxf_id <- as.factor(WH_long$oxf_id)
#WH_long$factor <- as.factor(WH_long$factor) #none.
levels(WH_long$phase) # check that the levels are correct.

# mean centering (revision request)--------------------------------------------------------------------
# to avoid colinearity issues; you can scale the variables. However, please note that the intercept looks at the moment when the value = 0. Hence, if you are interested in time at baseline, scaling is not handy.
# Scaling variables to test for revision
WH_long$t_scale <- scale(WH_long$t) # t - timepoint 0 taken as reference point.


# ---- individual lines: --------------------------------

# plot t - ahei
#plot(WH_long$t,WH_long$ahei) 

p_bw <- ggplot(WH_long,aes(t,ahei))+
  theme_set(theme_bw()) +theme_minimal() + 
  theme(text = element_text(size = 18)) +
  geom_point(alpha = 0.5)+
  geom_line(aes(group=oxf_id),alpha=0.1,size=0.5)+ 
  theme(legend.position = "none")  # adding space between panels


# add spagetti plots with fitted line on top
WH_long$oxf_id <- as.numeric(as.character(WH_long$oxf_id))
spagetti_p <- ggplot(data = WH_long, aes(x = t, y = ahei, group = oxf_id)) + 
  geom_line(aes(color = oxf_id), alpha = 0.3, size = 1.5) + 
  geom_point(data = WH_long, aes(x = t, y = ahei), alpha = 0.3, size = 1) + # raw data points
  scale_colour_identity() + 
  ylab('AHEI') + 
  xlab("Years from baseline") +
  scale_x_continuous(breaks = seq(0, 20, 5)) +
  scale_y_continuous(breaks = seq(0, 110, 10), limits = c(0, 110)) +  # Set y-axis limits
  theme_bw() +
  theme(
    legend.title = element_blank(), 
    legend.position = "none", 
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 18)  # Larger text size
  ) 
## SAVE SPAGETTI PLOT
# Open a new PDF device
tmp = sprintf(paste("Spagettiplot_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 5, height = 5)
# Create your plot
plot(spagetti_p)
# Close the PDF device
dev.off()



# ---- 1. run models --------------------------------------------------------------------

# run a linear and quadratic mixed effects model
# t - x random; value - y fixed
# autoregressive residuals, correlation between timepoints: CORCAR1(form=~t|oxf_id)
# continuous autoregressive process (AR(1) process for a continuous time covariate).

# random intercept and slope model
fit_lin<- lme(ahei~t, random=~t|oxf_id, data=WH_long, correlation=corCAR1(form=~t|oxf_id/phase), method="ML",na.action=na.exclude, control=list(opt="optim")) 

# spline
fit_cub_spline_df2<- lme(ahei~ns(t,2), random=~t|oxf_id, data=WH_long, correlation=corCAR1(form=~t|oxf_id/phase), method="ML",na.action=na.exclude, control=list(opt="optim")) 
fit_cub_spline_df3<- lme(ahei~ns(t,3), random=~t|oxf_id, data=WH_long, correlation=corCAR1(form=~t|oxf_id/phase), method="ML",na.action=na.exclude, control=list(opt="optim"))

# ---- 2. model comparison --------------------------------------------------------------------  
# Does model represent an improvement over null?
print(anova(fit_lin,fit_cub_spline_df2))
#print(anova(fit_cub_spline_df2, fit_cub_spline_df3)) 

# get the winner of anova for each factor:
fit <- fit_lin # choose winner model


# ---- 3. check model fit --------------------------------------------------------------------
tab_model(fit, p.style = "stars",digits = 2) # report in a table:

#tab_model(fit, fit_cub_spline_df2, p.style = "stars",digits = 2) # report in a table:
#fit_ahei <- fit_lin

# ---- 4. plots and summarise results --------------------------------------------------------------------
# Q-Q plot and diagnostic plots
tmp=sprintf(paste("Q-Q_",output,".pdf",sep ="", collapse=NULL))
pdf(file = tmp, width = 5, height = 5)
# Q-Q plot
qqnorm(resid(fit))
qqline(resid(fit))
# normal plot of standardized residuals
qqnorm(fit, ~ resid(., type = "p") , abline = c(0, 1))
# normal plots of random effects
qqnorm(fit, ~ranef(.))
dev.off()

predplot1 <- ggpredict(fit, terms = c("t [all] "))
plot2 <-plot(predplot1, facet = TRUE, add.data = TRUE, ci = FALSE) + 
  theme_set(theme_bw()) +theme_minimal() +
  theme(text = element_text(size = 18)) +
  labs(x = "t", y = "AHEI-2010 score", title = "AHEI-2010 score across Waves")
plot1 <-plot(predplot1, facet = TRUE, (cex = 3))+
  theme_set(theme_bw()) +theme_minimal() + 
  theme(text = element_text(size = 18)) +
  labs(x = "t", y = "AHEI-2010 score", title = "AHEI-2010 score across Waves") 
plot3 <-plot(fit, ahei~ fitted(.) , abline = c(0,1), cex=0.5)
plot4 <-plot(fit, ahei~ resid(.))
tmp=sprintf(paste("Pred_",output,".pdf",sep ="", collapse=NULL))
pdf(file = tmp, width = 5, height = 5)
plot(plot1)
plot(plot2)
plot(plot3)
plot(plot4)
dev.off()

ggplot(predplot1) + 
  theme_set(theme_bw()) + theme_minimal() + # minimal setting
  theme(legend.position = "none",panel.spacing = unit(1, "lines")) + #no legend
  geom_point(data=WH_long,aes(x=t,y=ahei),alpha=0.3,size=1) + #raw data points
  geom_line(aes(x = x, y = predicted)) +     # slope
  labs(x = "t", y = "AHEI", title = "AHEI across phases")

# Save the plot as an SVG file
tmp <- sprintf("Pred_%s.svg", output)
ggsave(filename = tmp, plot = plot1, width = 4, height = 4)

rm(plot1,plot2,predplot1,tmp)


  
# ---- 5. save and use the best model ------------------------------------------------------------------------
# extracting slopes and intercepts for the LME
fit<-coef(fit)
#fit<-ranef(fit)
tmp=sprintf(paste("WH_wide_",output,".csv",sep ="", collapse=NULL))
write.csv(fit,tmp)

# Create file with slopes and ids (a similar approach needed if the quadratic model is more suitable)
slope <- read_csv(tmp)
slope <- rename(slope, "oxf_id" = "X1") #please check whether IDs are saved correct.
slope <- rename(slope, "intercept" = "(Intercept)")

# Repeat this for other variables of interest. 
# when you are done, you can save the new datafile. 
write_csv(slope, sprintf(paste("slope_file_wide_",output,".csv",sep ="", collapse=NULL)))

# summarize models in one table
outf <- sprintf(paste("LME_",output,".html",sep ="", collapse=NULL))
stargazer(fit,
          title="Linear Mixed Model Results",
          dep.var.labels.include = FALSE,
          report = "vc*s", omit = "Constant",
          column.labels = c("quadratic cuv"),
          digits = 2, star.cutoffs = c(0.05, 0.01, 0.001), align = TRUE, model.numbers = FALSE,
          out = outf , out.header = TRUE, flip = TRUE, ci=TRUE, single.row=TRUE)




# ---- 6. More Plots ---------------------------------------------------------------------------------------
# visualising model fit -> to see inter-individual variability and diagnostics to evaluate model fit ----
predplot_cubic_splines <- ggpredict(fit_cub_spline_df2, terms = c("t [all] "))
predplot_lin <- ggpredict(fit_lin, terms = c("t [all] "))
predplot_quad <- ggpredict(fit_quad, terms = c("t [all] "))
# here with coloured for each subj
# Define a function to create a plot for each model
create_model_plot <- function(data, model_name, color) {
  ggplot(data = WH_long, aes(x = t, y = ahei)) + 
    geom_point(aes(x = t, y = ahei), alpha = 0.3, size = 1) + # raw data points
    geom_line(aes(color = as.factor(oxf_id)), alpha = 0.3, size = 1) + # lines for each oxf_id
    ylab('AHEI') + 
    xlab("Years from baseline") +
    scale_x_continuous(breaks = seq(0, 20, 5)) +
    theme_set(theme_bw()) + theme_minimal() +
    theme(legend.title = element_blank(), 
          legend.position = "none", 
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 14)) +
    ggtitle(paste("AHEI with", model_name)) +
    geom_line(data = data, aes(x = x, y = predicted), color = color, size = 1) 
}
# Create individual plots for each model
plot_cubic_spline <- create_model_plot(predplot_cubic_splines, "Cubic Spline", "black")
plot_lin <- create_model_plot(predplot_lin, "Linear model", "black")
plot_quad <- create_model_plot(predplot_quad, "Quadratic model", "black")
# Arrange the plots in a grid
p <- grid.arrange(plot_lin, plot_cubic_spline, ncol = 2)
#p <- grid.arrange(plot_lin, plot_quad, plot_cubic_spline, ncol = 2)
## SAVE  PLOT
tmp=sprintf(paste("CompModelsfitAndIndiv_",output,".pdf",sep ="", collapse=NULL))
pdf(file = tmp, width = 10, height = 5)
grid.draw(p)  # Use grid.draw to ensure the background is set
dev.off()





# --- model fit across all sub ----
p <-ggplot(predplot_lin) + 
  theme_set(theme_bw()) + theme_minimal() + # minimal setting
  theme(legend.position = "none",panel.spacing = unit(1, "lines")) + #no legend
  geom_point(data=WH_long,aes(x=t,y=ahei),alpha=0.3,size=1) + #raw data points
  geom_line(aes(x = x, y = predicted)) +     # slope
  labs(x = "t", y = "ahei", title = "AHEI across phases")
## SAVE  PLOT
# Open a new SVG device
tmp=sprintf(paste("fitAndIndiv_",output,".pdf",sep ="", collapse=NULL))
#svg(tmp, width = 6, height = 6)
pdf(file = tmp, width = 5, height = 5)
# Create your plot
plot(p)
# Close the SVG device
dev.off()



# --- Plot each model using ggeffects' built-in plot function -> to see just the fit and CIs
plot_cubic_spline <- plot(predplot_cubic_splines, facet = TRUE, (cex = 3)) + 
  theme_set(theme_bw()) +theme_minimal() + 
  labs(x = "Years from baseline", y = "AHEI", title = "AHEI across phases") +
  ggtitle("Observed AHEI with Cubic Spline") +
  theme(text = element_text(size = 18))+scale_x_continuous(breaks=seq(0,30,5))
plot_lin <- plot(predplot_lin, facet = TRUE, (cex = 3)) + 
  theme_set(theme_bw()) +theme_minimal() + 
  labs(x = "Years from baseline", y = "AHEI", title = "AHEI across phases") +
  ggtitle("Observed AHEI with Linear Model") +
  theme(text = element_text(size = 18))+scale_x_continuous(breaks=seq(0,30,5))
plot_quad <- plot(predplot_quad, facet = TRUE, (cex = 3))+ 
  theme_set(theme_bw()) +theme_minimal() + 
  labs(x = "Years from baseline", y = "AHEI", title = "AHEI across phases") +
  ggtitle("Observed AHEI with Quad Model") +
  theme(text = element_text(size = 18))+scale_x_continuous(breaks=seq(0,30,5))
# Arrange the plots in a grid
#p <- grid.arrange(plot_lin, plot_quad, plot_cubic_spline,ncol = 2)
p <- grid.arrange(plot_lin, plot_cubic_spline,ncol = 2)
# Open a new SVG device
tmp=sprintf(paste("fitComp_",output,".pdf",sep ="", collapse=NULL))
#svg(tmp, width = 6, height = 6)
pdf(file = tmp, width = 10, height = 5)
# Create your plot
plot(p)
# Close the SVG device
dev.off()


# --- compare cubic spline and linear model ----
# Extract data from ggeffects objects
data_lin <- as.data.frame(predplot_lin)
data_cubic_spline <- as.data.frame(predplot_cubic_splines)
# Create a combined plot
combined_plot <- ggplot() +
  geom_line(data = data_lin, aes(x = x, y = predicted, color = "Linear Model"), size = 1) +
  geom_ribbon(data = data_lin, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  geom_line(data = data_cubic_spline, aes(x = x, y = predicted, color = "Cubic Spline Model"), size = 1) +
  geom_ribbon(data = data_cubic_spline, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
  labs(x = "Years from baseline", y = "AHEI", title = "Comparison of Linear and Cubic Spline Models") +
  scale_color_manual(values = c("Linear Model" = "blue", "Cubic Spline Model" = "red")) +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, 30, 5))
# Display the combined plot
print(combined_plot)
## SAVE  PLOT
# Open a new SVG device
tmp=sprintf(paste("fitComp_oneplot_",output,".pdf",sep ="", collapse=NULL))
#svg(tmp, width = 6, height = 6)
pdf(file = tmp, width = 10, height = 10)
# Create your plot
plot(combined_plot)
# Close the SVG device
dev.off()


# predicted line plot:
plot(predplot_lin, facet = TRUE, (cex = 3))+
  theme_set(theme_bw()) +theme_minimal() + 
  theme(text = element_text(size = 18)) +
  labs(x = "Years from baseline", y = "AHEI", title = "AHEI across waves") +
  scale_x_continuous(breaks=seq(0,30,5))


# fitted lines:
# Set up the PDF output for fitted line plots
tmp = sprintf(paste("fitComp_fittedline_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 1.5)  # Adjust the cex parameter to increase font size
# Create fitted line plots
p1 <- plot(fit_lin, ahei ~ fitted(.), abline = c(0, 1), cex = 0.5, main ="Fitted Line: Linear Model" )
p2 <- plot(fit_cub_spline_df2, ahei ~ fitted(.), abline = c(0, 1), cex = 0.5, main ="Fitted Line: Cubic Spline Model")
p <- grid.arrange(p1, p2, ncol = 2)
plot(p)
# Close the PDF device
dev.off()

# residuals:
# Set up the PDF output for residuals plots
tmp = sprintf(paste("fitComp_residuals_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 1.5)  # Adjust the cex parameter to increase font size
# Create residuals plots
p1 <- plot(fit_lin, ahei ~ resid(.), main  ="Fitted Line: Linear Model" )
p2 <- plot(fit_cub_spline_df2, ahei ~ resid(.), main = "Fitted Line: Cubic Spline Model")
p <- grid.arrange(p1, p2, ncol = 2)
plot(p)
# Close the PDF device
dev.off()


# Set up the PDF output
tmp = sprintf(paste("fitComp_residualsfit_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 3)  # Adjust the cex parameter to increase font size
# Create QQ plots
p1 <- qqnorm(fit_lin, ~ resid(., type = "p"), abline = c(0, 1), main = "Residuals: Linear Model")
p2 <- qqnorm(fit_cub_spline_df2, ~ resid(., type = "p"), abline = c(0, 1), main = "Cubic Spline Model")
# Arrange the plots in a grid
p <- grid.arrange(p1, p2, ncol = 2)
# Plot and save to PDF
plot(p)
dev.off()



# ---- 7. Compare mean centering ---------------------

fit_lin_m<- lme(ahei~t_scale, random=~t_scale|oxf_id, data=WH_long, correlation=corCAR1(form=~t_scale|oxf_id/phase), method="ML",na.action=na.exclude, control=list(opt="optim")) 
print(anova(fit_lin,fit_lin_m)) 
tab_model(fit_lin, fit_lin_m,p.style = "stars",digits = 2)


predplot_lin <- ggpredict(fit_lin, terms = c("t [all] "))
predplot_lin_m <- ggpredict(fit_lin_m, terms = c("t_scale [all] "))
# Create individual plots for each model
plot_lin <- create_model_plot(predplot_lin, "Linear model", "black")
create_model_plot <- function(data, model_name, color) {
  ggplot(data = WH_long, aes(x = t, y = ahei)) + 
    geom_point(aes(x = t, y = ahei), alpha = 0.3, size = 1) + # raw data points
    geom_line(aes(color = as.factor(oxf_id)), alpha = 0.3, size = 1) + # lines for each oxf_id
    ylab('AHEI') + 
    xlab("Years from baseline") +
  #scale_x_continuous(breaks = seq(0, 20, 5)) +
    theme_set(theme_bw()) + theme_minimal() +
    theme(legend.title = element_blank(), 
          legend.position = "none", 
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 14)) +
    ggtitle(paste("AHEI with", model_name)) +
    geom_line(data = data, aes(x = x, y = predicted), color = color, size = 1) 
}
plot_lin_m <- create_model_plot(predplot_lin_m,"Linear mean-centered model", "black")
# Arrange the plots in a grid
p <- grid.arrange(plot_lin, plot_lin_m, ncol = 2)

p1 <- plot(fit_lin, ahei~ resid(.))
p2 <- plot(fit_lin_m, ahei~ resid(.))
p <- grid.arrange(p1, p2, ncol = 2)

p1 <- plot(fit_lin, ahei~ fitted(.) , abline = c(0,1), cex=0.5)
p2 <- plot(fit_lin_m, ahei~ fitted(.) , abline = c(0,1), cex=0.5)
p <- grid.arrange(p1, p2, ncol = 2)



# ---- correlation plots: -----
cor_lin_m <- coef(fit_lin_m)
cor_lin <- coef(fit_lin)
# Extract relevant columns
t_lin_m <- cor_lin_m[, "t_scale"]
t_lin <- cor_lin[, "t"]
i_lin_m <- cor_lin_m[, "(Intercept)"]
i_lin <- cor_lin_m[, "(Intercept)"]


# Plot correlations
# Using base R
par(mfrow=c(1, 2)) # Set up a 1x2 plotting space
# Plot for quadratic model
plot(i_lin, i_lin_m, main="intercept in Linear vs. Linear mean-centered Model",
     xlab="intercept (Linear)", ylab="intercept (Linear mean-centered)", pch=19, col="blue")
# Plot for comparison between models
plot(t_lin, t_lin_m, main="t in Linear vs. Linear mean-centered Model",
     xlab="t (Linear)", ylab="t (Linear mean-centered)", pch=19, col="red")
# Reset plotting space
par(mfrow=c(1, 1))
# Calculate correlation and p-value for intercept in quadratic model and spline model
cor_test_i_quad_cub <- cor.test(i_lin, i_lin_m)
cor_coef_i_quad_cub <- cor_test_i_quad_cub$estimate
p_value_i_quad_cub <- cor_test_i_quad_cub$p.value
# Print results
cat("Correlation between intercept model:\n")
cat("Correlation coefficient:", cor_coef_i_quad_cub, "\n")
cat("P-value:", p_value_i_quad_cub, "\n")
# Calculate correlation and p-value for t in quadratic model and t in spline model
cor_test_t_quad_cub <- cor.test(t_lin, t_lin_m)
cor_coef_t_quad_cub <- cor_test_t_quad_cub$estimate
p_value_t_quad_cub <- cor_test_t_quad_cub$p.value
# Print results
cat("Correlation between t in models:\n")
cat("Correlation coefficient:", cor_coef_t_quad_cub, "\n")
cat("P-value:", p_value_t_quad_cub, "\n")


# end
