#lme  with random interncept and slopes on WHR
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
#library(powerlmm)
library(sjmisc)
library(devtools)
library(ggeffects)
library(splines)
library(lmerTest)
library(RColorBrewer)
library(svglite)
library(gridExtra)
library(grid)

#added for revision: remotes::install_github("mbojan/lspline", build_vignettes = TRUE)
#Reference: Greene, William H. 2003. Econometric Analysis. Pearson Education. Junger, Washington L., and Antonio Ponce de Leon. 2011. Ares: Environment Air Pollution Epidemiology: A Library for Timeseries Analysis. R package version 0.7.2 in CRAN’s archives.
library(lspline)
#for the plots: install R package ggiraphExtra from github.
#install.packages("devtools")
#devtools::install_github("cardiomoon/ggiraphExtra")

# load data --------------------------------------------------------------------
setwd("/XXscripts") # set path
WH_long <- read.csv("XX.csv") 
output=("WHR_XX")

# NaNs and controls --------------------------------------------------------------------
# Any NANs?
any(is.na(WH_long)) # FALSE

# note: think about all the variables you need here and to control for and add those in the model too (e.g., already correcting for age). 
# long table includudes:
# y = value , fixed (WHR index value)
# x = time t, random (age related to baseline in phase 1 in 5 phases (3,5,7,9,11))
# subjects = oxf_id (n=665)

# factors --------------------------------------------------------------------
# for LME the phase, oxf_id and metabolic factors need to be factors; 
WH_long$phase <- as.factor(WH_long$phase)
WH_long$oxf_id <- as.factor(WH_long$oxf_id)
levels(WH_long$phase) # check that the levels are correct.

# mean centering (revision request)--------------------------------------------------------------------
# to avoid colinearity issues; you can scale the variables. However, please note that the intercept looks at the moment when the value = 0. Hence, if you are interested in time at baseline, scaling is not handy.
# Scaling variables
WH_long$t_scale <- scale(WH_long$t, center = TRUE, scale = TRUE) # t - timepoint 0 taken as reference point.

# ---- individual lines: --------------------------------

# plot t - value
#plot(WH_long$t,WH_long$value) 

p_bw <- ggplot(WH_long,aes(t,value))+
  theme_set(theme_bw()) +theme_minimal() + 
  theme(text = element_text(size = 18)) +
  geom_point(alpha = 0.5)+
  geom_line(aes(group=oxf_id),alpha=0.1,size=0.5)+ 
  theme(legend.position = "none")  # adding space between panels

#plot(WH_long$t, WH_long$value)
# data = , aes(x = t, y = value)) + 
#   theme_classic() +
#   #geom_point(aes(colour = oxf_id), size = 0.2) + 
#   #facet_wrap(sex ~ ., strip.position="top") + 
#   theme(legend.title = element_blank(), legend.position = "top", panel.grid.minor.x = element_blank()) +
#   #scale_y_continuous(breaks=seq(500,4500,1000)) + 
#   ylab('WHR') + xlab("Age - years") +
#   #scale_x_continuous(breaks=c(4, 8, 12, 16, 20, 24, 28, 32, 36, 40)) +
#   guides(colour = guide_legend(override.aes = list(size=5))) +
#   ggtitle("A Observed WHR values")


# add spagetti plots with fitted line on top
WH_long$oxf_id <- as.numeric(as.character(WH_long$oxf_id))
spagetti_p <- ggplot(data = WH_long, aes(x = t, y = value, group = oxf_id)) + 
  geom_line(aes(color = oxf_id), alpha = 0.3, size = 1.5) + 
  geom_point(data = WH_long, aes(x = t, y = value), alpha = 0.3, size = 1) + # raw data points
  scale_colour_identity() + 
  ylab('WHR') + 
  xlab("Years from baseline") +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(limits = c(0.4, 1.4), breaks = seq(0.4, 1.4, 0.1)) +  # Set y-axis limits
  theme_bw() +
  theme(
    legend.title = element_blank(), 
    legend.position = "none", 
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 18)  # Larger text size
  ) 
## SAVE SPAGETTI PLOT
tmp = sprintf(paste("Spagettiplot_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 5, height = 5)
plot(spagetti_p)
dev.off()




# ---- 1. run models --------------------------------------------------------------------
# ---- run a linear and quadratic mixed effects model
# t - x random; value - y fixed
# autoregressive residuals, correlation between timepoints: CORCAR1(form=~t|oxf_id)
# continuous autoregressive process (AR(1) process for a continuous time covariate).

# random intercept and slope model
fit_lin<- lme(value~t, random=~t|oxf_id, data=WH_long, correlation=corCAR1(form=~t|oxf_id/phase), method="ML",na.action=na.exclude, control=list(opt="optim")) 
# natural cubic spline models
# 2 df
fit_cub_spline_df2<- lme(value~ns(t,2), random=~t|oxf_id, data=WH_long, correlation=corCAR1(form=~t|oxf_id/phase), method="ML",na.action=na.exclude, control=list(opt="optim")) 
# 3df
fit_cub_spline_df3<- lme(value~ns(t,3), random=~t|oxf_id, data=WH_long, correlation=corCAR1(form=~t|oxf_id/phase), method="ML",na.action=na.exclude, control=list(opt="optim")) 

# ---- 2. model comparison --------------------------------------------------------------------
# Does model represent an improvement over null?
print(anova(fit_lin,fit_cub_spline_df2)) 
#print(anova(fit_cub_spline_df2, fit_cub_spline_df3)) 

# choose the winner of anova for each factor:
fit <- fit_cub_spline_df2 # revision

# ---- 3. check model fit --------------------------------------------------------------------
tab_model(fit, p.style = "stars",digits = 2) # report in a table:

#for table in suppl with ahei and whr together use:
#fit_whr <- fit_cub_spline_df2
tab_model(fit_ahei,fit_whr, string.p = "p-value",p.style = "numeric", digits.p = 3,collapse.se = TRUE,digits = 2,show.p = TRUE, title = "Summary output of linear mixed effect models",show.aic = T,show.df = T,string.intercept = "Intercept",strings = c(fit_ahei="AHEI-2010",fit_whr="WHR"))


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

# ---- pred to plot individual fit ----
predplot1 <- ggpredict(fit, terms = c("t [all] "))
plot2 <-plot(predplot1, facet = TRUE, add.data = TRUE, ci = FALSE) + 
  theme_set(theme_bw()) +theme_minimal() +
  theme(text = element_text(size = 18)) +
  labs(x = "Years from baseline", y = "WHR", title = "WHR across Waves")
plot1 <-plot(predplot1, facet = TRUE, (cex = 3))+
  theme_set(theme_bw()) +theme_minimal() + 
  theme(text = element_text(size = 18)) +
  labs(x = "Years from baseline", y = "WHR", title = "WHR across Waves") +
  scale_x_continuous(breaks=seq(0,30,5))
plot3 <-plot(fit, value~ fitted(.) , abline = c(0,1), cex=0.5)
plot4 <-plot(fit, value~ resid(.))
tmp=sprintf(paste("Pred_",output,".pdf",sep ="", collapse=NULL))
pdf(file = tmp, width = 5, height = 5)
plot(plot1)
plot(plot2)
plot(plot3)
plot(plot4)
dev.off()

# also save plot1 as svg:
tmp=sprintf(paste("Pred_",output,".svg",sep ="", collapse=NULL))
plot(plot1)
ggsave(tmp, width = 4, height = 4)

# Save the plot as an SVG file
tmp <- sprintf("Pred_%s.svg", output)
ggsave(filename = tmp, plot = plot1, width = 4, height = 4)

rm(plot1,plot2,predplot1,tmp)



# ---- 5. save and use the best model ------------------------------------------------------------------------
# extracting slopes and intercepts for the LME
fit<-coef(fit)
tmp=sprintf(paste("WH_wide_",output,"_norand.csv",sep ="", collapse=NULL))
write.csv(fit,tmp)# this is not needed as its just a copy of below.
# Create file with slopes and ids (a similar approach needed if the quadratic model is more suitable)
slope <- read_csv(tmp)
slope <- rename(slope, "oxf_id" = "...1") #please check whether IDs are saved correct.
slope <- rename(slope, "intercept" = "(Intercept)")
# when you are done, you can save the new datafile. 
write_csv(slope, sprintf(paste("slope_file_wide_",output,"_norand.csv",sep ="", collapse=NULL)))
# summarize models in one table
outf <- sprintf(paste("LME_",output,".html",sep ="", collapse=NULL))
stargazer(fit,
          title="Linear Mixed Model Results",
          dep.var.labels.include = FALSE,
          report = "vc*s", omit = "Constant",
          column.labels = c("cubic spline"),
          digits = 2, star.cutoffs = c(0.05, 0.01, 0.001), align = TRUE, model.numbers = FALSE,
          out = outf , out.header = TRUE, flip = TRUE, ci=TRUE, single.row=TRUE)

# outf <- sprintf(paste("LME_",output,".html",sep ="", collapse=NULL))
# stargazer(fit,
#           title="Linear Mixed Model Results",
#           dep.var.labels.include = FALSE,
#           report = "vc*s", omit = "Constant",
#           column.labels = c("quadratic cuv"),
#           digits = 2, star.cutoffs = c(0.05, 0.01, 0.001), align = TRUE, model.numbers = FALSE,
#           out = outf , out.header = TRUE, flip = TRUE, ci=TRUE, single.row=TRUE)




# ---- 6. More Plots ---------------------------------------------------------------------------------------

# visualising model fit -> to see inter-individual variability and diagnostics to evaluate model fit
predplot_cubic_splines <- ggpredict(fit_cub_spline_df2, terms = c("t [all] "))
predplot_lin <- ggpredict(fit_lin, terms = c("t [all] "))

# just for revision to compare 2 models with each other: Lin & cubc----

# --- compare cubic spline and linear model.------
data_lin <- as.data.frame(predplot_lin)
data_cubic_spline <- as.data.frame(predplot_cubic_splines)
# Create a combined plot
combined_plot <- ggplot() +
  geom_line(data = data_lin, aes(x = x, y = predicted, color = "Linear Model"), size = 1) +
  geom_ribbon(data = data_lin, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  geom_line(data = data_cubic_spline, aes(x = x, y = predicted, color = "Cubic Spline Model"), size = 1) +
  geom_ribbon(data = data_cubic_spline, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
  labs(x = "Years from baseline", y = "WHR", title = "Comparison of Linear and Cubic Spline Models") +
  scale_color_manual(values = c("Linear Model" = "blue", "Cubic Spline Model" = "red")) +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(0, 30, 5))
# Display the combined plot
print(combined_plot)
# Save the plot to a PDF
tmp = sprintf(paste("fitCompLin_oneplot_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
print(combined_plot)  # Use print for ggplot objects
dev.off()




WH_long$pred_model1 <- predict(fit_lin)
WH_long$pred_model2 <- predict(fit_cub_spline_df2)
ggplot(WH_long, aes(x = t, y = value)) +
  geom_point(alpha = 0.5) +  # Originaldaten
  geom_line(aes(y = pred_model1), color = "blue", linetype = "dashed") +  # Modell 1
  geom_line(aes(y = pred_model2), color = "red") +  # Modell 2
  labs(title = "Modellanpassung: Modell 1 vs. Modell 2",
       x = "t", y = "value") +
  theme_minimal()
# Residuen plotten
par(mfrow = c(1, 2))  # Zwei Plots nebeneinander
# Residuen für Modell 1
plot(resid(fit_lin), main = "Residuen Linear Model", ylab = "Residuen", xlab = "Index")
abline(h = 0, col = "red", lty = 2)
# Residuen für Modell 2
plot(resid(fit_cub_spline_df2), main = "Residuen Cubic Spine Model", ylab = "Residuen", xlab = "Index")
abline(h = 0, col = "red", lty = 2)


# fitted lines plot: ----
# Set up the PDF output for fitted line plots
tmp = sprintf(paste("fitComp_fittedlineLin_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 1.5)  # Adjust the cex parameter to increase font size
# Create fitted line plots
p1 <- plot(fit_lin, value ~ fitted(.), abline = c(0, 1), cex = 0.5, main ="Fitted Line: Linear Model" )
p2 <- plot(fit_cub_spline_df2, value ~ fitted(.), abline = c(0, 1), cex = 0.5, main ="Fitted Line: Cubic Spline Model")
p <- grid.arrange(p1, p2, ncol = 2)
grid.draw(p)
# Close the PDF device
dev.off()

# residuals plot: ----
# Set up the PDF output for residuals plots
tmp = sprintf(paste("fitComp_residualsLin_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 1.5)  # Adjust the cex parameter to increase font size
# Create residuals plots
p1 <- plot(fit_lin, value ~ resid(.), main  ="Fitted Line: Linear Model" )
p2 <- plot(fit_cub_spline_df2, value ~ resid(.), main = "Fitted Line: Cubic Spline Model")
p <- grid.arrange(p1, p2, ncol = 2)
grid.draw(p)
# Close the PDF device
dev.off()

tmp = sprintf(paste("fitComp_residualsfitLin_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 3)  # Adjust the cex parameter to increase font size
# Create QQ plots
p1 <- qqnorm(fit_lin, ~ resid(., type = "p"), abline = c(0, 1), main = "Residuals: Linear Model")
p2 <- qqnorm(fit_cub_spline_df2, ~ resid(., type = "p"), abline = c(0, 1), main = "Cubic Spline Model")
# Arrange the plots in a grid
p <- grid.arrange(p1, p2, ncol = 2)
# Plot and save to PDF
grid.draw(p)
dev.off()








# ---- model fit across all sub: ------
p <-ggplot(predplot_cubic_splines) + 
  theme_set(theme_bw()) + theme_minimal() + # minimal setting
  theme(legend.position = "none",panel.spacing = unit(1, "lines")) + #no legend
  geom_point(data=WH_long,aes(x=t,y=value),alpha=0.3,size=1) + #raw data points
  geom_line(aes(x = x, y = predicted)) +     # slope
  labs(x = "t", y = "value", title = "WHR across phases")
tmp=sprintf(paste("fitAndIndiv_",output,".pdf",sep ="", collapse=NULL))
#svg(tmp, width = 6, height = 6)
pdf(file = tmp, width = 5, height = 5)
# Create your plot
plot(p)
# Close the SVG device
dev.off()



# ---- Plot each model using ggeffects' built-in plot function -> to see just the fit and CIs -----
plot_cubic_spline <- plot(predplot_cubic_splines, facet = TRUE, (cex = 3)) + 
  theme_set(theme_bw()) +theme_minimal() + 
  labs(x = "Years from baseline", y = "WHR", title = "WHR across phases") +
  ggtitle("Observed WHR with Cubic Spline") +
  theme(text = element_text(size = 18))+scale_x_continuous(breaks=seq(0,30,5))
plot_lin <- plot(predplot_lin, facet = TRUE, (cex = 3)) + 
  theme_set(theme_bw()) +theme_minimal() + 
  labs(x = "Years from baseline", y = "WHR", title = "WHR across phases") +
  ggtitle("Observed WHR with Linear Model") +
  theme(text = element_text(size = 18))+scale_x_continuous(breaks=seq(0,30,5))
# Arrange the plots in a grid
p <- grid.arrange(plot_lin, plot_cubic_spline,ncol = 2)
## SAVE  PLOT
tmp=sprintf(paste("fitComp_",output,".pdf",sep ="", collapse=NULL))
pdf(file = tmp, width = 20, height = 10)
# Create your plot
grid.draw(p)
dev.off()




# --- compare cubic spline and quadratic model.------
# Extract data from ggeffects objects
data_quad <- as.data.frame(predplot_quad)
data_cubic_spline <- as.data.frame(predplot_cubic_splines)
# Create a combined plot
combined_plot <- ggplot() +
  # Plot the quadratic model
  geom_line(data = data_quad, aes(x = x, y = predicted, color = "Quadratic Model"), size = 1) +
  geom_ribbon(data = data_quad, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Plot the cubic spline model
  geom_line(data = data_cubic_spline, aes(x = x, y = predicted, color = "Cubic Spline Model"), size = 1) +
  geom_ribbon(data = data_cubic_spline, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
  # Customize the plot
  labs(x = "Years from baseline", y = "WHR", title = "Comparison of Quadratic and Cubic Spline Models") +
  scale_color_manual(values = c("Quadratic Model" = "blue", "Cubic Spline Model" = "red")) +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(0, 30, 5))
# Display the combined plot
print(combined_plot)
## SAVE  PLOT
# Open a new SVG device
tmp=sprintf(paste("fitComp_oneplot_",output,".pdf",sep ="", collapse=NULL))
#svg(tmp, width = 6, height = 6)
pdf(file = tmp, width = 5, height = 5)
# Create your plot
plot(combined_plot)
# Close the SVG device
dev.off()



# ---- predicted line plot: -------
plot(predplot_cubic_splines, facet = TRUE, (cex = 3))+
  theme_set(theme_bw()) +theme_minimal() + 
  theme(text = element_text(size = 18)) +
  labs(x = "Years from baseline", y = "WHR", title = "WHR across waves") +
  scale_x_continuous(breaks=seq(0,30,5))

# fitted lines:
# Set up the PDF output for fitted line plots
tmp = sprintf(paste("fitComp_fittedline_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 1.5)  # Adjust the cex parameter to increase font size
# Create fitted line plots
p1 <- plot(fit_quad, value ~ fitted(.), abline = c(0, 1), cex = 0.5, main ="Fitted Line: Quadratic Model" )
p2 <- plot(fit_cub_spline_df2, value ~ fitted(.), abline = c(0, 1), cex = 0.5, main ="Fitted Line: Cubic Spline Model")
p <- grid.arrange(p1, p2, ncol = 2)
grid.draw(p)
# Close the PDF device
dev.off()

# residuals:
# Set up the PDF output for residuals plots
tmp = sprintf(paste("fitComp_residuals_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 1.5)  # Adjust the cex parameter to increase font size
# Create residuals plots
p1 <- plot(fit_quad, value ~ resid(.), main  ="Fitted Line: Quadratic Model" )
p2 <- plot(fit_cub_spline_df2, value ~ resid(.), main = "Fitted Line: Cubic Spline Model")
p <- grid.arrange(p1, p2, ncol = 2)
grid.draw(p)
# Close the PDF device
dev.off()


tmp = sprintf(paste("fitComp_residualsfit_", output, ".pdf", sep = "", collapse = NULL))
pdf(file = tmp, width = 10, height = 5)
# Increase font size using par()
par(cex = 3)  # Adjust the cex parameter to increase font size
# Create QQ plots
p1 <- qqnorm(fit_quad, ~ resid(., type = "p"), abline = c(0, 1), main = "Residuals: Quadratic Model")
p2 <- qqnorm(fit_cub_spline_df2, ~ resid(., type = "p"), abline = c(0, 1), main = "Cubic Spline Model")
# Arrange the plots in a grid
p <- grid.arrange(p1, p2, ncol = 2)
# Plot and save to PDF
grid.draw(p)
dev.off()




# ---- correlation plots: -----
cor_quad <- coef(fit_quad)
cor_cub <- coef(fit_cub_spline_df2)
# Extract relevant columns
t_quad <- cor_quad[, "t"]
t_cub <- cor_cub[, "t"]
i_quad <- cor_quad[, "(Intercept)"]
i_cub <- cor_cub[, "(Intercept)"]
# Plot correlations
# Using base R
par(mfrow=c(1, 2)) # Set up a 1x2 plotting space
# Plot for quadratic model
plot(i_quad, i_cub, main="intercept in Quadratic vs Spline Model",
     xlab="intercept (Quadratic)", ylab="intercept (Spline)", pch=19, col="blue")
# Plot for comparison between models
plot(t_quad, t_cub, main="t in Quadratic vs Spline Model",
     xlab="t (Quadratic)", ylab="t (Spline)", pch=19, col="red")
# Reset plotting space
par(mfrow=c(1, 1))
# Calculate correlation and p-value for intercept in quadratic model and spline model
cor_test_i_quad_cub <- cor.test(i_quad, i_cub)
cor_coef_i_quad_cub <- cor_test_i_quad_cub$estimate
p_value_i_quad_cub <- cor_test_i_quad_cub$p.value
# Print results
cat("Correlation between intercept in quadratic model and spline model:\n")
cat("Correlation coefficient:", cor_coef_i_quad_cub, "\n")
cat("P-value:", p_value_i_quad_cub, "\n")
# Calculate correlation and p-value for t in quadratic model and t in spline model
cor_test_t_quad_cub <- cor.test(t_quad, t_cub)
cor_coef_t_quad_cub <- cor_test_t_quad_cub$estimate
p_value_t_quad_cub <- cor_test_t_quad_cub$p.value
# Print results
cat("Correlation between t in quadratic model and t in spline model:\n")
cat("Correlation coefficient:", cor_coef_t_quad_cub, "\n")
cat("P-value:", p_value_t_quad_cub, "\n")






# ---- 7. Compare mean centering ---------------------

# repeat for mean-centres:

fit_cub_spline_df2_m<- lme(value~ns(t_scale,2), random=~t_scale|oxf_id, data=WH_long, correlation=corCAR1(form=~t_scale|oxf_id/phase), method="ML",na.action=na.exclude, control=list(opt="optim")) 
print(anova(fit_cub_spline_df2,fit_cub_spline_df2_m)) 

fit_lin <- fit_cub_spline_df2
fit_lin_m <- fit_cub_spline_df2_m

tab_model(fit_lin, fit_lin_m,p.style = "stars",digits = 2)


predplot_lin <- ggpredict(fit_lin, terms = c("t [all] "))
predplot_lin_m <- ggpredict(fit_lin_m, terms = c("t_scale [all] "))
# Create individual plots for each model
plot_lin <- create_model_plot(predplot_lin, "Linear model", "black")
create_model_plot <- function(data, model_name, color) {
  ggplot(data = WH_long, aes(x = t_scale, y = value)) + 
    geom_point(aes(x = t_scale, y = value), alpha = 0.3, size = 1) + # raw data points
    geom_line(aes(color = as.factor(oxf_id)), alpha = 0.3) + # lines for each oxf_id
    ylab('WHR') + 
    xlab("Years from baseline") +
    #scale_x_continuous(breaks = seq(0, 20, 5)) +
    theme_set(theme_bw()) + theme_minimal() +
    theme(legend.title = element_blank(), 
          legend.position = "none", 
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 18)) +
    ggtitle(paste("Observed WHR with", model_name)) +
    geom_line(data = data, aes(x = x, y = predicted), color = color, size = 1) 
}
plot_lin_m <- create_model_plot(predplot_lin_m,"Linear mean-centered model", "black")
# Arrange the plots in a grid
p <- grid.arrange(plot_lin, plot_lin_m, ncol = 2)

p1 <- plot(fit_lin_m, value~ resid(.))
p2 <- plot(fit_lin_m, value~ resid(.))
p <- grid.arrange(p1, p2, ncol = 2)

p1 <- plot(fit_lin, value~ fitted(.) , abline = c(0,1), cex=0.5)
p2 <- plot(fit_lin_m, value~ fitted(.) , abline = c(0,1), cex=0.5)
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
