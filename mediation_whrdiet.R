# MEDIATION
# Test if the association between diet/whr and cognitive performance is mediated via brain outcomes.
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
#install.packages("mediation") #uncomment this command in case you haven't installed the mediation package yet
library(mediation)
library(diagram)
library(gridExtra)
library(gridGraphics)
library(gridSVG)

# -------- START SCRIPT -------- 
setwd("XX/scripts/") # set path

# ----- First see if WM brain outcomes mediate the association between diet/whr and cognition: --------
# or file including already all modified tracts and art_pres: (see script: WM_dti_tracts_linearRegr.R)
metab_data <- read.csv('XX.csv');

# -----1. intercept WHR - cognition-----

# DV
x <- metab_data$intercept

# Cognitive performance measures which work previously
COG1 <- metab_data$OX.HVLT_TR # WORKS RD and MD
COG2 <- metab_data$OX.DST # WORKS RD and MD
COG3 <- metab_data$OX.FLU_C # WORKS
COG4 <- metab_data$OX.DCOD # WORKS MD and RD
COG5 <- metab_data$tmtba # ??

# MEDIATORS
# global WM:
y1 <- metab_data$MD# MEDIATOR WORKS with DST and DCOD!
y2 <- metab_data$RD# MEDIATOR WORKS with DST and DCOD!
y3 <- metab_data$FA

# test:
y4 <- metab_data$FA_WM_ILF# MEDIATOR WORKS with DST and DCOD
y5 <- metab_data$FA_WM_CINGULUM# MEDIATOR works with DST
y6 <- metab_data$RD_WM_ILF_1000# MEDIATOR works with DST DCOD
y7 <- metab_data$MD_WM_CINGULUM_1000# MEDIATOR works with DST DCOD
y8 <- metab_data$RD_WM_CINGULUM_1000# MEDIATOR works with DST DCOD


# ---- step 1: WHR (X) -> COG (Y) (this was already tested in cog_linearRegr.R) -----
#lm1 <-lm(x~COG1+sex+OX.AGE+OX.CHAM_MA++OX.EDUC_MOCA,data=metab_data)
#summary(lm1)
# --> we found that intercept WHR - five cognitive measures (COG1-COG5)
# ---- step 2: BRAIN (mediator) -> COG (Y) -----
# go through y1-10 and check which ones are significant

# Define the variables
cogs <- paste0("COG", 1:5)
ys <- paste0("y", 1:8)

# Loop through each combination of y and COG and assign models to separate variables
for (y in ys) {
  for (cog in cogs) {
    model_name <- paste0("lm_", y, "_", cog)
    formula <- as.formula(paste(y, "~", cog, "+ sex + scanner + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA"))
    assign(model_name, lm(formula, data = metab_data))
  }
}

# summary:
tab_model(lm_y1_COG1, lm_y1_COG2, lm_y1_COG3, lm_y1_COG4, lm_y1_COG5,
          lm_y2_COG1, lm_y2_COG2, lm_y2_COG3, lm_y2_COG4, lm_y2_COG5,
          lm_y3_COG1, lm_y3_COG2, lm_y3_COG3, lm_y3_COG4, lm_y3_COG5,
          lm_y4_COG1, lm_y4_COG2, lm_y4_COG3, lm_y4_COG4, lm_y4_COG5,
          lm_y5_COG1, lm_y5_COG2, lm_y5_COG3, lm_y5_COG4, lm_y5_COG5,
          lm_y6_COG1, lm_y6_COG2, lm_y6_COG3, lm_y6_COG4, lm_y6_COG5,
          lm_y7_COG1, lm_y7_COG2, lm_y7_COG3, lm_y7_COG4, lm_y7_COG5,
          lm_y8_COG1, lm_y8_COG2, lm_y8_COG3, lm_y8_COG4, lm_y8_COG5,
          auto.label = FALSE, show.est = TRUE, show.stat = FALSE, show.intercept = FALSE, 
          collapse.se = TRUE, digits = 4, string.est = "Beta", string.p = "p-value", 
          p.style = "numeric", digits.p = 3, 
          title = "Cognition - WM micostructure",
          rm.terms = c("sex", "scanner", "OX.AGE", "OX.CHAM_MA", "OX.EDUC_MOCA"))




# all significant y-s significant in step 2 ----

# Define the significant COG-y combinations with p<0.00625 for 8 mediators (y variables) 

# Identify significant combinations with p<0.05. //p < 0.00625
significant_combinations <- list()
for (y in ys) {
  for (cog in cogs) {
    model_name <- paste0("lm_", y, "_", cog)
    model <- get(model_name)
    p_value <- summary(model)$coefficients[cog, "Pr(>|t|)"]
    
    if (p_value < 0.05) {
      significant_combinations[[cog]] <- c(significant_combinations[[cog]], y)
    }
  }
}
# Create a list to store models for each COG
models <- list()
for (cog in names(significant_combinations)) {
  for (y in significant_combinations[[cog]]) {
    model_name <- paste0("lm_step2_", cog, "_", y)
    formula <- as.formula(paste("x ~", cog, "+", y, "+ sex + scanner + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA"))
    assign(model_name, lm(formula, data = metab_data))
    models[[model_name]] <- get(model_name)
  }
}

# summary step 2: ----
# Plot results in separate tab_models per COG

# OX.HVLT_TR
tab_model(
  lm_step2_COG1_y2, lm_step2_COG1_y3, lm_step2_COG1_y5, #lm_step2_COG1_y4
  auto.label = FALSE, show.est = TRUE, show.stat = FALSE, show.intercept = FALSE,
  collapse.se = TRUE, digits = 4, string.est = "Beta", string.p = "p-value",
  p.style = "numeric", digits.p = 4,
  title = "COG1 - WM microstructure",
  rm.terms = c("sex", "scanner", "OX.AGE", "OX.CHAM_MA", "OX.EDUC_MOCA")
)

# OX.DST
tab_model(
  lm_step2_COG2_y1, lm_step2_COG2_y2, lm_step2_COG2_y3, lm_step2_COG2_y4, lm_step2_COG2_y5,
  auto.label = FALSE, show.est = TRUE, show.stat = FALSE, show.intercept = FALSE, 
  collapse.se = TRUE, digits = 4, string.est = "Beta", string.p = "p-value", 
  p.style = "numeric", digits.p = 4, 
  title = "COG2 - WM microstructure",
  rm.terms = c("sex", "scanner", "OX.AGE", "OX.CHAM_MA", "OX.EDUC_MOCA")
)

# OX.FLU_C
tab_model(
  lm_step2_COG3_y2, lm_step2_COG3_y3,lm_step2_COG3_y4,
  auto.label = FALSE, show.est = TRUE, show.stat = FALSE, show.intercept = FALSE, 
  collapse.se = TRUE, digits = 4, string.est = "Beta", string.p = "p-value", 
  p.style = "numeric", digits.p = 4, 
  title = "COG3 - WM microstructure",
  rm.terms = c("sex", "scanner", "OX.AGE", "OX.CHAM_MA", "OX.EDUC_MOCA")
)

# OX.DCOD
tab_model(
  lm_step2_COG4_y1, lm_step2_COG4_y2, lm_step2_COG4_y3, lm_step2_COG4_y4, lm_step2_COG4_y5,
  auto.label = FALSE, show.est = TRUE, show.stat = FALSE, show.intercept = FALSE, 
  collapse.se = TRUE, digits = 4, string.est = "Beta", string.p = "p-value", 
  p.style = "numeric", digits.p = 4, 
  title = "COG4 - WM microstructure",
  rm.terms = c("sex", "scanner", "OX.AGE", "OX.CHAM_MA", "OX.EDUC_MOCA")
)

# tmtba
tab_model(
  lm_step2_COG5_y3, lm_step2_COG5_y4,#lm_step2_COG5_y2
  auto.label = FALSE, show.est = TRUE, show.stat = FALSE, show.intercept = FALSE,
  collapse.se = TRUE, digits = 4, string.est = "Beta", string.p = "p-value",
  p.style = "numeric", digits.p = 4,
  title = "COG5 - WM microstructure",
  rm.terms = c("sex", "scanner", "OX.AGE", "OX.CHAM_MA", "OX.EDUC_MOCA")
)





# ---- step 3: test mediation first lm then mediation bootstrap ----
# mediation when mediator (y variables) are significant in tables above
# to report results for which significant: in the order of the table:-------

#For each of the columns I want the "Estimate (CI.low - CI.high), p=p-value"
# step 3 WHR (x) -> Brain (M) 
lm_step2_COG1_y2$coefficients[3]
summary(lm_step2_COG1_y2)$coefficients[3, 4]
confint(lm_step2_COG1_y2)[3, ]
# step 2 BRAIN (M) -> COG 
lm_y2_COG1$coefficients[2] 
summary(lm_y2_COG1)$coefficients[2, 4]
confint(lm_y2_COG1)[2, ]
# Indirect effect: ACME 
med_2_COG1$d0 
med_2_COG1$d0.p
med_2_COG1$d0.ci
# Direct effect: ADE
med_2_COG1$z0
med_2_COG1$z0.p
med_2_COG1$z0.ci
# total/direct effect or step 1 (WHR (x) -> COG (y))
med_2_COG1$tau.coef 
med_2_COG1$tau.p
med_2_COG1$tau.ci
# Proportion mediated
med_2_COG1$n0
med_2_COG1$n0.p
med_2_COG1$n0.ci




# ---- step 4: automated FULL table: -------
library(mediation)
library(knitr)

# Define the cognitive and brain outcome mappings which have been shown significant relation between each of the paths
cogs <- list(
  COG1 = c("y2", "y3", "y5"),
  COG2 = c("y1", "y2", "y3", "y4", "y5"),
  COG3 = c("y2", "y3", "y4"),
  COG4 = c("y1", "y2", "y3", "y4", "y5"),
  COG5 = c("y3", "y4")
)

cog_names <- c(
  COG1 = "verbal episodic memory",
  COG2 = "digit span",
  COG3 = "semantic fluency",
  COG4 = "digit coding",
  COG5 = "trail making"
)

y_names <- c(
  y1 = "global MD",
  y2 = "global RD",
  y3 = "global FA",
  y4 = "FA in ILF",
  y5 = "FA in Cingulum",
  y6 = "RD in ILF",
  y7 = "MD in Cingulum",
  y8 = "RD in Cingulum"
)

# Function to format p-values
format_p_value <- function(p) {
  if (p < 0.001) {
    return("p<0.001")
  } else if (p < 0.01) {
    return(sprintf("p=%.3f", p))
  } else {
    return(sprintf("p=%.2f", p))
  }
}

# Create lists to store models and mediation results
mediator_models <- list()
outcome_models <- list()
mediation_results <- list()

# Loop through each COG and its significant y variables
for (cog in names(cogs)) {
  for (y in cogs[[cog]]) {
    # Define and fit the mediator model
    mediator_model_name <- paste0("lm_", y, "_", cog)
    mediator_formula <- as.formula(paste(y, "~", cog, "+ sex + scanner + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA"))
    assign(mediator_model_name, lm(mediator_formula, data = metab_data))
    mediator_models[[mediator_model_name]] <- get(mediator_model_name)
    
    # Define and fit the outcome model
    outcome_model_name <- paste0("lm_step2_", cog, "_", y)
    outcome_formula <- as.formula(paste("x ~", cog, "+", y, "+ sex + scanner + OX.AGE + OX.CHAM_MA + OX.EDUC_MOCA"))
    assign(outcome_model_name, lm(outcome_formula, data = metab_data))
    outcome_models[[outcome_model_name]] <- get(outcome_model_name)
    
    # Run mediation analysis
    med_name <- paste0("med_", y, "_", cog)
    mediation_results[[med_name]] <- mediate(
      model.m = mediator_models[[mediator_model_name]],
      model.y = outcome_models[[outcome_model_name]],
      treat = cog,
      mediator = y,
      boot = TRUE,
      sims = 1000
    )
  }
}

# Helper function to format numbers
format_number <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(sprintf("%.2e", x))
  } else {
    return(sprintf("%.2f", x))
  }
}

# Function to create a table for each combination
create_mediation_table <- function(cog, y, mediator_model, outcome_model, med_result) {
  
  # Extract and format results
  x_m_estimate <- format_number(outcome_model$coefficients[3])
  x_m_p <- format_p_value(summary(outcome_model)$coefficients[3, 4])
  x_m_ci <- sapply(confint(outcome_model)[3, ], format_number)
  
  m_y_estimate <- format_number(mediator_model$coefficients[2])
  m_y_p <- format_p_value(summary(mediator_model)$coefficients[2, 4])
  m_y_ci <- sapply(confint(mediator_model)[2, ], format_number)
  
  acme_estimate <- format_number(med_result$d0)
  acme_p <- format_p_value(med_result$d0.p)
  acme_ci <- sapply(med_result$d0.ci, format_number)
  
  ade_estimate <- format_number(med_result$z0)
  ade_p <- format_p_value(med_result$z0.p)
  ade_ci <- sapply(med_result$z0.ci, format_number)
  
  total_estimate <- format_number(med_result$tau.coef)
  total_p <- format_p_value(med_result$tau.p)
  total_ci <- sapply(med_result$tau.ci, format_number)
  
  prop_med_estimate <- format_number(med_result$n0)
  prop_med_p <- format_p_value(med_result$n0.p)
  prop_med_ci <- sapply(med_result$n0.ci, format_number)
  
  # Create a data frame for the results
  table <- data.frame(
    Y = cog_names[cog],
    M = y_names[y],
    `X -> M (CI)` = sprintf("%s (%s, %s), %s", x_m_estimate, x_m_ci[1], x_m_ci[2], x_m_p),
    `M -> Y (CI)` = sprintf("%s (%s, %s), %s", m_y_estimate, m_y_ci[1], m_y_ci[2], m_y_p),
    `ACME (Indirect effect)` = sprintf("%s (%s, %s), %s", acme_estimate, acme_ci[1], acme_ci[2], acme_p),
    `ADE (Direct effect)` = sprintf("%s (%s, %s), %s", ade_estimate, ade_ci[1], ade_ci[2], ade_p),
    `Total effect (direct + indirect)` = sprintf("%s (%s, %s), %s", total_estimate, total_ci[1], total_ci[2], total_p),
    `Proportion mediated` = sprintf("%s (%s, %s), %s", prop_med_estimate, prop_med_ci[1], prop_med_ci[2], prop_med_p)
  )
  
  return(table)
}

# Collect results into a table
results <- do.call(rbind, lapply(names(cogs), function(cog) {
  do.call(rbind, lapply(cogs[[cog]], function(y) {
    mediator_model_name <- paste0("lm_", y, "_", cog)
    outcome_model_name <- paste0("lm_step2_", cog, "_", y)
    med_name <- paste0("med_", y, "_", cog)
    
    create_mediation_table(
      cog, y,
      mediator_models[[mediator_model_name]],
      outcome_models[[outcome_model_name]],
      mediation_results[[med_name]]
    )
  }))
}))

# Print the results
print(knitr::kable(results, format = "pipe"))

### end automated table and copy in excel for formatting it


# ---- step 5: plot mediation for FIGURE 5B ------
´
# Function to create a mediation plot and capture it as a plot object
create_mediation_plot <- function(mediator, outcome, x_m_coef, m_y_coef, 
                                  x_m_ci, m_y_ci, 
                                  x_m_p, m_y_p,
                                  indirect_effect, indirect_ci, indirect_p,
                                  direct_effect, direct_ci, direct_p,
                                  plot_title) {
  # Coefficients for the plot, formatted over three lines
  coef <- c(0, paste0("'", x_m_coef, "\n(", x_m_ci, ")\np=", x_m_p, "'"), 0,  # X -> M
            0, 0, 0,
            paste0("'", m_y_coef, "\n(", m_y_ci, ")\np=", m_y_p, "'"), 
            "", 0)  # M -> Y, X -> Y
  
  # Create the matrix for the plot
  M <- matrix(nrow = 3, ncol = 3, byrow = TRUE, data = coef)
  
  # Plot the mediation diagram
  plotmat(M, pos = c(1, 2), name = c(mediator, "WHR intercept", outcome),
          box.type = "rect", box.size = 0.11, box.prop = 0.3, curve =0, 
          main = plot_title, cex.main = 1.2, # Centralize and enlarge title
          lwd = 1.5, arr.lwd = 1., arr.pos = 0.4, # Adjust arrow position
          box.cex = 1.2)  # Increase font size in boxes
  
  # Add annotations for direct and indirect effects directly below the plot
  mtext(side = 1, line = 2, adj = 0, cex = 1.0,  # Increase font size
        text = paste("Direct effect of WHR =", direct_effect, "(", direct_ci, "), p=", direct_p))
  
  # Split the last line into two lines
  mtext(side = 1, line = 3, adj = 0, cex = 1.0,
        text = paste("Indirect effect of WHR through", mediator, "="))
  mtext(side = 1, line = 4, adj = 0, cex = 1.0,
        text = paste(indirect_effect, "(", indirect_ci, "), p=", indirect_p))
  
  # Capture the plot as a plot object
  plot_obj <- recordPlot()
  return(plot_obj)
}

# Create and store plots for significant mediations as plot objects
plots <- list(
  create_mediation_plot(
    mediator = "global RD",
    outcome = "digit span",
    x_m_coef = "7359.05", m_y_coef = "-3.65e-07",
    x_m_ci = "3221.53, 11496.57", m_y_ci = "-5.64e-07, -1.67e-07",
    x_m_p = "<0.001", m_y_p = "<0.001",
    indirect_effect = "-2.69e-03", indirect_ci = "-5.03e-03, -9.25e-04", indirect_p = "<0.001",
    direct_effect = "-0.02", direct_ci = "-0.03, -6.36e-03", direct_p = "0.002",
    plot_title = "Mediation: Global RD -> Digit Span"
  ),
  create_mediation_plot(
    mediator = "global FA",
    outcome = "digit span",
    x_m_coef = "-11.14", m_y_coef = "2.66e-04",
    x_m_ci = "-16.95, -5.33", m_y_ci = "1.25e-04, 4.07e-04",
    x_m_p = "<0.001", m_y_p = "<0.001",
    indirect_effect = "-2.96e-03", indirect_ci = "-5.56e-03, -1.01e-03", indirect_p = "<0.001",
    direct_effect = "-0.02", direct_ci = "-0.03, -5.17e-03", direct_p = "0.004",
    plot_title = "Mediation: Global FA -> Digit Span"
  ),
  create_mediation_plot(
    mediator = "global MD",
    outcome = "digit coding",
    x_m_coef = "7778.52", m_y_coef = "-1.14e-07",
    x_m_ci = "2958.04, 12599", m_y_ci = "-1.88e-07, -4.03e-08",
    x_m_p = "0.002", m_y_p = "0.003",
    indirect_effect = "-8.88e-04", indirect_ci = "-1.73e-03, -2.40e-04", indirect_p = "0.004",
    direct_effect = "-7.65e-03", direct_ci = "-0.01, -3.33e-03", direct_p = "<0.001",
    plot_title = "Mediation: Global MD -> Digit Coding"
  ),
  create_mediation_plot(
    mediator = "global RD",
    outcome = "digit coding",
    x_m_coef = "7470.74", m_y_coef = "-1.55e-07",
    x_m_ci = "3329.34, 11612.15", m_y_ci = "-2.41e-07, -6.90e-08",
    x_m_p = "<0.001", m_y_p = "<0.001",
    indirect_effect = "-1.16e-03", indirect_ci = "-2.21e-03, -3.26e-04", indirect_p = "0.002",
    direct_effect = "-7.38e-03", direct_ci = "-0.01, -3.17e-03", direct_p = "0.004",
    plot_title = "Mediation: Global RD -> Digit Coding"
  ),
  create_mediation_plot(
    mediator = "global FA",
    outcome = "digit coding",
    x_m_coef = "-11.23", m_y_coef = "1.18e-04",
    x_m_ci = "-17.05, -5.41", m_y_ci = "5.73e-05, 1.79e-04",
    x_m_p = "<0.001", m_y_p = "<0.001",
    indirect_effect = "-1.33e-03", indirect_ci = "-2.38e-03, -5.18e-04", indirect_p = "0.002",
    direct_effect = "-7.21e-03", direct_ci = "-0.01, -2.74e-03", direct_p = "0.002",
    plot_title = "Mediation: Global FA -> Digit Coding"
  )
)

# save them directly from Rstudio.



# >>>> all other tests didn't show a significant (after FDR correction) diet/whr- brain relationship, thus a mediation effect is not expected <<<<

