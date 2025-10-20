# List of predictors
predictors <- c("Temp_C", "DOC_avg", "DO_mgL", "TDN_avg", "Cl_avg")

# Function to create clean summary table
create_summary_table <- function(data, response, predictors) {
  results <- data.frame(
    Predictor = predictors,
    Nonlogged_Normal = NA,
    Logged_Normal = NA,
    KS_D = NA,
    Preferred_Model = NA
  )
  
  for (i in seq_along(predictors)) {
    pred <- predictors[i]
    
    # Fit models
    mod_nonlog <- lm(as.formula(paste(response, "~", pred)), data = data)
    mod_log <- lm(as.formula(paste("log(", response, ") ~", pred)), data = data)
    
    # Residuals
    resid_nonlog <- resid(mod_nonlog)
    resid_log <- resid(mod_log)
    
    # Shapiro-Wilk tests
    sh_nonlog <- shapiro.test(resid_nonlog)
    sh_log <- shapiro.test(resid_log)
    
    # KS test
    ks <- ks.test(resid_nonlog, resid_log)
    
    # Normality check ✔ if p > 0.05
    results$Nonlogged_Normal[i] <- ifelse(sh_nonlog$p.value > 0.05, "✔", "✘")
    results$Logged_Normal[i] <- ifelse(sh_log$p.value > 0.05, "✔", "✘")
    
    # KS statistic
    results$KS_D[i] <- round(ks$statistic, 3)
    
    # Preferred model: choose the one with normal residuals
    if (sh_nonlog$p.value > 0.05 & sh_log$p.value <= 0.05) {
      results$Preferred_Model[i] <- "Non-logged"
    } else if (sh_log$p.value > 0.05 & sh_nonlog$p.value <= 0.05) {
      results$Preferred_Model[i] <- "Logged"
    } else if (sh_nonlog$p.value > sh_log$p.value) {
      results$Preferred_Model[i] <- "Non-logged"
    } else {
      results$Preferred_Model[i] <- "Logged"
    }
  }
  
  return(results)
}

# Create tables
co2_summary <- create_summary_table(merge, "CO2_gL", predictors)
ch4_summary <- create_summary_table(merge, "CH4_mgL", predictors)

# View
co2_summary
ch4_summary
