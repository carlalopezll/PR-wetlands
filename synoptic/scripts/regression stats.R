# List of predictors
predictors <- c("Temp_C", "DOC_avg", "DO_mgL", "TDN_avg", "Cl_avg")

# Responses
responses <- c("CO2_gL", "CH4_log")  # use logged CH4 as appropriate

# Function to extract regression stats
get_lm_stats <- function(data, response, predictors) {
  results <- data.frame(
    Response = character(),
    Predictor = character(),
    F_stat = numeric(),
    p_value = numeric(),
    R_squared = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (pred in predictors) {
    # Fit model
    formula <- as.formula(paste(response, "~", pred))
    mod <- lm(formula, data = data)
    summ <- summary(mod)
    
    # Extract F-statistic and p-value from overall regression
    fstat <- summ$fstatistic[1]
    pval <- pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE)
    
    # R-squared
    r2 <- summ$r.squared
    
    # Add row
    results <- rbind(results, data.frame(
      Response = response,
      Predictor = pred,
      F_stat = round(fstat, 2),
      p_value = signif(pval, 3),
      R_squared = round(r2, 3)
    ))
  }
  
  return(results)
}

# CO2 table
co2_stats <- get_lm_stats(merge, "CO2_gL", predictors)

# CH4 table
ch4_stats <- get_lm_stats(merge, "CH4_log", predictors)

# Combine into one table if desired
all_stats <- rbind(co2_stats, ch4_stats)

# View table
all_stats
