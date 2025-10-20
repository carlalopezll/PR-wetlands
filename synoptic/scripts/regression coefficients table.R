# Define a function to extract p-value and R-squared
extract_model_info <- function(model) {
  p_value <- summary(model)$coefficients[2, 4]  # Extract p-value for the predictor variable
  r_squared <- summary(model)$r.squared  # Extract R-squared value
  return(c(p_value = p_value, r_squared = r_squared))
}

# Store results in a data frame
model_results <- data.frame(
  Variable = c("Temp_C vs CO2_gL", "Temp_C vs CH4_log", "DOC_avg vs CO2_gL", "DOC_avg vs CH4_log", 
               "DO_mgL vs CO2_gL", "DO_mgL vs CH4_log", "TDN_avg vs CO2_gL", "TDN_avg vs CH4_log",
               "Cl_avg vs CO2_gL", "Cl_avg vs CH4_log"),
  P_Value = NA,
  R_Squared = NA
)

# Fit models and extract statistics
# Temp_C vs CO2_gL
model_temp_a <- lm(CO2_gL ~ Temp_C, merge)
model_results[1, 2:3] <- extract_model_info(model_temp_a)

# Temp_C vs CH4_log
model_temp_b <- lm(CH4_log ~ Temp_C, merge)
model_results[2, 2:3] <- extract_model_info(model_temp_b)

# DOC_avg vs CO2_gL
model_DOC_a <- lm(CO2_gL ~ DOC_avg, merge)
model_results[3, 2:3] <- extract_model_info(model_DOC_a)

# DOC_avg vs CH4_log
model_DOC_b <- lm(CH4_log ~ DOC_avg, merge)
model_results[4, 2:3] <- extract_model_info(model_DOC_b)

# DO_mgL vs CO2_gL
model_DO_a <- lm(CO2_gL ~ DO_mgL, merge)
model_results[5, 2:3] <- extract_model_info(model_DO_a)

# DO_mgL vs CH4_log
model_DO_b <- lm(log(CH4_mgL) ~ DO_mgL, merge)
model_results[6, 2:3] <- extract_model_info(model_DO_b)

# TDN_avg vs CO2_gL
model_TDN_a <- lm(CO2_gL ~ TDN_avg, merge)
model_results[7, 2:3] <- extract_model_info(model_TDN_a)

# TDN_avg vs CH4_log
model_TDN_b <- lm(CH4_log ~ TDN_avg, merge)
model_results[8, 2:3] <- extract_model_info(model_TDN_b)

# Cl_avg vs CO2_gL
model_Cl_a <- lm(CO2_gL ~ Cl_avg, merge)
model_results[9, 2:3] <- extract_model_info(model_Cl_a)

# Cl_avg vs CH4_log
model_Cl_b <- lm(CH4_log ~ Cl_avg, merge)
model_results[10, 2:3] <- extract_model_info(model_Cl_b)

# Print results
print(model_results)
