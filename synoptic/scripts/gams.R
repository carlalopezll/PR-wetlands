# Load library
library(mgcv)
library(ggplot2)
library(dplyr)
library(e1071)  # for skewness/kurtosis
library(car)    # for Levene's test

# ---- 1. Fit models ----
# Linear model (non-logged predictor)
lm_nonlog <- lm(CO2_gL ~ Temp_C, data = merge)

# Linear model (logged predictor)
lm_log <- lm(CO2_gL ~ log(Temp_C), data = merge)

# GAM model
gam_mod <- gam(CO2_gL ~ s(Temp_C), data = merge)

# ---- 2. Residual diagnostics ----
# Put residuals in a dataframe
resid_df <- data.frame(
  residuals = c(resid(lm_nonlog), resid(lm_log), resid(gam_mod)),
  model = rep(c("Non-logged LM", "Logged LM", "GAM"),
              c(length(resid(lm_nonlog)),
                length(resid(lm_log)),
                length(resid(gam_mod))))
)

# Density plots
ggplot(resid_df, aes(x = residuals, color = model, fill = model)) +
  geom_density(alpha = 0.3) +
  labs(x = "Residuals", y = "Density", title = "Residual distributions") +
  theme_minimal()

# QQ plots
ggplot(resid_df, aes(sample = residuals, color = model)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(~ model, scales = "free") +
  labs(title = "QQ-plots of residuals") +
  theme_minimal()

# Residual vs fitted
resid_fit <- data.frame(
  residuals = c(resid(lm_nonlog), resid(lm_log), resid(gam_mod)),
  fitted = c(fitted(lm_nonlog), fitted(lm_log), fitted(gam_mod)),
  model = rep(c("Non-logged LM", "Logged LM", "GAM"),
              c(length(resid(lm_nonlog)),
                length(resid(lm_log)),
                length(resid(gam_mod))))
)

ggplot(resid_fit, aes(x = fitted, y = residuals, color = model)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ model, scales = "free") +
  labs(title = "Residuals vs Fitted values") +
  theme_minimal()

# ---- 3. Summary stats ----
resid_df %>%
  group_by(model) %>%
  summarise(
    mean = mean(residuals),
    sd = sd(residuals),
    skewness = skewness(residuals),
    kurtosis = kurtosis(residuals),
    .groups = "drop"
  )

# ---- 4. Normality tests ----
shapiro.test(resid(lm_nonlog))
shapiro.test(resid(lm_log))
shapiro.test(resid(gam_mod))

# ---- 5. Model comparison ----
AIC(lm_nonlog, lm_log, gam_mod)
BIC(lm_nonlog, lm_log, gam_mod)

rmse <- function(m) sqrt(mean(resid(m)^2))
c(
  Non_logged_RMSE = rmse(lm_nonlog),
  Logged_RMSE = rmse(lm_log),
  GAM_RMSE = rmse(gam_mod)
)

# ---- 6. Plot the GAM smooth ----
ggplot(merge, aes(x = Temp_C, y = CO2_gL)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "blue") +
  labs(title = "GAM fit of CO2 vs Temp", x = "Temperature (°C)", y = "CO2 (g/L)") +
  theme_minimal()
