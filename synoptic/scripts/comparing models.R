# Load libraries
library(ggplot2)
library(dplyr)
library(e1071)    # for skewness/kurtosis
library(car)      # for Levene's test


hist(merge$CO2_gL)

shapiro.test(resid(model_raw))

model_raw  <- lm(CO2_gL ~ Temp_C, data = merge)
model_log  <- lm(log(CO2_gL) ~ Temp_C, data = merge)

model1 <- model_raw
model2 <- model_log


library(MASS)

#find optimal lambda for Box-Cox transformation 
bc <- boxcox(CO2_gL~Temp_C, data = merge)
(lambda <- bc$x[which.max(bc$y)])

new_model <- lm(((CO2_gL^lambda-1)/lambda) ~ Temp_C, data = merge)
summary(new_model)

#define plotting area
op <- par(pty = "s", mfrow = c(1, 3))

#Q-Q plot for original model
qqnorm(model1$residuals)
qqline(model1$residuals)

#Q-Q plot for logged model
qqnorm(model2$residuals)
qqline(model2$residuals)

#Q-Q plot for Box-Cox transformed model
qqnorm(new_model$residuals)
qqline(new_model$residuals)

#display both Q-Q plots
par(op)



# Extract residuals
resid_df <- data.frame(
  residuals = c(resid(model1), resid(model2)),
  model = rep(c("Non-logged", "Logged"),
              c(length(resid(model1)), length(resid(model2))))
)

## ---- 1. Visual comparisons ----
# Density plots
ggplot(resid_df, aes(x = residuals, color = model, fill = model)) +
  geom_density(alpha = 0.3) +
  labs(x = "Residuals", y = "Density", title = "Residual distributions") +
  theme_minimal()

# QQ plots side by side
ggplot(resid_df, aes(sample = residuals, color = model)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(~ model, scales = "free") +
  labs(title = "QQ-plots of residuals") +
  theme_minimal()

# Residual vs fitted plots
resid_fit <- data.frame(
  residuals = c(resid(model1), resid(model2)),
  fitted = c(fitted(model1), fitted(model2)),
  model = rep(c("Non-logged", "Logged"),
              c(length(resid(model1)), length(resid(model2))))
)

ggplot(resid_fit, aes(x = fitted, y = residuals, color = model)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ model, scales = "free") +
  labs(title = "Residuals vs Fitted values") +
  theme_minimal()

## ---- 2. Descriptive statistics ----
resid_df %>%
  group_by(model) %>%
  summarise(
    mean = mean(residuals),
    sd = sd(residuals),
    skewness = skewness(residuals),
    kurtosis = kurtosis(residuals),
    .groups = "drop"
  )

## ---- 3. Statistical tests ----
# Shapiro-Wilk test for normality (each model separately)
shapiro.test(resid(model1))
shapiro.test(resid(model2))

# Levene's test for variance equality
leveneTest(residuals ~ model, data = resid_df)

# KS test for distributional differences
ks.test(resid(model1), resid(model2))

## ---- 4. Model comparison ----
AIC(model1, model2)
BIC(model1, model2)

# Or RMSE if you want:
rmse <- function(m) sqrt(mean(resid(m)^2))
c(Non_logged_RMSE = rmse(model1),
  Logged_RMSE = rmse(model2))
