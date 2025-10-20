# Checking residuals on logged vs non-logged models

model_raw  <- lm(CO2_gL ~ Temp_C, data = merge)
model_log  <- lm(log(CO2_gL) ~ Temp_C, data = merge)


merge$resid_raw <- residuals(model_raw)
merge$resid_log <- residuals(model_log)

library(performance)


check_model(model_raw)
check_model(model_log)

model_performance(model_raw)

ggplot() +
  geom_density(aes(x = resid(model_raw), color = "Non-logged")) +
  geom_density(aes(x = resid(model_log), color = "Logged")) +
  labs(x = "Residuals", y = "Density")

qqnorm(resid(model_raw)); qqline(resid(model_raw), col = "red")
qqnorm(resid(model_log)); qqline(resid(model_log), col = "blue")
