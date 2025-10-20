# Create a grid of Cl values across your observed range
newdat <- data.frame(Cl_avg = seq(min(merge$Cl_avg, na.rm = TRUE),
                                  max(merge$Cl_avg, na.rm = TRUE),
                                  length.out = 200))

# Get predictions and SE for the grid
pred <- predict(seg_model, newdata = newdat, se.fit = TRUE)

newdat$CH4_fit <- pred$fit
newdat$CH4_se  <- pred$se.fit

# Plot with points + fitted line + confidence ribbon
ggplot(merge, aes(x = Cl_avg, y = CH4_mgL)) +
  geom_point(size = 5, aes(color = Season)) +
  geom_ribbon(data = newdat, 
              aes(x = Cl_avg,
                  ymin = CH4_fit - 1.96 * CH4_se,
                  ymax = CH4_fit + 1.96 * CH4_se),
              inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_line(data = newdat, aes(x = Cl_avg, y = CH4_fit), 
            color = "black", linewidth = 1) +
  labs(x = "Chloride (mg/L)", y = CH4_lab) +
  scale_y_log10() +
  ylim(0,NA) +
  theme