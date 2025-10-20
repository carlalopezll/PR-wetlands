library(lme4)
library(lmerTest)

install.packages("https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.6-5.tar.gz", repos = NULL, type = "source")

model <- lmer(CO2_total ~ treatment_label * period * timepoint +
                (1 | rep), data = rates)
anova(model, type = 3) # to get interaction p-values