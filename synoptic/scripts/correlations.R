library(corrgram)
library(corrplot)
library(ggfortify) # for autoplot
library(factoextra)
library(dplyr)


subset <- merge2 %>%
  select(CO2_avg, CH4_avg, TKN_N, TP, DP, NO3_N, DNH4_N, TOC, Hardness, Al, Ca, Fe, K, Mg, Mn, Na, Si, Cl)


# Creating a correlation matrix and plotting the corrolelogram
cor <- cor(na.omit(subset))
view(cor)

# Plot a corrolelogram
corrplot(cor, method="circle", type="lower", na.label=" ")

testRes = cor.mtest(cor, conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(cor, p.mat = testRes$p, sig.level = 0.05, addrect = 2, type = "lower")
