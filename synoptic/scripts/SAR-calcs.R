

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Humedales Puerto Rico")

soils <- read.csv("PR wetlands soils.csv")

library(ggplot2)

# Assuming a soil bulk density of 1.4g/cm3
# 1. Calculate volume of soil

# 1 kg of soil = 1000g/1.4g/cm3



# 2. Convert soil anions/cations from g/kg or mg/kg to meq/L

# Na

soils$Na_ppm <- soils$Na_g_Kg * 1000

# Multiply ppm by Valence/Atomic Weight

soils$Na_meq_L <- (soils$Na_ppm*1)/22.99

# K

soils$K_ppm <- soils$K_mg_Kg
soils$K_meq_L <- (soils$K_ppm*1)/39.1

# Ca

soils$Ca_ppm <- soils$Ca_g_Kg * 1000
soils$Ca_meq_L <- soils$Ca_ppm*(2/40.08)

# Mg

soils$Mg_ppm <- soils$Mg_g_Kg * 1000
soils$Mg_meq_L <- soils$Mg_ppm*(2/24.31)


# Then, calculate SAR, units in meq/L
# 3. SAR = Na + K / sqrt(Ca+Mg)

soils$SAR <- (soils$Na_meq_L + soils$K_meq_L) / sqrt((soils$Ca_meq_L+soils$Mg_meq_L)/2)

ggplot(soils, aes(x=Wetland_Name, y = Ca_ppm)) +
  geom_boxplot()

ggplot(soils, aes(x= Wetland_Name, y = SAR)) +
  geom_boxplot()

ggsave("PR wetlands SAR.jpg")
