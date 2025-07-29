amaral <- readxl::read_xlsx("global/Amaral et al_2019_data.xlsx")

amaral_site <- amaral %>%
  group_by(site) %>%
  summarise(CO2_mmolm3 = mean(CO2mmolm3))
