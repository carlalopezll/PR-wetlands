

om <- readxl::read_xlsx("incubations/data/Incubations_OM.xlsx")

om <- filter(om, period == "summer")


om_pa <- om %>%
  filter(site == "PA")

om_to <- om %>%
  filter(site == "TO")

summary(om_pa)

summary(om_to)
