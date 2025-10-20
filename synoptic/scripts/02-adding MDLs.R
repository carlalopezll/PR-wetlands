# adding minimum detection limits
# Do this for the negative CH4 values as well # need to discuss with Erin

ic <- read_csv("synoptic/data/IC synoptic data.csv")

# converting anions from ug/L to mg/L
ic <- ic %>%
  mutate(`F` = `F`/1000,
         Cl = Cl/1000,
         Br = Br/1000,
         `NO3-N` = `NO3-N`/1000,
         `PO4-P` = `PO4-P`/1000,
         SO4 = SO4/1000)

# Adding values of half the minimum detection limit to below detects and multiplying by the dilution factor

ic_mdl <- ic %>%
  mutate(Date_corrected = as.Date(Sample_date, format = '%m/%d/%Y')) %>%
  mutate(
    `F` = if_else(is.na(`F`), 0.064238/2, `F` * dilution_factor),
    Cl = if_else(is.na(Cl), 0.129321/2, Cl * dilution_factor),
    Br = if_else(is.na(Br), 0.12476/2, Br * dilution_factor),
    `NO3-N` = if_else(is.na(`NO3-N`), 0.047042/2, `NO3-N` * dilution_factor),
    `PO4-P` = if_else(is.na(`PO4-P`), 0.101131/2, `PO4-P` * dilution_factor),
    SO4 = if_else(is.na(SO4), 0.189399/2, SO4 * dilution_factor),
    Na = if_else(is.na(Na), 0.0548/2, Na * dilution_factor),
    `NH4-N` = if_else(is.na(`NH4-N`), 0.0839/2, `NH4-N` * dilution_factor),
    K = if_else(is.na(K), 0.0503/2, K * dilution_factor),
    Mg = if_else(is.na(Mg), 0.1661/2, Mg * dilution_factor),
    Ca = if_else(is.na(Ca), 0.1233/2, Ca * dilution_factor)
  )

write_csv(ic_mdl, "synoptic/data/IC w MDLs_PR synoptic.csv")

#### DOC and TDN ####

doc <- read_csv("synoptic/data/NPOC synoptic data.csv")
doc <- doc %>%
  mutate(DOC_mgL = DOC_mgL * dilution_factor,
         TDN_mgL = TDN_mgL * dilution_factor)

write_csv(doc, "synoptic/data/NPOC synoptic data_corrected.csv")


#### Solubles ####

# update MDLs
# there are no NAs in the chem data, should I just do this for the negative values?

chem <- read_csv("synoptic/data/Solubles_PR synoptic.csv")
chem$Date_corrected <- as.Date(chem$Date, format = '%m/%d/%Y')

# `NO3-N` = if_else(is.na(`NO3-N`) | `NO3-N` < 0, 0.04767/2, `NO3-N` * dilution_factor),
chem_mdl <- chem %>%
  mutate(
    `NH4-N_ppb` = if_else(is.na(`NH4-N_ppb`), 0.02, `NH4-N_ppb`),
    `PO4-P_ppb` = if_else(is.na(`PO4-P_ppb`), 0.02, `PO4-P_ppb`),
    `NO3-N_ppb` = if_else(is.na(`NO3-N_ppb`), 0.02, `NO3-N_ppb`),
  )

write_csv(chem_mdl, "synoptic/data/Solubles w MDLs_PR synoptic.csv")
