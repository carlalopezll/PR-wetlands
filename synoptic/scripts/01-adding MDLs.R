# adding minimum detection limits

ic <- read_csv("synoptic/data/IC synoptic data.csv")
ic$Date_corrected <- as.Date(ic$Sample_date, format = '%m/%d/%Y')

chem <- read_csv("synoptic/data/Solubles_PR synoptic.csv")
chem$Date_corrected <- as.Date(chem$Date, format = '%m/%d/%Y')


# Adding values of half the minimum detection limit to below detects
ic_mdl <- ic %>%
  mutate(
    `F` = if_else(is.na(`F`), 0.06375/2, `F`),
    Cl = if_else(is.na(Cl), 0.13233/2, Cl),
    Br = if_else(is.na(Br), 0.12794/2, Br),
    `NO3-N` = if_else(is.na(`NO3-N`) | `NO3-N` < 0, 0.04767/2, `NO3-N`),
    SO4 = if_else(is.na(SO4), 0.19379/2, SO4),
    Na = if_else(is.na(Na), 0.17/2, Na),
    `NH4-N` = if_else(is.na(`NH4-N`), 0.08/2, `NH4-N`),
    K = if_else(is.na(K), 0.05/2, K),
    Mg = if_else(is.na(Mg), 0.25/2, Mg),
    Ca = if_else(is.na(Ca), 0.87/2, Ca),
  )

write_csv(ic_mdl, "synoptic/data/IC w MDLs_PR synoptic.csv")

# there are no NAs in the chem data, should I just do this for the negative values?
chem_mdl <- chem %>%
  mutate(
    `NH4-N_ppb` = if_else(is.na(`NH4-N_ppb`), 0.02, `NH4-N_ppb`),
    `PO4-P_ppb` = if_else(is.na(`PO4-P_ppb`), 0.02, `PO4-P_ppb`),
    `NO3-N_ppb` = if_else(is.na(`NO3-N_ppb`), 0.02, `NO3-N_ppb`),
  )

write_csv(chem_mdl, "synoptic/data/Solubles w MDLs_PR synoptic.csv")
