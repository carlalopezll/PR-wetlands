# Palmas metabolism


##################################################
# Estimate metabolism for ponds after loading model fxns
# Code from Hotchkiss & Hall 2014 via van de Bogert et al 2007
# As in Jonsson et al. 2015; Hamdan et al. 2018; Hamdan et al. 2021 
# Updates: 20240612 by ERH to include BP data (not calc from elev)
# 20250623 modified for Puerto Rico wetlands
##################################################

############################################
### [1] NOTES ABOUT THIS FILE --  
############################################

# LOAD Packages AND FUNCTIONS IN "LOAD" R script first

### / END notes ###

############################################
### [2] LOAD fixed parameters for model runs
############################################

# if all model runs have the same # of runs, cuts, and scale, can define here ---
# NOTE: *** Don't just run all dates blindly without checking on early results. *** You'll need to change these parameters and re-run if accept, sig, burn-in time are not high-quality
# see R info about "metrop" function to learn more about this

# Total number of model runs per date to get best estimate of GPP, ER
# *** should be 10,000 to 50,000 after -ncut for final runs for published paper

##################################################
## Tortugero
##################################################

# load libraries
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)

nbatch=56000 

# This is the "burn-in" period removed before calculating GPP, ER
# GPP = GPP for all nbatch - GPP for ncut (& same for ER)
# *** check that ncut removes whole "burn-in" period
ncut=1000 

# Target is ~0.2 acceptance rate (20%)
# *** IF NEEDED: Increase scale to get lower accept. Decrease scale to increase accept.
scale=0.2

# Assign starting values for three unknowns (MET) in lakeMET function: GPP, ER, and sigma (related to model error)
# *** check if different starting values produce different GPP, ER
initial=c(1,-1,1) 

# SITE-SPECIFIC PARAMETERS 
# *** Change Wetland ID & vary zmix, k600, bp with date or month as best we can ***
# can re-assign parms within each date's run code below depending on how you add dates/wetlands!

Wetland <- "PA"
ts <- 0.0069444 #(unit = day; 10 min time interval of O2 data)
zmix <- 0.1 #(unit = m; this is the depth of the water seen by sensor)
# zmix can be fixed by site or vary daily - code allows daily var below
k600 <- 0.01 # m/h - **also run all days for k600 = 0.04 from wind model**
bp <- (101.453*7.50062) #101.453 kPa on 15 July converted to mmHg (where 1 kPa = 7.50062 mmHg)

### / END fixed(ish) parameters ###

########################################################################################

############################################
### [3] SET working drive & LOAD merged data
############################################

# Load data

met <- read_csv("sensors/data/DO/Summer 2024/Palmas/Palmas_summer2024.csv", col_types = cols(Timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# should probably just do rename columns
met$oxy <- met$DO_mgL
met$temp <- met$Temp_C

# fix datetime

met$dtime <- met$Timestamp

met$dtime <- as.POSIXct(met$dtime, format = "%Y-%m-%d %H:%M:%S")

## estimate light using lightest function in LOAD file
# Set coordinates **** NEED TO UPDATE FOR EACH SITE
# 18.468792, -66.461647 (Tortuguero)

met$light<- lightest(time=met$dtime)

# check that modeled light makes sense
plot(met$dtime, met$light)

# fixed "zmix" as a single depth for now; can vary daily with data input
# will also want to update LOAD and this file once we have time-varying k600

# plot oxygen data
plot(met$dtime, met$DO_mgL)

# plot temp data
plot(met$dtime, met$Temp_C)

# plot Kcor estimate (m/h) from fixed k600 (m/h) & varying temp
plot(met$dtime, Kcor(met$Temp_C, k600))

### / END working data section ###

# for running a day at a time
# identify subset of data for this model run from 00:00 to 02:00 the next day

# data <- met %>% 
#   filter(dtime > "2024-07-29 00:00:00" & dtime < "2024-07-30 00:02:00")
# 
# plot(data$dtime, data$oxy)
# plot(data$dtime, data$temp)
# plot(data$dtime, data$light) # re-check against met data; leave for now
# 
# # RUN metrop function to solve for GPP, ER
# # met.out will then give you the summary output for that pond
# 
# met.out <- metrop(lakeMET,initial=initial,nbatch=nbatch,scale=scale,oxy=data$oxy,zmix=zmix,temp=data$temp,light=data$light,ts=ts,bp=bp,k600=k600)
# 
# # To save this output, write to dataframe
# # this called the mout function from the LOAD file
# m.out <- mout(data, met.out)
# # save correct column names
# Colnames <- names(Excel)
# # combine with any previous output so it's all in one place
# Excel=rbind(Excel,m.out)
# # replace with correct column names
# names(Excel) <- Colnames
# # check that this worked!
# Excel

#### For all dates ####

# Get list of unique dates
unique_dates <- unique(as.Date(met$Timestamp))

# unique_dates <- as.Date(c("2024-07-25", "2024-07-26", "2024-07-30"))

# Initialize a list to store outputs
met_results <- list()

# Loop over each unique date
for (i in unique_dates) {
  # Subset data for that specific date
  data_day <- met[as.Date(met$Timestamp) == i, ]
  
  # Skip if there's not enough data
  if (nrow(data_day) == 0) next
  
  # Run model using daily data
  met.out_PA <- metrop(
    lakeMET,
    initial = initial,
    nbatch = nbatch,
    scale = scale,
    oxy = data_day$oxy,
    zmix = zmix,
    temp = data_day$temp,
    light = data_day$light,
    ts = ts,
    bp = bp,
    k600 = k600
  )
  
  # Set global Wetland variable if needed by mout()
  Wetland <- "PA"
  
  # Save model output
  m.out <- mout(data_day, met.out)
  
  Colnames <- names(Excel)
  
  # Bind to Excel (convert to data frame first to ensure row structure)
  Excel <- rbind(Excel, m.out)
  
  names(Excel) <- Colnames
}

Excel$Date <- as.Date(Excel$Date)

Excel$GPP.50 <- as.numeric(Excel$GPP.50)
Excel$ER.50 <- as.numeric(Excel$ER.50)
Excel$NEPcalc <- as.numeric(Excel$NEPcalc)

Excel <- Excel %>%
  filter(GPP.50 >0) %>%
  filter(Date > "2024-07-23 00:00:00" & Date < "2024-08-12 00:02:00")


write.csv(Excel, "PA_met_model_output.csv", row.names = FALSE)

a <- ggplot(Excel, aes(x=Date, y=GPP.50)) +
  geom_point() +
  geom_hline(yintercept = 0)

b <- ggplot(Excel, aes(x=Date, y=ER.50)) +
  geom_point() +
  geom_hline(yintercept = 0)

c <- ggplot(Excel, aes(x=Date, y=NEPcalc)) +
  geom_point() +
  geom_hline(yintercept = 0)

cowplot::plot_grid(a, b, c, ncol = 1)

ggsave("sensors/Palmas metabolism metabolism.jpg")
