

# Round datetime columns to nearest 10 minutes
cond <- cond %>%
  mutate(datetime_round = round_date(ymd_hms(datetime), "10 minutes"))

do <- do %>%
  mutate(datetime_round = round_date(ymd_hms(datetime), "10 minutes"))

# Merge by site and rounded datetime
merged_data <- full_join(do, cond,
                         by = c("site", "datetime_round"),
                         suffix = c("_do", "_cond"))

# Optional: reorder columns and rename rounded datetime
merged_data <- merged_data %>%
  rename(datetime = datetime_round) %>%
  select(datetime, site, everything())

# Save to CSV
write_csv(merged_data, "sensors/data/DO_COND_merged_nearest10min.csv")
