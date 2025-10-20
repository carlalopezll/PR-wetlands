library(openxlsx)
library(dplyr)
library(tidyr)

# 1️⃣ Prepare wide table
all_tables_wide <- all_tables %>%
  dplyr::select(site, season, metric, label) %>%
  unite("site_season", site, season, sep = "_") %>%
  pivot_wider(
    names_from = site_season,
    values_from = label
  )

# Optional: reorder metrics nicely
metric_order <- c(
  "do_mgL_mean", "amp_do",
  "cond_uS_cm_mean", "amp_cond",
  "temp_C_mean", "amp_temp"
)
all_tables_wide <- all_tables_wide %>%
  mutate(metric = factor(metric, levels = metric_order)) %>%
  arrange(metric)

writexl::write_xlsx(all_tables_wide, "incubations/sensor emmeans.xlsx")

# 2️⃣ Create workbook
wb <- createWorkbook()

addWorksheet(wb, "Summary")

# 3️⃣ Write the table starting from row 3 (leave top 2 rows for multi-row headers)
writeData(wb, "Summary", all_tables_wide, startRow = 3, colNames = TRUE)

# 4️⃣ Add multi-row headers manually
# Row 1: Sites
writeData(wb, "Summary", c("", "Palmas", "Palmas", "Tortuguero", "Tortuguero"), startRow = 1, startCol = 1)
mergeCells(wb, "Summary", cols = 2:3, rows = 1)
mergeCells(wb, "Summary", cols = 4:5, rows = 1)

# Row 2: Seasons
writeData(wb, "Summary", c("", "Summer", "Winter", "Summer", "Winter"), startRow = 2, startCol = 1)

# 5️⃣ Save workbook
saveWorkbook(wb, "incubations/wetland_summary_with_tukey.xlsx", overwrite = TRUE)
