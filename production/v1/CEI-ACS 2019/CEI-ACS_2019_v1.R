# Produce 'CEI-ACS_2019_v1.fst' file using existing single implicate of calibrated CEI-ACS data on-disk

# Load consumption categories from Google Drive
# https://docs.google.com/spreadsheets/d/1UDn8hGzDMgmPnX1E5D_FDk5n1UvUD6Vx5AJ5ix-8v3M
gs.id <- "13GRKkVZXapHtP7oK1WUh0Yu7OQ_9icd17wUGuhX-WRg"
cats <- googlesheets4::read_sheet(ss = gs.id, sheet = "Category Summary")

# Remove "Other" category from 'cats'
cats <- filter(cats, major != "Other")

# Calibrated CEI-ACS 2019 microdata
cal <- fst::read_fst("production/calibration/CEI_2015-2019_calibrated.fst") %>%
  #mutate(acs_2019_hid = as.integer(gsub(pattern = "2019HU", "", as.character(acs_2019_hid), fixed = TRUE))) %>%
  select(all_of(c(tolower(cats$cat))))

fst::write_fst(cal, "production/v1/CEI-ACS 2019/CEI-ACS_2019_v1.fst", compress = 100)
