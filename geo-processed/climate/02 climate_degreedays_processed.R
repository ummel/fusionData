library(sf)
library(tidyverse)

#--------

# Function to return CDD and HDD by climate division for individual years
# See here for raw data: https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/
getDD <- function(year) {
  url <- "https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/YEAR/ClimateDivisions.TYPE.txt"
  url <- sub("YEAR", year, url)
  out <- list()
  for (i in c("Cooling", "Heating")) {
    d <- read.table(sub("TYPE", i, url), skip = 3, sep = "|", header = TRUE)
    out[[i]] <- tibble(climate_division = str_pad(d$Region, width = 4, pad = 0),
                       year = as.character(year),
                       value = rowSums(d[-1L]))
  }
  result <- full_join(out$Cooling, out$Heating, by = c("climate_division", "year"))
  names(result) <- c("climate_division", "year", "cdd", "hdd")
  return(result)
}

# Process all years (see here: https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/)
# Data actually go back to 1981, but starting at 1990 for consistency with AK and HI data
years <- 1990:2020
lower48 <- lapply(years, getDD)

#--------

# Lower 48 CDD Climate Normals (1981-2010)
d <- read.table("https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/climatology/1981-2010/ClimateDivisions.Cooling.txt", skip = 3, sep = "|", header = TRUE)
cdd <- tibble(climate_division = str_pad(d$Region, width = 4, pad = 0),
              year = "1981-2010",
              cdd = rowSums(d[-1L]))

# Lower 48 HDD Climate Normals (1981-2010)
d <- read.table("https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/climatology/1981-2010/ClimateDivisions.Heating.txt", skip = 3, sep = "|", header = TRUE)
hdd <- tibble(climate_division = str_pad(d$Region, width = 4, pad = 0),
              year = "1981-2010",
              hdd = rowSums(d[-1L]))

normals <- full_join(cdd, hdd, by = c("climate_division", "year"))

#--------

# Get DD's for Hawaii and Alaska

# Manual entry for Anchorage and Honolulu weather stations
# Data source: https://portfoliomanager.energystar.gov/pm/degreeDaysCalculator
# Data available 1990 through present

# Anchorage weather station: 702735, MERRILL FLD (search zip code 99504)
# Use arbitrary climate division code "4900" for Alaska
ak <- tibble(climate_division = "4900",
             year = as.character(1990:2020),
             cdd = c(7, 2, 3, 40, 13, 8, 4, 33, 1, 22, 1, 11, 13, 37, 56, 11, 0, 6, 0, 11, 1, 0, 0, 62, 6, 39, 27, 1, 20, 114, 10),
             hdd = c(10577, 10048, 10453, 9011, 9917, 9812, 10950, 9400, 10150, 10821, 9412, 9644, 9234, 9420, 9577, 9341, 10532, 10126, 10757, 10090, 9805, 9684, 11082, 9621, 8999, 9088, 8585, 10537, 9286, 8733, 10378)) %>%
  add_row(climate_division = "4900", year = "1981-2010", cdd = mean(.$cdd), hdd = mean(.$hdd)) # Compute approximate climate normals (call it 1981-2010 for consistency with other data)

# Honoluly weather station: 911820, HONOLULU INTL (search zip code 96817)
# Use arbitrary climate division code "5000" for Hawaii
hi <- tibble(climate_division = "5000",
             year = as.character(1990:2020),
             cdd = c(4267, 4351, 4381, 4116, 4672, 4907, 4685, 4351, 4079, 4060, 4318, 4532, 4460, 4710, 4826, 4669, 4204, 4483, 4597, 4502, 4290, 4606, 4165, 4188, 4601, 4760, 4459, 4603, 4802, 5048, 4845),
             hdd = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) %>%
  add_row(climate_division = "5000", year = "1981-2010", cdd = mean(.$cdd), hdd = mean(.$hdd)) # Compute approximate climate normals (call it 1981-2010 for consistency with other data)

#--------

# Combine all degree day data, by climate division
dd <- bind_rows(lower48, normals, ak, hi) %>%
  rename(vintage = year,
         `Cooling degree days (base 65F)` = cdd,
         `Heating degree days (base 65F)` = hdd) %>%
  mutate_if(is.double, ~ as.integer(round(.x))) %>%
  arrange(climate_division, vintage)

# Extract degree days normals only (1981-2010)
dd.normals <- dd %>%
  filter(!nchar(vintage) == 4) %>%
  mutate(vintage = "always") %>%  # Special 'vintage' value indicating it can always be used
  rename(`Cooling degree days 1981-2010 (base 65F)` = `Cooling degree days (base 65F)`,
         `Heating degree days 1981-2010 (base 65F)` = `Heating degree days (base 65F)`)

# Save processed annual degree days data to disk
saveRDS(filter(dd, nchar(vintage) == 4), "geo-processed/climate/climate_degreedays_annual_processed.rds")

# Save processed degree days normals to disk
saveRDS(dd.normals, "geo-processed/climate/climate_degreedays_normals_processed.rds")
