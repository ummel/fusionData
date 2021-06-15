# Process the UCC-PCE concordance prepared by BLS (pce-concordance-20XX.xlsx)
# Output: "pce_concordance.RData": tibble linking individual UCC's to a PCE
# series along with any allocation factor (ucc_share) provided in the BLS concordance file
library(tidyverse)

data(BEA_pce_national)

#---

# Import concordance mapping of CE UCC and PCE series
d <- readxl::read_excel("survey-raw/CEX/pce-concordance-2017.xlsx", trim_ws = TRUE, skip = 3,
                        col_names = c("ucc", "source", "ucc_desc", "pce_series", "pce_desc", "note")) %>%
  select(-ucc_desc) %>%
  mutate_if(is.character, ~ str_squish(gsub("\r\n", "", ., fixed = TRUE)))  # Remove line breaks from text fields

#---

# Apparent spelling errors
# "DERERX" should be "DERERC" (Repair of household appliances)
# "DAFTC" should be "DAFTRC" (Passenger fares for foreign travel)
d <- d %>%
  mutate(pce_series = ifelse(pce_series == "DERERX", "DERERC", pce_series),
         pce_series = ifelse(pce_series == "DAFTC", "DAFTRC", pce_series))

# FLAG rows in in input data with an invalid 'pce_series'
flagged <- d %>%
  filter(!pce_series %in% BEA_pce_national$pce_series)

# Alternative check that the pce_series and pce_desc fields in 'd' are consistent with official records
#  i.e. flag any potential mismatch between code and description
# The "flagged" output must be assessed manually; any desired changes are hard-coded below as manual fixes
# x <- adist(d$pce_desc, BEA_pce_national$pce_desc)
# i <- apply(x, 1, which.min)
# flagged <- d %>%
#   mutate(possible_series = BEA_pce_national$pce_series[i]) %>%
#   filter(pce_series != possible_series)

#---

# APPLY MANUAL FIXES

# UCC "420115" is present as both D and I sources. Decided to drop the I source entry (based on record in "ucc_source"). They are assigned to different PCE series.
i <- which(d$ucc == "420115" & d$source == "I")
d <- d[-i, ]

# Reassign child PCE series of "DADMRC" to the parent
# The children don't appear to align nicely with UCC's and there is some confusion/possible-error in the concordance itself
# Also, the greater resolution of children not necessary in this case
# The more complicated calculation of 'ind' is because UCC 620212 is linked to multiple PCE's in the concordance
ind <- apply(sapply(c("DMOVRC", "DLIGRC", "DSPERC"), function(v) grepl(v, d$pce_series, fixed = TRUE)), 1, any)
d <- d %>% mutate(pce_series = ifelse(ind, "DADMRC", pce_series))

# Reassign child PCE series of "DTCSRC" to the parent
# The concordance does not cleanly identify all of the children for this PCE
# Also, the greater resolution of children not necessary in this case
ind <- apply(sapply(c("DLOCRC", "DLDTRC", "DCELRC"), function(v) grepl(v, d$pce_series, fixed = TRUE)), 1, any)
d <- d %>% mutate(pce_series = ifelse(ind, "DTCSRC", pce_series))

# Adjustment for car/truck leasing UCC's
# CEX does not currently distinguish between car and truck lease payments (as PCE does)
# Consequently, I set car and truck lease payment PCE series ("DALERC" and "DTLERC") to the parent series "Motor vehicle leasing" ("DMVLRC")
# The constructed "allocated" leasing variables are removed from concordance (those with non-zero 3rd digit in UCC code)
ind <- apply(sapply(c("DALERC", "DTLERC"), function(v) grepl(v, d$pce_series, fixed = TRUE)), 1, any)
d <- d %>%
  mutate(pce_series = ifelse(ind, "DMVLRC", pce_series)) %>%
  filter(!(pce_series == "DMVLRC" & str_sub(ucc, 3, 3) != "0"))

# Manually ADDING a potential concordance that is not in the BLS concordance file:
# It Looks to me like UCC 900002 ("Occupational expenses, such as union dues
# or professional licenses") is comparable to combination of PCE's DUNSRC and
# DAXSRC. I manually add this concordance to the "pce_concordance" table,
# assuming that 56% is assignable to DUNSRC and 44% to DAXSRC.
d <- d %>%
  add_row(ucc = "900002", source = "I", pce_series = "DUNSRC", pce_desc = filter(BEA_pce_national, pce_series == "DUNSRC")$pce_desc, note = c("ADDED MANUALLY! UCC 900002 contains expenditures that can be assigned to DUNSRC and DAXSRC. Based on 2018 PCE of these two series, the percentage allocated to DUNSRC is estimated at .56.")) %>%
  add_row(ucc = "900002", source = "I", pce_series = "DAXSRC", pce_desc = filter(BEA_pce_national, pce_series == "DAXSRC")$pce_desc, note = c("ADDED MANUALLY! UCC 900002 contains expenditures that can be assigned to DUNSRC and DAXSRC. Based on 2018 PCE of these two series, the percentage allocated to DAXSRC is estimated at .44."))

#---

# EXTRACT UCC-SPECIFIC ALLOCATION FACTORS These are factors (0-1) provided in
# the BLS concordance to approximate the share of a UCC that is assignable to a
# specific PCE series, in cases where the UCC could be considered part of
# multiple series

# Helper function for processing concordance UCC codes
# This sets the 3rd digit of UCC to "0"; useful in cases where the concordance contains an artificial/constructed UCC
extractUCC <- function(x) {
  for (i in 1:length(x)) str_sub(x[i], 3, 3) <- "0"
  return(x)
}

# Helper function for processing concordance "note" column
# Extracts the allocation share (if present) from the concordance "note" column
extractShare <- function(x) {
  out <- x %>%
    str_split(" is estimated at ") %>%
    map_chr(~.x[2]) %>%
    str_replace_all(fixed("."), "") %>%
    str_pad(width = 3, side = "right", pad = "0") %>%
    as.numeric()
  out / 1000
}

# Process "assigned" codes and assign official/preferred "source" variable
d <- d %>%
  mutate(ucc = ifelse(grepl(" is estimated at ", note, fixed = TRUE), extractUCC(ucc), ucc),
         ucc_share = ifelse(grepl(" is estimated at ", note, fixed = TRUE), extractShare(note), 1))

# Safety check: 'ucc_share' values should be (0, 1]
stopifnot(all(d$ucc_share <= 1 & d$ucc_share > 0))

#---

# Assemble final output
pce_concordance <- d %>%
  select(ucc, pce_series, ucc_share) %>%  # Retain only strictly unnecessary columns
  arrange(ucc)

# Save output to disk
save(pce_concordance, file = "survey-processed/CEX/pce_concordance.RData", compress = TRUE)
