# This script generates:
# CEI_2015-2019_2019_calib.gz

#-----

library(fusionData)
source("fusion/CEI/2015-2019/2019/post/calibrateCEIimplicate.R")

# Use data.table multithreading when possible (only possible if NOT using mclapply)
setDTthreads(3)

# Documented consumption categories to be included in final output
load("data/token.rda")
googlesheets4::gs4_auth(token = token)
doc.cats <- googlesheets4::read_sheet("1oECoLziaJcGQV21Wy26XIKO5Yn_fYth9C3xTRLBRV_Y")

#-----

# Create indices in *_fused.fst associated with each implicate (for subsetting)
M <- read_fst("fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_fused.fst", columns = "M")[[1]]
N <- length(M) / max(M)
M <- seq(from = 1, to = length(M) - N + 1, by = N)
M <- cbind(M, M + N - 1)

#-----

# Calibrate each implicate and append result to 'CEI_2015-2019_2019_calib.gz'
for (i in 1:nrow(M)) {
  cat("<< Calibrating implicate", i, "of", nrow(M), ">>\n")
  out <- calibrateCEIimplicate(sim = read_fst("fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_fused.fst", from = M[i, 1], to = M[i, 2]),
                               output_cats = doc.cats$cat,
                               delta_stop = 0.01,
                               iter_max = 10,
                               dir = "fusion/CEI/2015-2019/2019/post")
  out$M <- rep.int(i, nrow(out))
  setcolorder(out, "M")
  data.table::fwrite(out, file = "fusion/CEI/2015-2019/2019/post/CEI_2015-2019_2019_calib.csv.gz",
                     append = i > 1,
                     nThread = 4,
                     showProgress = TRUE)
}
