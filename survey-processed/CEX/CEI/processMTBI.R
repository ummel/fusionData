# library(labelled)
# library(tidyverse)
# source("R/utils.R")

#-----

processMTBI <- function(survey_years) {

  data(cpi_series)
  data(ucc_assignment)

  # Load categories data, sot that only the necessary UCC's are loaded from MTBI files on disk
  cats <- ucc_assignment %>%
    filter(file == "MTBI")

  #---

  readMTBI <- function(survey_year) {

    # Get files paths to process
    fpath <- normalizePath(list.files(path = paste0("survey-raw/CEX/", survey_year), pattern = glob2rx(pattern = "mtbi*.csv"), recursive = TRUE, full.names = TRUE))
    fpath <- fpath[!grepl("x.csv", fpath, fixed = TRUE)]  # Remove the "YRQx.csv" files, when applicable

    # Read selected variables from file
    select.vars <- c("EXPNAME", "COST", "COST_", "UCC")
    select.vector <- rep("character", length(select.vars) + 3)
    names(select.vector) <- c("NEWID", "REF_MO", "REF_YR", select.vars)

    # Unique CU/calendar month/UCC combinations in output are identified by file-cuid-intnum-month
    # Column 'file' is an integer identifying the file in 'fpath' from which the data was obtained
    # !!! NOTE: MTBI does not appear to contain any COST_ flags that are "A", "B", or "C" (i.e. blanks); the only flag values are for valid entries or topcoded
    d <- fpath %>%
      map_dfr(data.table::fread, .id = "file", select = select.vector, na.strings = "", data.table = FALSE) %>%
      rename(expname = EXPNAME, ucc = UCC, cost = COST) %>%
      filter(ucc %in% cats$ucc) %>%  # Restrict to UCC's we are interested in...
      mutate(cuid = as.integer(str_sub(NEWID, 1, -2)),
             intnum = as.integer(str_sub(NEWID, -1, -1)),
             month = as.integer(REF_MO),
             year = as.integer(REF_YR),
             cost = as.numeric(cost),
             coded = COST_ %in% c("T", "U", "V", "W")) %>%  # These are bot/topcode flag values (there are no regular NA's in the MTBI data)
      group_by(cuid, intnum) %>%
      #mutate(month = dense_rank(paste0(REF_YR, REF_MO))) %>%  # Assigns 'month' as 1, 2, or 3 for each CU interview (i.e. NEWID; CUID/INTNUM); the calendar months in the lookback period (REF_MO) can be different for each CU
      #id = 1:n()) %>%  # A unique transaction/entry/row ID for each CUID-INTNUM
      ungroup() %>%
      select(file, cuid, intnum, month, year, ucc, expname, coded, cost)

    return(d)

  }

  #-----

  # Load expenditure data across multiple survey years
  d <- survey_years %>%
    map_dfr(readMTBI, .id = "survey_year") %>%
    mutate(survey_year = survey_years[as.integer(survey_year)])  # Replace index value with actual survey year

  #-----

  # NOTE: Variable 'expname' is "Name of expense variable from which UCC mapped" (UCC = "Universal Classification Code")
  # i.e. The relationship between 'ucc' and 'expname' is potentially many-to-many
  # About 60% of 'expname' values map to a single 'ucc'
  # About 80% of 'ucc' values map to a single 'expname'
  # The UCC value is what is ultimately used to map to a category (via 'cats')

  #-----

  # Load CPI data and create 'cpi' variable to inflate dollar values to final survey year
  cpi.adj <- cpi_series %>%
    mutate(base = mean(filter(., year == max(survey_years))$cpi),
           cpi = base / cpi) %>%
    select(year, month, cpi)

  #-----

  # Aggregate expenditure data by primary consumption category ('cat')
  # The use of dense_rank() ensures that the expenditure columns are sorted by the sequence of available interviews
  # Example: If a 'cuid' has only their final interview in 'd' (intnum = 5), intnum is reassigned to '1' by dense_rank()
  d <- d %>%
    left_join(cpi.adj, by = c("year", "month")) %>%
    mutate(cost = cost * cpi) %>%   # Adjust for inflation
    left_join(cats %>% select(ucc, cat, ucc_share), by = "ucc") %>%
    mutate(cost = cost * ucc_share) %>%  # Multiply raw/original cost by 'ucc_share' to allocate (or adjust) as necessary
    group_by(cuid) %>%
    mutate(intrank = dense_rank(intnum)) %>%  # Add dense-ranked version of 'intnum'
    group_by(cuid, intnum) %>%
    mutate(emonth = weighted.mean(year + month / 12, pmax(0, cost))) %>%  # Assign single reference time to each cuid-intnum combination
    group_by(cuid, intnum, intrank, emonth, cat) %>%
    summarize(cost = sum(cost), .groups = 'drop')   # Aggregate expenditures at CU-level in preparation for making data wide (below)

  # Safety check
  stopifnot(!anyNA(d$cat))

  #-----

  # Spread the expenditure data wide
  exp.intrank <- d %>%
    complete(nesting(cuid, intnum, intrank, emonth), cat, fill = list(cost = 0)) %>%
    pivot_wider(id_cols = cuid,
                names_from = c(cat, intrank), names_sep = "_",
                values_from = cost, values_fill = list(cost = NA))

  # See how many NA's by column
  # There should be none for *_1 and increasing amounts up to *_4
  #colSums(is.na(exp.intrank))
  stopifnot(!is.unsorted(colSums(is.na(exp.intrank))))

  #-----

  # Create 'year.intrank' and 'month.intrank' df's
  # These provide cuid-specified 'year_i' and 'month_i' columns providing time stamps for each expenditure period
  temp <- d %>%
    select(-cat, -cost, -intnum) %>%
    distinct() %>%
    complete(cuid, intrank, fill = list(emonth = NA)) %>%
    group_by(cuid) %>%
    mutate(emonth = replace(emonth, is.na(emonth), max(emonth, na.rm = TRUE) + 0.25 * 1:sum(is.na(emonth)))) %>%
    ungroup() %>%
    mutate(year = as.integer(emonth),
           month = factor(as.integer(1 + floor(12 * (emonth - year))), levels = 1:12, ordered = TRUE),
           year = factor(year, levels = sort(unique(year)), ordered = TRUE))

  year.intrank <- pivot_wider(temp, id_cols = cuid, names_from = intrank, names_prefix = "year_", values_from = year)
  month.intrank <- pivot_wider(temp, id_cols = cuid, names_from = intrank, names_prefix = "month_", values_from = month)

  #-----

  # Assemble CU-level expenditures formatted for imputation
  expend <- year.intrank %>%
    inner_join(month.intrank, by = "cuid") %>%
    inner_join(exp.intrank, by = "cuid")

  return(expend)

}

#------------------------
#------------------------
# DEPRECATED
#------------------------
#------------------------

# # Determine which UCC-EXPNAME combinations contain ONLY negative values
# ucc.expname.neg <- d %>%
#   group_by(ucc, expname) %>%
#   summarize(neg = all(cost < 0), .groups = 'drop')
#
# # Identify the UCC-EXPNAME combinations that are mortgage/principal related
# # These are entries with negative values that should be "flipped" to positive values
# # The other negative line items are reimbursement-related and should remain negative
# to.flip <- ucc.expname.neg %>%
#   left_join(var_info, by = c("expname" = "var")) %>%
#   mutate(flip = neg & (expname == "QLMPSUMX" | grepl("principal paid", tolower(var_desc)))) %>%
#   select(ucc, expname, flip) %>%
#   distinct()
#
# # Safety check
# stopifnot(nrow(to.flip) == nrow(ucc.expname.neg))
#
# # "Flip" the negative UCC-EXPNAME combinations so they are strictly positive (will re-flip after imputation)
# d <- d %>%
#   left_join(to.flip, by = c("ucc", "expname")) %>%
#   mutate(cost = ifelse(flip, -cost, cost),
#          topcoded = coded & cost > 0,
#          botcoded = coded & cost < 0) %>%
#   select(-flip, -coded)
#
# #----------------
#
# # Observed critical values
# # Only calculated if the number of non-coded (i.e. legit) values is at least 30
# tcrit <- d %>%
#   group_by(ucc, expname) %>%
#   summarize(nlegit = sum(!topcoded & !botcoded),
#             tcrit_top = ifelse(sum(topcoded) <= 1 | nlegit < 30, NA, max(cost[cost < min(cost[topcoded])])),
#             tcrit_bot = ifelse(sum(botcoded) <= 1 | nlegit < 30, NA, min(cost[cost > max(cost[botcoded])])),
#             .groups = 'drop') %>%
#   select(-nlegit)
#
# # Overwrite original 'topcoded' and 'botcoded' variables so they are consistent with 'tcrit' above
# # In effect, this takes the maximum observed value of an (officially) non-coded entry as the legitimate critical value and reassign topcode/botcode status on this basis
# # Also aggregate cost at cuid-intnum-month-ucc-expname to create single cost entry for each, then reassign topcode/botcode flags
# d <- d %>%
#   group_by(cuid, intnum, month, ucc, expname) %>%
#   summarize(cost = sum(cost), .groups = 'drop') %>%
#   left_join(tcrit, by = c("ucc", "expname")) %>%
#   mutate(topcoded = ifelse(is.na(tcrit_top), FALSE, cost > tcrit_top),
#          botcoded = ifelse(is.na(tcrit_bot), FALSE, cost < tcrit_bot))
#
# # Calculate the mean topcode/botcode values (i.e. mean value of observations above the critical value)
# tmean <- d %>%
#   group_by(ucc, expname) %>%
#   summarize(tmean_top = ifelse(sum(topcoded) == 0, NA, mean(cost[topcoded])),
#             tmean_bot = ifelse(sum(botcoded) == 0, NA, mean(cost[botcoded])),
#             .groups = 'drop')
#
# # Assemble 'tcodes' data frame with critical and mean values for each ucc-expname combination
# tcodes <- tcrit %>%
#   left_join(tmean, by = c("ucc", "expname")) %>%
#   filter(is.finite(tmean_top) | is.finite(tmean_bot)) %>%
#   unite("var", expname, ucc)
#
# # Put a safety check here
# stopifnot(nrow(filter(tcodes, is.finite(tmean_top), tmean_top <= tcrit_top)) == 0)
# stopifnot(nrow(filter(tcodes, is.finite(tmean_bot), tmean_bot >= tcrit_bot)) == 0)
#
# #----------------
#
# # Make wide? fill with zeros
# d <- d %>%
#   mutate(cost = ifelse(topcoded, Inf, cost),
#          cost = ifelse(botcoded, -Inf, cost)) %>%
#   unite("var", expname, ucc) %>%
#   select(cuid, intnum, month, var, cost) %>%
#   pivot_wider(id_cols = c(cuid, intnum, month), names_from = var, values_from = cost, values_fill = 0)
#
# # Variables with NA values to be estimated prior to bot/topcode imputation
# # Sorted from least to most missing-ness; this is the order used for the estimation process below
# na.count <- sort(map_int(d, ~ sum(is.infinite(.x))))
# vimp <- names(na.count[na.count > 0])
#
# # Safety check
# stopifnot(nrow(tcodes) == length(vimp))
# stopifnot(all(vimp %in% tcodes$var))
#
# # Create a bot/topcode indicator matrix identifying the location of topcode (1) and botcode (-1) entries for the 'vimp' variables
# tcode.indicator <- d[vimp] %>%
#   mutate_all(~ ifelse(is.finite(.x), 0, sign(.x)))
#
# #----------------
#
# # Calculate principal components for the variables that do not require estimation
# # This reduces the dimensionality and speeds up linear model fitting in next step
# # 100 components tends to pick up about 50% of the variance (good enough) (.rank = 100)
# # Use a 20% random sample of the input data to estimate the PC's (slow with full dataset)
# pca.in <- d %>%
#   select(-cuid, -intnum, -month, -all_of(vimp)) %>%
#   mutate_all(scale)
# pca.fit <- prcomp(x = slice_sample(pca.in, prop = 0.20), rank. = 100)
#
# # Diagnostic: Plot number of components and cumulative variance
# # See here: https://rstatisticsblog.com/data-science-in-action/data-preprocessing/complete-guide-to-principal-component-analysis-in-r/
# # cum.var <- cumsum(pca.fit$sdev ^ 2 / sum(pca.fit$sdev ^ 2))
# # plot(cum.var, xlab = "Principal Component",
# #      ylab = "Cumulative Proportion of Variance Explained",
# #      type = "l")
# # abline(v = 100, lty = 2)
#
# # New dataset using calcualed PC's
# # Predict the PC's for the full set of observations (pca.in)
# pca.out <- predict(pca.fit, newdata = pca.in)
#
# # Clean up
# rm(pca.fit, pca.in)
# gc()
#
# #----------------
#
# # Sequential imputation of the  linear models to impute the NA top/botcoded observations
#
# # Prepare input data
# mm <- d %>%
#   mutate(intnum = factor(intnum),
#          month = factor(month)) %>%
#   select(intnum, month, all_of(vimp)) %>%
#   cbind(pca.out)
#
# # Create model matrix
# mm <- model.matrix.lm(~., data = mm, na.action = na.pass)
# gc()
#
# message("Making linear predictions for bot/topcoded entries...")
# pb <- txtProgressBar(max = length(vimp), style = 3)
# for (n in 1:length(vimp)) {
#
#   # Indices used to subset rows and columns of the model matrix
#   v <- vimp[n]
#   i <- is.na(mm[, v])  # Rows with missing 'v' (to impute)
#   j <- is.na(colSums(mm[i, , drop = FALSE]))  # Columns with NA in rows requiring imputation (omitted from training)
#   j[v] <- FALSE  # Ensures the response column is included in 'dfit'
#
#   # Training data
#   dfit <- na.omit(mm[!i, !j, drop = FALSE])
#
#   # Index for rows with non-zero response values
#   # These can be excluded with significantly affecting the results (considerably faster, too)
#   k <- dfit[, v] != 0
#
#   # Fast linear model
#   # Method 3 found to be the fastest in testing
#   fit <- RcppEigen::fastLm(X = dfit[k, setdiff(colnames(dfit), v)], y = dfit[k, v], method = 3)
#
#   # Update missing values for 'v'
#   # See predict.fastLM() here: https://rdrr.io/cran/RcppArmadillo/src/R/fastLm.R
#   z <- coef(fit)
#   z <- z[!is.na(z)]  # Remove any NA coefficient entries (for safety)
#   mm[i, v] <- as.vector(mm[i, names(z), drop = FALSE] %*% z)
#
#   # Update progress bar
#   setTxtProgressBar(pb, n)
#
# }
#
# # Update original values
# d[vimp] <- mm[, vimp]
#
# # Safety check
# stopifnot(anyNA(d))
#
# # Cleanup
# rm(mm, pca.out)
# gc()
#
# #----------------
#
# # !!! FROM HERE: !!!
#
# # Use the model estimates to impute the bot/topcoded entries
# # Location of bot/topcoded entries given by 'tcode.indicator' matrix
# # Mean values for the bot/topcoded entries given by 'tcodes'
#
# # Impute the bot/topcoded values
# for (i in 1:nrow(tcodes)) {
#   v <- paste(tcodes$expname[i], tcodes$ucc[i], sep = "_")
#   message("\nVariable: ", v, " (", tcodes$type[i], "codes)")
#   imp.out[[v]] <- imputeTopcoded(
#     x = imp.out[[v]],
#     j = is.na(imp.in[[v]]),
#     tmean = tcodes$tmean[i],
#     tcrit = tcodes$tcrit[i],
#     w = imp.in$weight,
#     b = imp.bound[[v]]
#   )
# }
#
#
#
# #----------------
#
#
# #---
#
# # Can see here that topcode values are seemingly all over the place; they can even differ for the same UCC-EXPNAME combination
#
#
#
#
#
# filter(topcoded) %>%
#   group_by(ucc, expname) %>%
#   summarize(tmean_avg = mean(cost),
#             tmean_min = min(cost),
#             tmean_max = max(cost),
#             .groups = 'drop')
#
#
# tmean = ifelse(mean(cost) > tcrit, mean(cost), max(cost)), .groups = 'drop')
#
#
# filter(tcodes, tcrit > tmean)
#
# select(survey_year, ucc, expname, cost, topcoded) %>%
#
#
#   distinct() %>%
#
#
#
#   filter(topcoded, survey_year == 2015) %>%
#   group_by(ucc, expname) %>%
#   mutate(tmean = mean(cost))
#
#
#
#
# #---
#
# # ucc.expname.var <- ucc.expname.neg %>%
# #   mutate(#expname_new = ifelse(ucc %in% tcodes$ucc | neg, expname, "None"),
# #     var = paste(expname_new, ucc, sep = "_")) %>%
# #   select(ucc, expname, neg, var)
#
#
# # Merge 'cat' assignment for UCC's
# test <- d %>%
#   left_join(cats %>% select(ucc, cat, ucc_share), by = "ucc") %>%
#   mutate(cost = cost * ucc_share) %>%
#   group_by(cuid, intnum, cat) %>%
#   summarize(cost = sum(cost),
#             botcoded = cost < 0 & any(botcoded),
#             topcoded = cost >= 0 & any(topcoded))
#
#
# check <- test %>%
#
#
#
#   temp <- d %>%
#   #left_join(tcodes, by = c("ucc", "expname")) %>%
#   #left_join(ucc.expname.var, by = c("ucc", "expname")) %>%
#   #mutate(impute = topcoded & !is.na(tcrit),
#   #       cost = ifelse(impute, tcrit, cost)) %>%  # For topcoded observations that will be imputed, replace cost with critical value
#   group_by(cuid, intnum, month, year, ucc, topcoded, botcoded) %>%
#   summarize(bound = sum(cost),
#             cost = ifelse(any(topcoded | botcoded), NA, sum(cost))) %>%  # Set to NA for observations to be imputed
#   ungroup()
#
#
#
#
#
#
#
# #-------------------------------------------------
# #-------------------------------------------------
# # ORIGINAL FROM BELOW HERE!!!
# #-----
#
# # Determine which ucc-expname combinations are candidates for imputation and their associated topcode and critical values
# # SET MINIMUM THRESHOLDS FOR IMPUTATION TO BE CARRIED OUT:
# #   'topcode_n' is the number of monthly observations that are topcoded (i.e. we can leave unchanged variables with topcode_n below some threshold; see below)
# #   'topvalid_n' is the number of non-coded monthly observations available for distribution-fitting (i.e. only impute variables with topvalid_n above some threshold; see below)
# tcodes <- d %>%
#   group_by(ucc, expname) %>%
#   mutate(topunique = length(unique(cost[topcoded])) == 1 & suppressWarnings(max(cost[topcoded]) == max(cost)),  # Is there a single topcode value?
#          tcrit_top = ifelse(sum(!topcoded) > 0, max(cost[!topcoded]), NA)) %>%   # Upper critical value
#   filter(topunique, is.finite(tcrit_top))  %>%
#   #group_by(cuid, intnum, month, ucc, expname, topunique, tcrit_top) %>%
#   group_by(cuid, intnum, month, year, ucc, expname, topunique, tcrit_top) %>%
#   summarize(topcoded = any(topcoded),
#             cost = sum(cost)) %>%
#   group_by(ucc, expname, tcrit_top) %>%
#   summarize(topcode_n = ifelse(topunique[1], sum(topcoded), NA),  # Number of topcoded entries
#             topvalid_n = ifelse(is.na(topcode_n), NA, length(unique(cost[!topcoded]))),  # Number of unique, non-topcoded cost entries
#             tmean_top = ifelse(topunique[1], mean(cost[topcoded]), NA)) %>%
#   ungroup() %>%
#   filter(is.finite(tmean_top), topcode_n >= 5,  topvalid_n >= 30) %>%  # SET THRESHOLDS HERE!
#   mutate(tcrit_top = ifelse(is.na(tmean_top), NA, tcrit_top)) %>%
#   select(ucc, expname, tcrit_top, tmean_top) %>%
#   # Section below analogous to that added to end of getCodedValues() code
#   pivot_longer(cols = -c(ucc, expname), values_drop_na = TRUE) %>%
#   separate(name, c("name", "type"), sep = "_") %>%
#   pivot_wider(id_cols = c(ucc, expname, type)) %>%
#   filter(abs(tmean) > abs(tcrit))
#
# # Check that there are no botcoded cases
# stopifnot(all(tcodes$type == "top"))
#
# # NOT WORKING: SAFTY CHECK for top/botcode imputation
# # mean.check <- d %>%
# #   left_join(tcodes, by = c("expname", "ucc")) %>%
# #   group_by(expname, ucc) %>%
# #   summarize(mean = mean(cost)) %>%
# #   mutate(var = paste(expname, ucc, sep = "_"))
#
# #---
#
# # Create crosswalk between UCC-EXPNAME combinations and the collapsed category to be used for future aggregation, etc.
# ucc.expname.var <- ucc.expname.neg %>%
#   mutate(expname_new = ifelse(ucc %in% tcodes$ucc | neg, expname, "None"),
#          var = paste(expname_new, ucc, sep = "_")) %>%
#   select(ucc, expname, neg, var)
#
# #----TEMPORARY???
# # ERROR?
# # Unclear why there are different topcode values within the same file...
# # bls.error <- d %>%
# #   filter(ucc == "900002", coded) %>%
# #   distinct(file, REF_YR, REF_MO, cost, .keep_all = TRUE) %>%
# #   arrange(file, REF_YR, REF_MO) %>%
# #   mutate(NEWID = paste0(cuid, intnum)) %>%
# #   select(NEWID, REF_YR, REF_MO, ucc, cost, COST_)
# # View(bls.error)
# # write_csv(bls.error, "~/Desktop/CEX topcoding issue.csv")
#
# # Full extent of issue
# # topcode.check <- d %>%
# #   filter(coded) %>%
# #   distinct(ucc, cost)
#
# # Manual override to correct potential topcode issue (waiting for response from BLS...)
# # This sets all coded values in a given UCC to the max (or min) observed top-code value; i.e. ensures a single, unique value for each UCC
# # WARNINGS ARE OK
# # d <- d %>%
# #   group_by(ucc) %>%
# #   mutate(cost = ifelse(coded & cost > 0, max(cost[coded]), cost),
# #          cost = ifelse(coded & cost < 0, min(cost[coded]), cost)) %>%
# #   ungroup()
#
# # check <- test %>%
# #   filter(coded) %>%
# #   distinct(ucc, cost)
#
# # Include?
# # DROP any observations that return month > 3 (I believe these observations are erroneous and should be reported to BLS)
# # table(d$month)
# # bls.error <- filter(d, month > 3)
# # d <- filter(d, month <= 3)
#
# #-----
#
# # This is an important code chunk for aggregating data prior to making it wide for imputation step
# temp <- d %>%
#   left_join(tcodes, by = c("ucc", "expname")) %>%
#   left_join(ucc.expname.var, by = c("ucc", "expname")) %>%
#   mutate(impute = topcoded & !is.na(tcrit),
#          cost = ifelse(impute, tcrit, cost)) %>%  # For topcoded observations that will be imputed, replace cost with critical value
#   group_by(cuid, intnum, month, year, var) %>%
#   summarize(bound = sum(cost),
#             cost = ifelse(any(impute), NA, sum(cost))) %>%  # Set to NA for observations to be imputed
#   ungroup()
#
# #-----
#
# # Load CU attributes that can serve as potential predictors in topcode imputation step
# cu.post <- readRDS(paste0("data-raw/CEX/", survey_year, "/cu_post_", survey_year, ".rds"))
#
# # cost: either monthly summed cost (when all observations are valid) or NA for cases that require imputation
# # Note that naming the columns by expname_ucc avoids them starting with a numeric, which can create problems (e.g. calling lm)
# imp.in <- temp %>%
#   pivot_wider(id_cols = c(cuid, intnum, month, year), names_from = var, values_from = cost, values_fill = list(cost = 0L)) %>%
#   arrange(cuid, intnum, month, year) %>%
#   inner_join(cu.post, by = c("cuid", "intnum"))
#
# # bound: lower bound on actual cost; used to set minimum constraint for imputed values
# imp.bound <- temp %>%
#   pivot_wider(id_cols = c(cuid, intnum, month, year), names_from = var, values_from = bound, values_fill = list(bound = 0L)) %>%
#   arrange(cuid, intnum, month, year) %>%
#   inner_join(select(cu.post, cuid, intnum), by = c("cuid", "intnum"))
#
# # Variables for which imputation is required
# #rm(temp); gc()
# icount <- colSums(is.na(imp.in))
# V <- names(icount)[icount > 0]
#
# # Missing values in the MTBI are (only) the result of top-coded observations
# stopifnot(length(V) == nrow(tcodes))
#
# # Number of imputations for each 'V'
# #summary(icount[icount > 0])
#
# # Generate matrix to pass to 'predictorMatrix' argument of mice()
# # Call to corMat() IS TOO SLOW, because there are a large number of imputation variables and potential predictors
# # Note: The only geographic variable allowed is 'division', since the sample is representative at that level;
# #       'region' disallowed as it is collinear with 'division'
# message("Preparing for imputation step")
# ignore <- c("cuid", "intnum", "qintrvmo", "qintrvyr", "month", "year", "region", grep("^state", names(imp.in), value = TRUE), grep("^psu", names(imp.in), value = TRUE))
# cmat <- corMat(data = imp.in, yvars = V, wvar = "weight", exclude = ignore, xrank = FALSE, faster = TRUE)
# bpred <- bestPred(cormat = cmat, threshold = 0.5)
# mice.pmat <- matrix(data = 0L, nrow = ncol(imp.in), ncol = ncol(imp.in), dimnames = list(names(imp.in), names(imp.in)))
# for (v in V) mice.pmat[v, bpred[[v]]] <- 1L
#
# # MICE mputation
# # Since the NA's in 'imp.in' are ONLY for topcoded observations (confirm), a linear imputation (method = 'norm.predict')
# #  is suitable to determine approximate rank-ordering of the topcoded observations (which is all that is required)
# message("Imputing top-coded observations")
# imp <- mice::mice(data = imp.in,
#                   m = 1, maxit = 1,  # Only single iteration required when method = 'norm.predict'
#                   predictorMatrix = mice.pmat,
#                   visitSequence = "monotone",
#                   method = "norm.predict")
#
# # Compile full dataset with all imputations
# #imp.out <- mice::complete(imp, action = "long")
# imp.out <- mice::complete(imp, action = 1)
#
# # Check: Any NA values remaining?
# stopifnot(!anyNA(imp.out[V]))
#
# #----------------
#
# # Impute the topcoded values
# for (i in 1:nrow(tcodes)) {
#   v <- paste(tcodes$expname[i], tcodes$ucc[i], sep = "_")
#   message("\nVariable: ", v, " (", tcodes$type[i], "codes)")
#   imp.out[[v]] <- imputeTopcoded(
#     x = imp.out[[v]],
#     j = is.na(imp.in[[v]]),
#     tmean = tcodes$tmean[i],
#     tcrit = tcodes$tcrit[i],
#     w = imp.in$weight,
#     b = imp.bound[[v]]
#   )
# }
#
# # NOT WORKING: Check topcode imputation results: pre- and post-imputation means should be similar
# # message("Pre- and post-imputation means for topcoded variables")
# # x <- colMeans(imp.out[unique(paste(tcodes$expname, tcodes$ucc, sep = "_"))])
# # check <- mean.check %>%
# #   left_join(tibble(var = names(x), mean2 = x), by = "var") %>%
# #   na.omit()
#
# #----------------
#
# # Compile final results
# result <- imp.out %>%
#   select(cuid, intnum, month, year, one_of(ucc.expname.var$var)) %>%
#   pivot_longer(cols = contains("_"), names_to = "var", values_to = "cost") %>%  # Pivot data to long form
#   left_join(ucc.expname.var %>% select(var, neg) %>% distinct(), by = "var") %>%
#   mutate(cost = ifelse(neg, -cost, cost)) %>%  # "Flip" the originally all-negative variables back to negative
#   separate(var, into = c("expname", "ucc"), sep = "_") %>%
#   lazy_dt() %>%  # Added to speed up group_by %>% summarize via dtplyr package
#   group_by(cuid, intnum, month, year, ucc) %>%
#   summarize(cost = sum(cost)) %>%  # Aggregate expenditures by month and UCC
#   ungroup() %>%
#   mutate(cost = as.integer(round(cost))) %>%  # Round costs and convert to integer
#   filter(cost != 0) %>%  # Drop zero-cost observations
#   as_tibble()
#
# # Save result to disk
# saveRDS(result, paste0("data-raw/CEX/", survey_year, "/mtbi_post_", survey_year, ".rds"), compress = "bzip2")
#
# }
