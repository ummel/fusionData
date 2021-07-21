# data: data frame to be imputed
# N: number of iterations
# y_exclude: variables not to be imputed, even if NA values are present
# x_exclude: variables not to be used as potential predictors in imputation models
# weight: name of variable providing observation weights
# grouping: used to enforce consistent imputation within certain subsets of the data (e.g. within households)

#-----

# Sample data for testing
# data <- readRDS("survey-out/impin_test_FMLI.rds")
# y_exclude <- "state"
# x_exclude = c("cuid", "weight", "survey_year", "qintrvmo", "hh_cu_q")
# weight <- "weight"
#
# hierarchy <- list(c("region", "division", "state"),
#                   c("irab", "irax"),
#                   c("liquidb", "liquidx"),
#                   c("stockb", "stockx"))
#
# sequence <- NULL

#-----

# Drops input rows with NA's
#temp <- model.matrix.lm(object = ~ 0 + irab, data = test)

#y <- "irab"
#x <- "state"

collapseCategorical <- function(x, y, data, ncats) {

  mm <- model.matrix.lm(object = as.formula(paste("~ 0 +", y)), data = data, na.action = "na.pass")

  d <- data %>%
    select(all_of(c("_(wgt)_", x))) %>%  # Note that `_(wgt)_` variable is hard-coded
    cbind(mm) %>%
    na.omit() %>%
    group_by_at(x) %>%
    summarize_at(colnames(mm), ~ weighted.mean(.x, `_(wgt)_`, na.rm = TRUE))  # Note that `_(wgt)_` variable is hard-coded

  # Derive kmeans() clusters
  # NOTE: The following error is indicative of a 'y' variable that is perfectly defined by the 'x'
  # This usually means that 'x' should be listed as a subsidiary of 'y' via the 'hierarchy' argument in imputeMissing()
  # "Error in kmeans(x = select(d, -all_of(x)), centers = ncats, nstart = 30) : more cluster centers than distinct data points."
  maxn <- nrow(distinct(d[-1L]))  # This will eliminate error mentioned above (but preferable to not have it invoked)
  if (maxn < ncats) message("collapseCategorical(): Only ", maxn, " distinct data points (x = ", x, ", y = ", y, "); setting 'ncats' to ", min(ncats, maxn))
  k <- kmeans(x = select(d, -all_of(x)), centers = min(ncats, maxn), nstart = 30)

  out <- d %>%
    mutate(cluster = factor(paste("cluster", k$cluster, sep = "_"))) %>%
    select(all_of(c(x, "cluster"))) %>%
    setNames(c(x, paste(x, "collapsed", sep = "_")))

  return(out)

}


#---------------------------------
#---------------------------------

imputeMissing <- function(data,
                          N = 2,  # number of iterations to perform
                          max_ncats = 10,  # Maximum number of levels allowed in categorical predictors
                          weight = NULL,
                          y_exclude = NULL,
                          x_exclude = NULL,
                          sequence = NULL,
                          grouping = NULL,
                          parallel = TRUE) {

  # Check validity of inputs
  stopifnot(is.data.frame(data))

  #-----

  # Identify variables to impute

  # Identify variables with NA's and parse 'y_exclude' to determine which should be imputed
  regx <- setdiff(y_exclude, names(data))
  if (!length(regx)) regx <- character()
  suppressWarnings(y.exclude <- data %>%
                     select(one_of(y_exclude), matches(regx, ignore.case = FALSE)) %>%
                     names())
  if (!length(y.exclude)) y.exclude <- NULL

  # Number of NA values per non-excluded variable
  na.count <- data %>%
    select(-one_of(y.exclude)) %>%
    is.na() %>%
    colSums()

  # Variables to impute
  vimp <- names(na.count[na.count > 0])

  # Identify any imputation variables without variation
  # These are imputed right away (only one possible value)
  novary <- map_lgl(data[vimp], ~ length(unique(.x)) == 2)
  novary <- names(which(novary))
  if (length(novary) > 0) {
    message("Imputing variable(s) with no variance: ", paste(novary, collapse = ", "))
    for (v in novary) {x <- data[[v]]; data[[v]] <- replace_na(x, x[which(!is.na(x))[1]])}
    vimp <- setdiff(vimp, novary)
  }

  # Print message about imputation variables
  stopifnot(length(vimp) > 0)
  message("Identified ", length(vimp), " variable(s) to be imputed")
  message("Missing values = ", sum(na.count), " (", paste0(round(100 * sum(na.count) / (nrow(data) * length(vimp)), 3), "%"), ")")

  #---

  # Parse 'x_exclude' to identify variables that should be excluded as predictors
  regx <- setdiff(x_exclude, names(data))
  if (!length(regx)) regx <- character()
  suppressWarnings(x.exclude <- data %>%
                     select(one_of(x_exclude), matches(regx, ignore.case = FALSE)) %>%
                     names())
  if (length(x.exclude)) {
    message("The following ", length(x.exclude), " variables are excluded as predictors:\n", paste(x.exclude, collapse = ", "))
  } else {
    x.exclude <- NULL
  }

  #---

  # Create 'xm' copy of input 'data' and convert to preferable classes
  xm <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.ordered, as.integer) %>%
    mutate_if(is.logical, as.integer)

  #---

  # Create observation weights column in 'xm'
  if (is.null(weight)) {
    w <- "_(wgt)_"
    xm[[w]] <- rep(1L, nrow(data))
    message("Using uniform observation weights")
  } else {
    stopifnot(weight %in% names(xm))
    stopifnot(all(xm[[weight]] >= 0 & is.finite(xm[[weight]])))
    xm$`_(wgt)_` <- xm[[weight]] / sum(xm[[weight]])  # Scaled weight to avoid numerical issues with larger integer weights
    xm[[weight]] <- NULL
    w <- "_(wgt)_"
  }

  #---

  # Number of categorical levels for each allowable, non-numeric predictor
  cats.count <- xm %>%
    select(-one_of(x.exclude)) %>%
    map_int(~ ifelse(is.numeric(.x), 0L, length(na.omit(unique(.x)))))

  # Categorical predictor variables that need to be collapsed prior to fitting rpart() model
  xlarge <- names(cats.count[cats.count > max_ncats])

  #---

  # Detect derivatives of imputation variables
  message("Detecting dependence among the imputation variables...")
  derivatives <- detectDependence(data, xvars = vimp)

  # Process 'derivatives' list created earlier
  if (!is.null(derivatives)) {

    # drop <- setdiff(names(derivatives), vimp)
    # if (length(drop) > 0) {
    #   warning("Ignored the following entries in 'derivatives' (not imputation variables):\n", paste(drop, collapse = ", "))
    #   derivatives <- derivatives[names(derivatives) %in% vimp]
    # }

    # Create inverse of the derivative list
    # Use to determine which predictors to ignore during mode fitting
    deriv.inv <- mapply(rep, names(derivatives), lengths(derivatives)) %>%
      unlist(use.names = FALSE) %>%
      as.list() %>%
      setNames(unlist(derivatives))

    # Create 'deriv.dfs' list. This is a list of data frames, each identifying a
    # unique combination of "derivative" variables for the variable given by the
    # slot name. The derivative variables must be imputed first.
    deriv.dfs <- lapply(derivatives, FUN = function(x) {
      out <- xm %>%
        select(all_of(x)) %>%
        na.omit() %>%
        distinct() %>%
        mutate(deriv_group_index = 1:n())
      #stopifnot(nrow(out) == length(unique(out[[1]])))  # This enforces a strict hierarchy, which I don't think is always desirable
      return(out)
    })

  } else {
    deriv.inv <- NULL
    deriv.dfs <- NULL
  }

  #---

  # Check validity of the 'sequence' argument
  if (!is.null(sequence)) {
    stopifnot(is.list(sequence) | is.character(sequence))
    stopifnot(all(unlist(sequence) %in% names(xm)))
  }

  # Assign all imputation variables not specified in 'sequence'
  sequence[[length(sequence) + 1]] <- setdiff(vimp, unlist(sequence))

  # Generate the 'vimp' groups list for sequential imputation
  vimp.groups <- sequence %>%
    map(~ intersect(.x, vimp)) %>%
    keep(~ length(.x) > 0)

  #---

  # Check validity of grouping and pre-process

  if (!is.null(grouping)) {

    # Check that variables are only assigned to only one grouping element
    # The idea of 'grouping' is that some variables are measured at different "levels" than other (e.g. household vs. person)
    # Consequently, it doesn't make sense for a variable to be measured at multiple levels
    stopifnot(!duplicated(unlist(grouping[[1]])))
    stopifnot(!duplicated(unlist(grouping[[2]])))

    # Validity checks and pre-computation of row-group tibble for each grouping element
    # for (i in 1:length(grouping)) {
    grps <- grouping[[1]]  # Variables that define the groups
    stopifnot(all(grps %in% names(xm)))
    stopifnot(!anyNA(xm[grps]))  # Grouping variables must be all non-NA

    vars <- intersect(grouping[[2]], names(xm))  # Variables that should be consistent within groups
    grouping[[2]] <- vars

    # Overwrite first list slot with group-row tibble with row index
    # Since the 'grps' variables are all non-NA to begin with, this can be computed upfront just once
    grouping[[1]] <- xm[grps] %>%
      mutate(row = 1:n()) %>%
      group_by_at(grps) %>%
      mutate(group = cur_group_id()) %>%
      ungroup() %>%
      select(row, group)

  }

  #-------------------------
  #-------------------------

  # Function to fit rpart() models for each variable to be imputed
  fitRpartModel <- function(v) {

    d.rpart <- xm

    # Potential predictors variables to ignore for model fitting
    parents <- rev(names(deriv.dfs[[v]]))[-1L]
    children <- unlist(deriv.inv[v])
    ignore <- c(parents, children)

    # Move this outside of fitRpartModel? Do results really change much?
    # If 'v' is categorical, collapse the 'xlarge' predictor variables
    x.clps <- setdiff(xlarge, c(v, ignore))
    if (!is.numeric(xm[[v]]) & length(x.clps)) {
      collapsed.xwalk <- x.clps %>%
        map(collapseCategorical, y = v, data = d.rpart, ncats = max_ncats) %>%
        setNames(x.clps)
      for (x in names(collapsed.xwalk)) {
        d.rpart <- d.rpart %>%
          left_join(collapsed.xwalk[[x]], by = x) %>%
          select(-all_of(x))
      }
    } else {
      collapsed.xwalk <- NULL
    }

    # rpart() formula object
    xvars <- setdiff(names(d.rpart), c(v, w, x.exclude, ignore))
    fobj <- as.formula(paste0(v, "~", paste(xvars, collapse = "+")))

    # Merge strata, if necessary
    if (v %in% names(deriv.dfs)) {
      d.rpart <- left_join(d.rpart, deriv.dfs[[v]], by = rev(names(deriv.dfs[[v]]))[-1L])
    } else {
      d.rpart$deriv_group_index <- 1L
    }

    # Restrict 'd.rpart' to necessary variables
    d.rpart <- select(d.rpart, all_of(c("deriv_group_index", v, w, xvars)))

    #------------

    # Fit rpart() model(s)

    # If relevant, restrict to strata with NA values in original data (don't need models otherwise)
    ind <- sort(unique(d.rpart$deriv_group_index))
    out <- vector(mode = "list", length = length(ind))

    # Using loop and assigning results to 'out' list
    # rpart() does not like if this is put inside lapply() or map(); don't know why
    for (i in ind) {

      # Subset 'd.rpart' for strata 'i'
      #d.fit <- filter(d.rpart, deriv_group_index == i)

      # Subset 'd.rpart' for strata 'i' and restrict to observationsn with non-NA response values
      d.fit <- d.rpart %>%
        filter(deriv_group_index == i, !is.na(d.rpart[[v]]))

      # Sample 'd.fit' if it has more than 500,000 observations
      if (nrow(d.fit) > 500e3) d.fit <- slice_sample(d.fit, n = 500e3)

      # Convert factor response to character
      if (is.factor(d.fit[[v]])) d.fit[[v]] <- as.character(d.fit[[v]])

      # Logical vector indicating valid (non-NA) response values in 'd.fit'
      #j <- !is.na(d.fit[[v]])

      # Fit rpart() model
      m <- rpart::rpart(formula = fobj,
                        data = d.fit,
                        weights = d.fit[[w]],
                        method = ifelse(is.numeric(d.fit[[v]]), "anova", "class"),
                        minbucket = max(30, ceiling(0.0001 * nrow(d.fit))),
                        cp = 0.001,  # Set sufficiently low without imposing computing cost
                        xval = 0)

      # OPTIONAL: Prune rpart model using minimum cross-validation (NOT USED)
      # If there is no variation in 'y', the CP table will be single row (but model object still works for prediction)
      #if (nrow(m$cptable) > 1) {
      #  m <- rpart::prune(m, cp = m$cptable[which.min(m$cptable[,"xerror"]), "CP"])
      #}

      # Visualize tree
      #rattle::fancyRpartPlot(m, type = 5)

      #---

      # Replace variable importance with aggregated and scaled
      vi <- m$variable.importance
      if (length(vi) > 0) {
        vi <- vi / sum(vi)
      } else {
        vi <- 0
        names(vi) <- v
      }
      m$variable.importance <- vi

      # Retain name of the model's response variable for easy lookup later
      m$yvar <- v

      # Add the 'weight' values from training data to rpart output
      #m$weight <- d.fit[[w]][j]
      m$weight <- d.fit[[w]]

      m$xwalk <- collapsed.xwalk

      # Assign result
      out[[i]] <- m

    }

    rm(d.fit, d.rpart)
    gc()

    return(out)

  }

  #-------------------------
  #-------------------------


  for (iter in 1:N) {

    for (vgrp in 1:length(vimp.groups)) {

      vimp <- vimp.groups[[vgrp]]

      # Fit all models
      n.cores <- ifelse(parallel, parallel::detectCores() - 1L, 1L)
      message("Iteration ", iter, ", Group ", vgrp, ": Fitting ", length(vimp), " rpart() models using ", n.cores, " cores...")
      m.all <- parallel::mclapply(X = vimp, FUN = fitRpartModel, mc.cores = n.cores)
      names(m.all) <- vimp
      gc()

      # Troubleshoot
      #for (v in vimp) fitRpartModel(v)

      #---------

      getVarImp <- function(m) {
        #if (!is.null(m)) {
        out <- m$variable.importance %>%
          tibble::enframe(name = "predictor", value = "importance") %>%
          filter(predictor %in% vimp) %>%
          mutate(predictor = factor(predictor, levels = vimp)) %>%
          complete(predictor, fill = list(importance = 0)) %>%
          mutate(predictor = as.character(predictor),
                 response = m$yvar,
                 na_weight = sum(xm[is.na(data[[response[1]]]), w])) %>%
          filter(predictor != response)
        #} else {
        #  out <- NULL
        #}
        return(out)
      }

      var.imp <- m.all %>%
        map_dfr(~ map_dfr(.x, getVarImp))

      vseq <- vector(mode = "character", length = length(vimp))
      for (i in 1:(length(vseq) - 1)) {
        ranking <- var.imp %>%
          group_by(predictor) %>%
          summarize(importance = weighted.mean(importance, na_weight)) %>%
          arrange(-importance)
        vbest <- ranking$predictor[1]
        vseq[i] <- vbest
        var.imp <- filter(var.imp, predictor != !!vbest, response != !!vbest)
      }
      vseq[length(vseq)] <- setdiff(vimp, vseq)  # Add the final imputation variable

      #---

      # Ensure 'vseq' honors ordering in 'derivatives' input
      # There is probably a theoretically preferable algorithm
      # i.e. one that finds the permutation of 'vseq' most like the original but that meets set of constraints
      Z <- vseq
      for (v in Z) {

        # 'v' must come after all variables in 'j'
        j <- intersect(derivatives[[v]], Z)

        if (length(j) > 0) {

          k <- max(match(j, Z))

          if (match(v, Z) < k) {

            part1 <- c(setdiff(Z[1:k], v), v)
            part2 <- setdiff(Z, part1)
            Z <- c(part1, part2)

          }

        }

      }

      # Final ordering prior to imputation
      vseq <- Z

      #---------

      # Sequentially impute missing values in original data, using rpart() models in 'm.all'
      for (v in vseq) {

        # Extract CART model(s) for variable in question
        M <- m.all[[v]]

        # Merge deriv.dfs identifier (deriv_group_index), if necessary
        xp <- xm
        if (v %in% names(deriv.dfs)) {
          xp <- left_join(xp, deriv.dfs[[v]], by = rev(names(deriv.dfs[[v]]))[-1L])
        } else {
          xp$deriv_group_index <- 1L
        }

        # Merge collapsed categorical predictors, if necessary
        ind <- which(!map_lgl(M, is.null))[1]
        collapsed.xwalk <- M[[ind]]$xwalk
        if (!is.null(collapsed.xwalk)) {
          for (x in names(collapsed.xwalk)) {
            xp <- xp %>%
              left_join(collapsed.xwalk[[x]], by = x) %>%
              select(-all_of(x))
          }
        }

        #---

        # Loop through each imputation model, making predictions for NA values, and updating 'xm'
        for (i in 1:length(M)) {

          m <- M[[i]]

          if (!is.null(m)) {

            # Row indices of missing values in the original 'data'; not the imputed data frame 'xm'
            na.ind <- which(is.na(data[[v]]) & xp$deriv_group_index == i)

            # Generic prediction, regardless of response variable type
            pred <- rpart.plot::rpart.predict(object = m, newdata = xp[na.ind, ], nn = TRUE)
            node <- match(pred$nn, as.integer(row.names(m$frame)))

            for (j in unique(node)) {
              k <- na.ind[node == j]  # Row indices in 'xm' associated with node 'j'
              yvals <- if (m$method == "class") attr(m, "ylevels")[m$y] else m$y
              samp <- sample(yvals[m$where == j], size = length(k), prob = m$weight[m$where == j], replace = TRUE)
              xm[k, v] <- samp  # Update 'xm'
            }

          }

        }

        #---

        # Enforce 'grouping' requirement (if any)
        if (!is.null(grouping)) {

          for (g in grouping) {

            # Variables that should be consistent within groups
            vars <- g[[1]]

            # Only enforce grouping if the current imputation variable is affected
            if (v %in% vars) {

              # Pre-processed row-group data frame
              df <- g[[2]]

              # Sample a row randomly for each group
              samp <- df[sample.int(nrow(df), nrow(df)), ] %>%
                filter(!duplicated(group)) %>%
                rename(sampled_row = row)

              # Index linking actual 'row' to 'sampled_row'
              # Used to overwrite group-wise values in 'xm'
              index <- df %>%
                left_join(samp, by = "group") %>%
                arrange(row)  # Not strictly necessary; just safer

              # Update 'xm'
              xm[, vars] <- xm[index$sampled_row, vars]

            }

          }

        }

      }

      rm(m.all, xp)
      gc()

      # This check only makes sense for iterations 2+
      # Check no missing values for the imputation variables
      #stopifnot(!anyNA(xm[vimp]))

      # Missing values after first iteration
      #map_int(xm[vimp], ~ sum(is.na(.x)))

    }

  }

  #-----

  # After N iterations complete, update 'data' with fully imputed values in 'xm' and return result
  # The somewhat convoluted approach here is needed to ensure original data classes are respected
  updateNA <- function(x, y) {
    y <- y[is.na(x)]
    if (is.numeric(x)) {
      return(y)
    } else {
      if (is.logical(x)) {
        return(as.logical(y))
      } else {
        if (is.ordered(x)) {
          return(levels(x)[y])
        } else {
          return(as.character(y))
        }
      }
    }
  }

  # Replace NA values in 'data' with imputed values in 'xm'
  vimp <- unlist(vimp.groups)
  #data[vimp] <- map2(data[vimp], xm[vimp], ~ replace(.x, is.na(.x), updateNA(.x, .y)))
  # Return only the imputed variables
  result <- map2_dfc(data[vimp], xm[vimp], ~ replace(.x, is.na(.x), updateNA(.x, .y)))

  # Final check; everything should be fully imputed except columns identified in 'y.exclude'
  #stopifnot(!anyNA(select(data, -one_of(y.exclude))))
  stopifnot(!anyNA(result))

  return(result)

}

#---------------------------------------

# TESTING
# Full-size test
# test <- imputeMissing(data = readRDS("survey-out/impin_test_FMLI.rds"),
#                       y_exclude = "state",
#                       x_exclude = c("cuid", "weight", "survey_year", "qintrvmo", "hh_cu_q"),
#                       weight = "weight",
#                       hierarchy = list(c("region", "division", "state"),
#                                        c("irab", "irax"),
#                                        c("liquidb", "liquidx"),
#                                        c("stockb", "stockx")),
#                       sequence = NULL)
#
# # Check validity of a "range" variable like "irab"
# tapply(test$irax, test$irab, range)
#
# # Check where missings are
# map_int(test, ~ sum(is.na(.x)))
#
# #---
#
# # Generic small example for documentation
#
# input <- iris %>%
#   mutate_all(~ replace(.x, runif(n()) < 0.2, NA)) %>%  # Insert NA's randomly
#   mutate(Petal.Width = Petal.Width < 1.5,  # Logical
#          Petal.Length = factor(letters[round(Petal.Length)], levels = letters[1:7], ordered = TRUE))  # Ordered factor
#
# output <- imputeMissing(data = input)
