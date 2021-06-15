
detectDependence <- function(data, xvars = NULL) {

  require(data.table)

  stopifnot(is.data.frame(data))

  # The variables for which we want to know if derivatives exist
  if (is.null(xvars)) {
    xvars <- names(data)
  } else {
    stopifnot(all(xvars %in% names(data)))
  }

  # Restrict 'xvars' to eliminate any ID columns with all unique values (e.g. household ID)
  xvars <- data[xvars] %>%
    select_if(~ length(unique(na.omit(.x))) < length(.x)) %>%
    names()

  # Variables we test to see if they are potential derivatives of 'xvars'. This
  # excludes numeric variables, which are assumed to never be derivative of
  # another variable.
  yvars <- data %>%
    select_if(~ !is.numeric(.x) & length(unique(na.omit(.x))) > 1) %>%
    names()

  # Ensure 'data' is a type-converted data frame (necessary?)
  dset <- data %>%
    select(all_of(unique(c(xvars, yvars))))
    #mutate_all(type.convert)

  # Convert factors to integer and coerce to data.table for speed
  DT <- dset %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.integer) %>%
    data.table()

  #-----

  # Function to iterate over each 'x'...
  #x <- xvars[3]

  # For variable 'x', detect any 'yvars' that can be deduced from 'x'
  checkX <- function(x) {

    y <- setdiff(yvars, x)

    # Data subset to work with
    dt <- DT %>%
      subset(!is.na(DT[[x]]), select = c(x, y)) %>%
      setkeyv(x)

    # If 'x' is numeric, first identify which 'y' variables are sorted after sorting on 'x'
    if (is.numeric(data[[x]])) {
      temp <- is.na(dt)
      for (v in y) dt[ , (v) := .GRP, by = v]
      dt[temp] <- NA  # Reassign NA values after group ID's inserted
      check <- unlist(!dt[, lapply(.SD, is.unsorted, na.rm = TRUE)])[1, -1]
      y <- names(which(check))
    }

    # Is a given 'y' deducible from 'x'? 'deduce' returns a logical. 'y' is
    # deducible if each 'x' value is associated with a single 'y' value
    if (length(y) > 0) {
      if (length(y) < ncol(dt) - 1) dt <- dt[, c(x, y), with = FALSE]  # Restrict to remaining 'y' candidates, if necessary
      f <- function(x) {length(unique(na.omit(x))) == 1}
      check1 <- as.vector(!dt[ , lapply(.SD, f), .SDcols = y])  # This check returns FALSE if a 'y' variable has only 1 non-NA value (this is considered incomplete information to determine derivative status)
      check2 <- colSums(!dt[, lapply(.SD, f), by = x][, -1]) == 0  # This check returns TRUE if each 'x' value is associated with a single (non-NA) 'y' value
      y <- names(which(check1 & check2))
    }

    out <- NULL
    if (length(y) > 0) out <- setNames(list(y), x)

    return(out)

  }

  #-----

  result <- parallel::mclapply(xvars, FUN = checkX, mc.cores = 3L) %>%
    discard(is.null) %>%
    unlist(recursive = FALSE)

  return(result)

}
