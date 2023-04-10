# Example input
#hfile <- dget("harmony/harmonies/RECS_2015__ACS_2019.R")
#hfile <- dget("harmony/harmonies/ASEC_2019__ACS_2019.R")

hfileList <- function(hfile) {

  # Safety function to convert any NULL values to empty string ""
  f <- function(x) if(is.null(x)) "" else x

  if (!is.null(hfile)) {

    vsplit <- strsplit(names(hfile), "__", fixed = TRUE)

    # Which "survey #1" variables specified for each match?
    v1 <- map_chr(vsplit, 1)

    # Which "survey #2" variables specified for each match?
    v2 <- map_chr(vsplit, 2)

    g1 <- map(hfile, ~ f(.x[[1]]$groups)) %>% setNames(v1)
    g2 <- map(hfile, ~ f(.x[[2]]$groups)) %>% setNames(v2)

    l1 <- map(hfile, ~ f(.x[[1]]$levels)) %>% setNames(v1)
    l2 <- map(hfile, ~ f(.x[[2]]$levels)) %>% setNames(v2)

    b1 <- map(hfile, ~ f(.x[[1]]$breaks)) %>% setNames(v1)
    b2 <- map(hfile, ~ f(.x[[2]]$breaks)) %>% setNames(v2)

    a1 <- map(hfile, ~ f(.x[[1]]$adj)) %>% setNames(v1)
    a2 <- map(hfile, ~ f(.x[[2]]$adj)) %>% setNames(v2)

    agg2 <- map(hfile, ~ f(.x[[2]]$agg)) %>% setNames(v2)

    # Construct data frame of existing matches
    df <- tibble(Donor = map_chr(v1, paste, collapse = ", "),
                 ACS = map_chr(v2, paste, collapse = ", "),
                 `No. of groups` = na_if(map2_dbl(g1, g2, ~ max(.x, .y)), 1),  # Set numeric-numeric pairs to NA for "No. of groups"
                 #Ordered = ifelse(is.na(`No. of groups`), NA, map_chr(hfile, "ordered")),
                 Ordered = ifelse(is.na(`No. of groups`), NA, map_chr(hfile, ~as.character(.x[["ordered"]]))),
                 `Last modified` = map_chr(hfile, "modified")) %>%
      mutate(n = 1:n()) %>%
      select(n, everything())

    list(
      ids = names(hfile),
      ordered = map(hfile, "ordered"),
      comment = map(hfile, "comment"),
      summary = df,
      groups1 = g1,
      groups2 = g2,
      levels1 = l1,
      levels2 = l2,
      breaks1 = b1,
      breaks2 = b2,
      adj1 = a1,
      adj2 = a2,
      agg2 = agg2
    )

  } else {
    NULL
  }

}
