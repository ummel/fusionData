harmony2dotR <- function(X, file.out) {

  dput(X, file = file.out, control = c("niceNames"))

  d <- readLines(file.out)
  d <- gsub('"', "'", d, fixed = TRUE)
  #d <- gsub(",", ", ", d, fixed = TRUE)
  d <- paste(d, collapse = " ")
  d <- stringr::str_squish(d)

  level.names <- lapply(0:3, function(i) {
    map_depth(X, .depth = i, names) %>%
      unlist(use.names = FALSE) %>%
      unique()
  })

  for (i in 1:length(level.names)) {
    v <- level.names[[i]]
    for (j in v) {
      d <- gsub(paste0("list(", j, " = "), paste0("list(\n", j, " = "), d, fixed = TRUE)
      d <- gsub(paste0(" ", j, " = "), paste0("\n", j, " = "), d, fixed = TRUE)
      #if (i == 1) d <- gsub(paste0("\\n", j), paste0("\n\n", j), d)
    }
  }

  d <- sub("list(", "list(\n", d, fixed = TRUE)
  # d <- sub("link = list(", "\nlink = list(\n", d, fixed = TRUE)
  # d <- gsub("ordered = FALSE),", "ordered = FALSE),\n", d, fixed = TRUE)
  # d <- gsub("ordered = TRUE),", "ordered = TRUE),\n", d, fixed = TRUE)
  d <- gsub(")))", ")\n))", d, fixed = TRUE)
  d <- gsub("))", ")\n)", d, fixed = TRUE)

  # This insert an additional line break between top-level list elements (for better readability)
  for (n in level.names[[1]][-1L]) d <- gsub(paste0("\\n", n), paste0("\n\n", n), d)

  write(d, file = file.out)
  styler::style_file(file.out, scope = I(c("spaces", "indention", "tokens")))

}

#----------

# X <- list(
#   moneypy__hincp = list(
#     RECS = list(
#       vars = "moneypy",
#       groups = 1:8,
#       levels = c("Less than $20,000", "$20,000 - $39,999", "$40,000 - $59,999", "$60,000 to $79,999", "$80,000 to $99,999", "$100,000 to $119,999", "$120,000 to $139,999", "$140,000 or more"),
#       breaks = "",
#       adj = ""),
#     ACS = list(
#       vars = "hincp",
#       groups = 1:8,
#       levels = c("Less than 20000", "[20000 to 40000)", "[40000 to 60000)", "[60000 to 80000)", "[80000 to 1e+05)", "[1e+05 to 120000)", "[120000 to 140000)", "140000 or more"),
#       breaks = c(20000, 40000, 60000, 80000, 1e+05, 120000, 140000),
#       adj = "hincp * 0.93"),
#     ordered = TRUE,
#     comment = "Adjustment is to deflate hincp from 2019 to 2015 price level prior to binning.",
#     modified = "2021-06-10 08:58:24"),
#   typehuq__bld = list(
#     RECS = list(
#       vars = "typehuq",
#       groups = 1:5,
#       levels = c("Apartment in a building with 2 to 4 units", "Apartment in a building with 5 or more units", "Mobile home", "Single-family attached house", "Single-family detached house"),
#       breaks = "",
#       adj = ""),
#     ACS = list(
#       vars = "bld",
#       groups = c(3, 5, 4, 1, 1, 2, 2, 2, 2, 3),
#       levels = c("Mobile home or trailer", "One-family house detached", "One-family house attached", "2 Apartments", "3-4 Apartments", "5-9 Apartments", "10-19 Apartments", "20-49 Apartments", "50 or more apartments", "Boat, RV, van, etc."),
#       breaks = "",
#       adj = ""),
#     ordered = FALSE,
#     comment = "",
#     modified = "2021-06-10 08:57:07")
# )
#
# test <- list2dotR(X, "test.R")
