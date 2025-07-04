list(

  d4_1__schl = list(
    GALLUP = list(
      groups = 1:8,
      levels = c("Less than a high school diploma (Grades 1 through 11 or no schooling)", "High school graduate (Grade 12 with diploma or GED certificate)", "Technical, trade, vocational or business school or program after high school.", "Some college - college, university or community college -- but no degree", "Two year associate degree from a college, university, or community college", "Four year bachelor's degree from a college or university (e.g., BS, BA, AB)", "Some postgraduate or professional schooling after graduating college, but no postgraduate degree (e.g. some graduate school)", "Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD, JD)"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 4, 5, 6, 8, 7, 8),
      levels = c("Less than 3 years old", "No schooling completed", "Nursery school, preschool", "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "12th grade - no diploma", "Regular high school diploma", "GED or alternative credential", "Some college, but less than 1 year", "1 or more years of college credit, no degree", "Associate's degree", "Bachelor's degree", "Master's degree", "Professional degree beyond a bachelor's degree", "Doctorate degree"),
      breaks = "",
      adj = "",
      agg = "reference"),
    ordered = TRUE,
    comment = "",
    modified = "2025-05-19 09:02:10.765889"),

  d9__np = list(
    GALLUP = list(
      groups = 1,
      levels = "Min: 0, Median: 2, Mean: 2.256, Max: 82",
      breaks = "",
      adj = ""),
    ACS = list(
      groups = 1,
      levels = "Min: 1, Median: 2, Mean: 2.47, Max: 20",
      breaks = "",
      adj = "",
      agg = ""),
    ordered = "",
    comment = "",
    modified = "2025-05-19 07:39:07.776818"),

  emp1__esr = list(
    GALLUP = list(
      groups = c(1, 2, 2, 2),
      levels = c("No, not employed", "Yes, employed for an employer", "Yes, employed for an employer AND self-employed", "Yes, self-employed"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = c(1, 2, 2, 1, 2, 2, 1),
      levels = c("Less than 16 years old", "Civilian employed, at work", "Civilian employed, with a job but not at work", "Unemployed", "Armed forces, at work", "Armed forces, with a job but not at work", "Not in labor force"),
      breaks = "",
      adj = "",
      agg = "reference"),
    ordered = FALSE,
    comment = "",
    modified = "2025-06-07 11:30:51.104271"),

  h14__hicov = list(
    GALLUP = list(
      groups = 1:2,
      levels = c("Yes", "No"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = 1:2,
      levels = c("With health insurance coverage", "No health insurance coverage"),
      breaks = "",
      adj = "",
      agg = "reference"),
    ordered = FALSE,
    comment = "",
    modified = "2025-05-19 07:39:07.777032"),

  h17__noc = list(
    GALLUP = list(
      groups = 1,
      levels = "Min: 0, Median: 0, Mean: 0.65, Max: 15",
      breaks = "",
      adj = ""),
    ACS = list(
      groups = 1,
      levels = "Min: 0, Median: 0, Mean: 0.497, Max: 14",
      breaks = "",
      adj = "",
      agg = ""),
    ordered = "",
    comment = "",
    modified = "2025-05-19 07:39:07.77708"),

  hispanic__hisp = list(
    GALLUP = list(
      groups = 1:2,
      levels = c("No", "Yes"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = c(1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
      levels = c("Not Spanish / Hispanic / Latino", "Mexican", "Puerto Rican", "Cuban", "Dominican", "Costa Rican", "Guatemalan", "Honduran", "Nicaraguan", "Panamanian", "Salvadoran", "Other Central American", "Argentinean", "Bolivian", "Chilean", "Colombian", "Ecuadorian", "Paraguayan", "Peruvian", "Uruguayan", "Venezuelan", "Other South American", "Spaniard", "All Other Spanish / Hispanic / Latino"),
      breaks = "",
      adj = "",
      agg = "reference"),
    ordered = FALSE,
    comment = "",
    modified = "2025-05-19 07:39:07.777132"),

  income_summary__hincp = list(
    GALLUP = list(
      groups = 1:10,
      levels = c("Under $720", "$720 to $5999", "$6000 to $11999", "$12000 to $23999", "$24000 to $35999", "$36000 to $47999", "$48000 to $59999", "$60000 to $89999", "$90000 to $119999", "$120000 and over"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = 1:10,
      levels = c("Less than 720", "[720 to 6000)", "[6000 to 12000)", "[12000 to 24000)", "[24000 to 36000)", "[36000 to 48000)", "[48000 to 60000)", "[60000 to 90000)", "[90000 to 120000)", "120000 or more"),
      breaks = c(720, 6000, 12000, 24000, 36000, 48000, 60000, 90000, 120000),
      adj = "",
      agg = ""),
    ordered = TRUE,
    comment = "",
    modified = "2025-05-19 08:54:30.887125"),

  race__rac1p = list(
    GALLUP = list(
      groups = c(1, 2, 4, 3, 4),
      levels = c("Asian American", "White", "Hispanic", "Black", "Other"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = c(2, 3, 4, 4, 4, 1, 4, 4, 4),
      levels = c("White alone", "Black or African American alone", "American Indian alone", "Alaska Native alone", "American Indian and Alaska Native tribes specified; or American Indian or Alaska Native, not specified and no other races", "Asian alone", "Native Hawaiian and Other Pacific Islander alone", "Some Other Race alone", "Two or More Races"),
      breaks = "",
      adj = "",
      agg = "reference"),
    ordered = FALSE,
    comment = "",
    modified = "2025-05-19 07:39:07.777174"),

  sc7__sex = list(
    GALLUP = list(
      groups = 1:2,
      levels = c("Male", "Female"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = 1:2,
      levels = c("Male", "Female"),
      breaks = "",
      adj = "",
      agg = "reference"),
    ordered = FALSE,
    comment = "",
    modified = "2025-05-19 07:39:07.777209"),

  wp1220__agep = list(
    GALLUP = list(
      groups = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 79, 79, 79, 1),
      levels = c("19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "Greater than 99", "Less than 18"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = 1:79,
      levels = c("Less than 19", "[19 to 20)", "[20 to 21)", "[21 to 22)", "[22 to 23)", "[23 to 24)", "[24 to 25)", "[25 to 26)", "[26 to 27)", "[27 to 28)", "[28 to 29)", "[29 to 30)", "[30 to 31)", "[31 to 32)", "[32 to 33)", "[33 to 34)", "[34 to 35)", "[35 to 36)", "[36 to 37)", "[37 to 38)", "[38 to 39)", "[39 to 40)", "[40 to 41)", "[41 to 42)", "[42 to 43)", "[43 to 44)", "[44 to 45)", "[45 to 46)", "[46 to 47)", "[47 to 48)", "[48 to 49)", "[49 to 50)", "[50 to 51)", "[51 to 52)", "[52 to 53)", "[53 to 54)", "[54 to 55)", "[55 to 56)", "[56 to 57)", "[57 to 58)", "[58 to 59)", "[59 to 60)", "[60 to 61)", "[61 to 62)", "[62 to 63)", "[63 to 64)", "[64 to 65)", "[65 to 66)", "[66 to 67)", "[67 to 68)", "[68 to 69)", "[69 to 70)", "[70 to 71)", "[71 to 72)", "[72 to 73)", "[73 to 74)", "[74 to 75)", "[75 to 76)", "[76 to 77)", "[77 to 78)", "[78 to 79)", "[79 to 80)", "[80 to 81)", "[81 to 82)", "[82 to 83)", "[83 to 84)", "[84 to 85)", "[85 to 86)", "[86 to 87)", "[87 to 88)", "[88 to 89)", "[89 to 90)", "[90 to 91)", "[91 to 92)", "[92 to 93)", "[93 to 94)", "[94 to 95)", "[95 to 96)", "96 or more"),
      breaks = c(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96),
      adj = "",
      agg = "reference"),
    ordered = TRUE,
    comment = "",
    modified = "2025-05-19 08:34:18.862082"),

  wp1223__mar = list(
    GALLUP = list(
      groups = c(5, 1, 4, 3, 2, 5),
      levels = c("Single/Never been married", "Married", "Separated", "Divorced", "Widowed", "Domestic partnership/Living with partner (not legally married)"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = 1:5,
      levels = c("Married", "Widowed", "Divorced", "Separated", "Never married or under 15 years old"),
      breaks = "",
      adj = "",
      agg = "reference"),
    ordered = FALSE,
    comment = "",
    modified = "2025-05-19 07:39:07.77725")
)
