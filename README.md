## Overview

**fusionData** is used to create and manage the data inputs underpinning
the larger fusionACS platform. It has a number of objectives:

1.  Process raw survey microdata using a standard approach and format.
2.  Consistently document survey variables to create a “universal” data
    dictionary.
3.  Harmonize variables in “donor” surveys with those in the American
    Community Survey (ACS).
4.  Create harmonized donor and ACS microdata to be used by
    [fusionModel](https://github.com/ummel/fusionModel).
5.  Process and integrate spatially-referenced data available to the
    fusion process.

Each of these objectives is described in more detail below along with
example usage.

## Structure and usage

Although fusionData is structured (and loadable) as a R package, it is
better to think of it as a code and data *repository* that is shared and
continuously modified by authorized users. fusionData is expected to
grow over time as new surveys and spatial datasets – and the code needed
to process and manipulate them – are added. fusionData is *not public*.

Since Github places limits on file/repository size, I decided to store
certain data files “remotely” – that is, outside of the Github
repository. Specifically, the lowest-level “raw” data inputs and the
associated “processed” versions of that data (details below). Over time,
it is expected that these types of data files could become quite large
in the aggregate.

The remotely-stored “raw” and “processed” data files are integral to the
overall fusionData “system”, but they are not present in the Github
repository itself. Instead, the remote files (and associated directory
structure) are stored in Google Drive and can be automatically and
safely added to a user’s *local* fusionData folder using provided
functions. Once the remote files are added, the user’s local fusionData
package is fully functional.

Remote data files should be rather static over time. So, it is expected
that a user will need to update (re-download) the remote files to their
local fusionData directory only infrequently. However, when users add or
modify *code* (or smaller, ancillary data files), the changes are then
pushed to the Github repository where they become subject to code
reviews, versioning control, and accessible to other authorized users.

In short: The Github repository stores any and all code needed to build
and document the fusionData architecture. But certain,
infrequently-modified data files are stored remotely. Users can add the
remote files to their local installation of fusionData and only code and
ancillary data files are pushed to Github when modified.

Changes or additions to the remote files is likely to be rare, and will
be done “by hand” to prevent any inadvertent changes.

Below is an overview of the top-level directories in the fusionData
repository, including both Github-based and “remote” elements.

### `/R`

`.R` scripts defining functions useful for doing “fusionData things”.
The exported functions become available to user after calling
`library(fusionData)`.

### `/data-raw`

`.R` scripts needed to create any package-wide `.rda` objects in
`/data`, as usual for R packages.

### `/data`

Shared, package-wide `.rda` data files. Loadable via `data()`, as usual
for R packages.

### `/dictionary`

Directory for the “Universal Survey Dictionary” Shiny app. The app
itself can be run by calling `dictionary()`.

### `/harmony`

Directory for the “Survey Harmonization Tool” Shiny app. The app itself
can be run by calling `harmony()`.

### `/man`

Documentation (i.e. “manual”) of functions in `/R` and data in `/data`,
as usual for R packages.

### `/survey-raw`

A remote directory (i.e. not present in the Github repository)
containing *raw* survey data files. Sub-directories refer to specific
surveys and vintages (e.g. `/survey-raw/RECS/2015`). `/survey-raw` can
be downloaded and added to a user’s local fusionData directory by
calling `getSurveyRaw()`.

### `/survey-processed`

Contains subdirectories that refer to specific surveys and vintages
(e.g. `/survey-processed/RECS/2015`). The Github repository version of
`/survey-processed` contains two kinds of files:

1.  Custom `.R` scripts that transform *raw* survey microdata (located
    in `/survey-raw`) into “processed” versions that adhere to certain
    requirements, structure, and naming conventions. Example:
    `/survey-processed/RECS/2015/RECS_2015_H_processed.R`

2.  “Dictionary” files (`.rds`) that contain standardized metadata and
    variable descriptions for a particular survey. Example:
    `/survey-processed/RECS/2015/RECS_2015_H_dictionary.rds`

Importantly, `/survey-processed` also contains `.fst` files with the
processed microdata. Example:
`/survey-processed/RECS/2015/RECS_2015_H_processed.fst`. These files are
stored remotely. Use of [.fst files](http://www.fstpackage.org/) allows
this data to be read very quickly from disk, in part or in full. The
remotely-stored `*_processed.fst` files can be added to a user’s local
fusionData directory by calling `getSurveyProcessed()`.

<!--
### `/spatial-raw`
In development. Analogous to `survey-raw` but for spatial data.

### `/spatial-processed`
In development. Analogous to `survey-processed` but for spatial data.
-->

## Process raw survey microdata

To do.

## Document survey variables

To do.

``` r
# Open "Universal Survey Dictionary" Shiny app
universe()
```

## Harmonize variables

To do.

``` r
# Open "Survey Harmonization Tool" Shiny app
harmony()
```

## Create harmonized microdata

To do.

## Integrate spatial data

To do.

## Putting it all together

The following example shows how fusionData functions can be used to
assemble harmonized microdata that is then passed to the [fusionModel
package](https://github.com/ummel/fusionModel) to fuse donor variables
to ACS microdata.

We begin by using `harmonize()` to parse the information in a `.R`
“harmony file” and create microdata containing the harmonized
(i.e. shared) variables among the donor and recipient – in this case,
the RECS and ACS. The harmony file is produced using the Survey
Harmonization Tool (`?harmony`). See `?harmonize` for details.

``` r
# Harmonize RECS 2015 and ACS 2019 microdata
hdata <- harmonize(harmony.file = "RECS_2015__ACS_2019.R", respondent = "household")
```

The resulting `hdata` object is a list containing two data frames.

``` r
lapply(hdata, dim)
```

    $RECS_2015
    [1] 5686   14

    $ACS_2019
    [1] 1276716      14

We can look at the microdata in each of the data frames to confirm that
the variables are, indeed, harmonized. Note that the harmonized variable
*values* are typically integers (possibly factorized); that is, they
contain no intelligible labels. This is because `harmonize()` maps each
original value/level to a (integer) group assignment as specified in the
`.R` harmony file. The one exception is when *numeric* variables in the
two surveys are conceptually identical and can be included without
modification.

``` r
head(hdata$RECS_2015[, 1:5])
```

      recs_2015_hid bedrooms__bdsp fuelheat__hfl hhage__agep hhsex__sex
    1         10001              3             4          42          2
    2         10002              2             2          60          1
    3         10003              4             4          73          1
    4         10004              3             5          69          1
    5         10005              3             4          51          2
    6         10006              0             5          33          2

``` r
head(hdata$ACS_2019[, 1:5])
```

       acs_2019_hid bedrooms__bdsp fuelheat__hfl hhage__agep hhsex__sex
    1 2019HU0000001              4             2          68          1
    2 2019HU0000002              3             4          80          2
    3 2019HU0000003              4             4          42          2
    4 2019HU0000004              2             4          47          1
    5 2019HU0000005              2             2          87          2
    6 2019HU0000006              4             4          54          1

Since RECS and ACS are both nationally representative surveys, the
distribution of the harmonized variables should look pretty similar
across the two data frames. Let’s confirm this for the `fuelheat__hfl`
variable, which creates harmony between the RECS and ACS heating fuel
variables (“fuelheat” and “hfl”, respectively, in the original data).

``` r
round(table(hdata$RECS_2015$fuelheat__hfl) / nrow(hdata[[1]]), 3)
```


        1     2     3     4     5     6     7 
    0.045 0.347 0.043 0.491 0.042 0.001 0.030 

``` r
round(table(hdata$ACS_2019$fuelheat__hfl) / nrow(hdata[[2]]), 3)
```


        1     2     3     4     5     6     7 
    0.011 0.375 0.049 0.467 0.067 0.009 0.023 

Looks good. The next step is to add “fusion variables” to the harmonized
RECS microdata to create our “donor” dataset. The fusion variables are
those we want to “fuse” from RECS to the ACS. The `completeDonor()`
function provides a convenient way to do this. By default, it will add
*all* valid fusion variables; i.e. variables not already used for
harmonization. But we can also use `dplyr::select()`-style arguments to
return only a subset of RECS variables. Like this:

``` r
donor <- completeDonor(data = hdata$RECS_2015, cooltype, agecenac, kwhcol)
```

The resulting `donor` data frame contains all of the harmonized
variables as well as the specified fusion variables.

``` r
head(donor)
```

      recs_2015_hid weight bedrooms__bdsp fuelheat__hfl hhage__agep hhsex__sex
    1         10001  12090              3             4          42          2
    2         10002  14400              2             2          60          1
    3         10003  23330              4             4          73          1
    4         10004  12170              3             5          69          1
    5         10005  16720              3             4          51          2
    6         10006  26060              0             5          33          2
      kownrent__ten moneypy__hincp numadult__agep numfrig__refr sdescent__hisp
    1             2              8              2             2              2
    2             2              2              2             2              2
    3             2              2              3             2              2
    4             2              3              1             2              2
    5             2              3              2             2              2
    6             3              1              1             2              2
      stoven__stov totrooms__rmsp typehuq__bld yearmaderange__ybl
    1            2              7            5                  7
    2            2              4            5                  5
    3            2              9            5                  4
    4            2              7            5                  2
    5            2              6            5                  4
    6            2              1            2                  5
                                        cooltype                   agecenac kwhcol
    1            Central air conditioning system         10 to 14 years old 1990.0
    2   Individual window/wall or portable units No central air conditioner 2224.0
    3 Both a central system and individual units         15 to 19 years old 6840.0
    4            Central air conditioning system           5 to 9 years old 3535.0
    5            Central air conditioning system           5 to 9 years old  333.0
    6   Individual window/wall or portable units No central air conditioner  157.2

At this point, we are ready to fuse! This is straightforward using the
`train()` and `fuse()` functions from the fusionModel package. To make
this even easier, `completeDonor()` output includes a “fusion.vars”
attribute that can be passed directly to `train()` to identify the
variables to be fused.

``` r
fit <- fusionModel::train(data = donor, 
                          y = attr(donor, "fusion.vars"), 
                          ignore = "recs_2015_hid", 
                          weight = "weight")
```

We then `fuse()` (i.e. simulate) the fusion variables onto the
harmonized ACS microdata. This is a non-trivial exercise, since the
recipient ACS microdata has 1276716 observations.

``` r
sim <- fusionModel::fuse(data = hdata$ACS_2019, train.object = fit)
```

A quick check that everything looks plausible:

``` r
nrow(sim)
```

    [1] 1276716

``` r
head(sim)
```

         kwhcol              agecenac                        cooltype
    1  906.9314 Less than 2 years old Central air conditioning system
    2 2486.0414     20 years or older Central air conditioning system
    3 3367.7424      2 to 4 years old Central air conditioning system
    4 1654.6932    10 to 14 years old Central air conditioning system
    5 2244.8653      5 to 9 years old Central air conditioning system
    6 1494.1341    10 to 14 years old Central air conditioning system

``` r
summary(donor$kwhcol)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        0.0   376.1  1093.5  1857.8  2527.0 20350.0 

``` r
summary(sim$kwhcol)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        0.0   371.9  1135.3  1864.2  2562.0 20350.0 

Onward and upward!
