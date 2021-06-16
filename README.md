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
`*_processed.fst` files can be added to a user’s local fusionData
directory by calling `getSurveyProcessed()`.

<!--
### `/spatial-raw`
In development. Analogous to `survey-raw` but for spatial data.

### `/spatial-processed`
In development. Analogous to `survey-processed` but for spatial data.
-->

## Process raw survey microdata

## Document survey variables

``` r
# Open "Universal Survey Dictionary" Shiny app
dictionary()
```

## Harmonize variables

``` r
# Open "Survey Harmonization Tool" Shiny app
harmony()
```

## Create harmonized microdata

## Integrate spatial data
