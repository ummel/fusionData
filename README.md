fusionData
================
Kevin Ummel (<ummel@sas.upenn.edu>)

-   [Overview](#overview)
-   [Setup and install](#setup-and-install)
-   [Usage and structure](#usage-and-structure)
-   [Ingest survey data](#ingest-survey-data)
-   [Document variables](#document-variables)
-   [Harmonize variables](#harmonize-variables)
-   [Compile spatial data](#compile-spatial-data)
-   [Prepare for fusion](#prepare-for-fusion)
-   [Make it rain](#make-it-rain)

## Overview

**fusionData** is used to create and manage the data inputs underpinning
the larger fusionACS platform. It facilitates a number of steps in the
overall fusionACS workflow:

1.  **Ingest**: Process raw survey data using a standard approach and
    formatting.
2.  **Document**: Document survey variables and compile a “universal”
    data dictionary.
3.  **Harmonize**: Harmonize variables in “donor” surveys with those in
    the American Community Survey (ACS).
4.  **Compile spatial data**: Compile data from multiple spatial
    datasets for merging with survey microdata.
5.  **Prepare for fusion**: Prepare donor and ACS/recipient microdata
    inputs for the [fusionModel
    package](https://github.com/ummel/fusionModel).

## Setup and install

The fusionData *master* branch can be cloned to its own project
directory on your local machine using RStudio and [these
instructions](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html).

Use the following setup parameters:

``` r
Repository URL: https://github.com/ummel/fusionData
Project directory name: fusionData
```

You will be prompted to enter your Github username and password. After
the repository has been cloned to your local project directory, you can
install and load the package itself. You may be prompted to install some
dependencies.

``` r
# Install the fusionData package locally
devtools::install()

# Load fusionData package
library(fusionData)
```

For full functionality, it is necessary to download the remotely-stored
processed survey microdata and processed spatial data files. The
following section (“Structure and usage”) has more detail about this and
the associated reasoning. See `?getSurveyProcessed` and
`?getGeoProcessed` as well. To get up and running, you’ll need to call
the following commands.

``` r
# Download all remote processed survey microdata files
getSurveyProcessed(survey = "all")

# Download only the essential remote spatial data files
getGeoProcessed(dataset = "essential")
```

You will be prompted to enter the password for the Google Drive account
storing the remote files (password: fusethis!). The download will take a
few minutes. The files are automatically placed in the appropriate
sub-directories of `/fusionData`, with the directories created if
necessary. After successful download, your fusionData “system” is ready
to go.

## Usage and structure

Although fusionData is structured (and loadable) as a R package, it is
better to think of it as a code and data *repository* that is shared and
continuously modified by authorized users. fusionData is expected to
grow over time as new surveys and spatial datasets – and the code needed
to process and manipulate them – are added. fusionData is *not public*.

As you modify code and files in your local `/fusionData` project
directory, you will need to commit and push those changes to the Github
repository for them to be accessible to other users. In addition, it is
good practice to pull the latest version of the repository from Github
prior to making any modifications. That way, you know you are working
from the latest shared version.

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

`.R` scripts defining functions for doing “fusionData things”. Not all
are exported.

### `/data`

Shared, package-wide `.rda` data files. Loadable via `data()`, as usual
for R packages.

### `/data-raw`

`.R` scripts needed to create any package-wide `.rda` objects in
`/data`, as usual for R packages.

### `/man`

Documentation (i.e. “manual”) of functions in `/R` and data in `/data`,
as usual for R packages.

### `/universe`

Directory for the “Universal Survey Dictionary” Shiny app. The app
itself can be run by calling `universe()`.

### `/harmony`

Directory for the “Survey Harmonization Tool” Shiny app. The app itself
can be run by calling `harmony()`.

### `/survey-processed`

Contains processed survey data and associated code. Subdirectories refer
to specific surveys and vintages.

Example: `/survey-processed/RECS/2015`

The Github repository version of `/survey-processed` contains two kinds
of files:

1.  Custom .R scripts that transform *raw* survey microdata (located in
    `/survey-raw`) into “processed” versions that adhere to certain
    requirements, structure, and naming conventions.

Example: `/survey-processed/RECS/2015/RECS_2015_H_processed.R`

2.  “Dictionary” files (.rds) that contain standardized metadata and
    variable descriptions for a particular survey.

Example: `/survey-processed/RECS/2015/RECS_2015_H_dictionary.rds`

`/survey-processed` also includes .fst files containing the processed
microdata itself. These files are stored remotely and can be added to a
user’s local fusionData directory by calling `getSurveyProcessed()`.

For example, the .fst file
`/survey-processed/RECS/2015/RECS_2015_H_processed.fst` contains
processed, household-level microdata for the 2015 RECS. The *code* that
creates this file is found in
`/survey-processed/RECS/2015/RECS_2015_H_processed.R`. The .R file is
part of the Github repository ([see
here](https://github.com/ummel/fusionData/blob/master/survey-processed/RECS/2015/RECS_2015_H_processed.R)),
but the .fst file is stored remotely.

Use of [.fst files](http://www.fstpackage.org/) allows data to be read
very quickly from disk, in part or in full. Functions in the fusionData
package take advantage of this.

### `/survey-raw`

A remote directory (i.e. not present in the Github repository)
containing *raw* survey data files. Subdirectories refer to specific
surveys and vintages.

Example: `/survey-raw/RECS/2015`

`/survey-raw` can be downloaded and added to a user’s local fusionData
directory by calling `getSurveyRaw()`. However, in practice, there is no
reason for a user to store raw survey data locally unless it is for a
survey that they are actively processing or editing. Once the
*processed* version of a survey (`*_processed.fst`) is stable and
uploaded to the remote Google Drive, users can access and use the
processed version without ever needing to download or look at the
original/raw data.

### `/geo-processed`

Contains processed spatial data and associated code. Subdirectories
refer to specific spatial datasets.

Example: `/geo-processed/EPA-SLD`

The Github repository version of `/geo-processed` contains the following
kinds of files:

1.  Custom `.R` scripts that transform *raw* spatial data (located in
    `/geo-raw`) into processed .rds files that meet certain
    requirements.

Example: `/geo-processed/EPA-SLD/epa-sld_v3_processed.R`

`/geo-processed` also includes .rds files containing the processed
spatial data itself. These files are stored remotely and can be added to
a user’s local fusionData directory by calling `getGeoProcessed()`.

For example, the .rds file
`/geo-processed/EPA-SLD/epa-sld_v3_processed.rds` contains processed
spatial variables from version 3 of the EPA’s [Smart Location Database
(SLD)](https://www.epa.gov/smartgrowth/smart-location-mapping#SLD). The
*code* that creates this file is found in
`/geo-processed/EPA-SLD/epa-sld_v3_processed.R`. The .R file is part of
the Github repository ([see
here](https://github.com/ummel/fusionData/blob/master/geo-processed/EPA-SLD/epa-sld_v3_processed.R)),
but the .rds file is stored remotely.

Importantly, the `/geo-processed` remote content *also* includes three
essential spatial data files that are, in practice, all that most users
will need to perform data fusion locally. These files and their roles
are described in more detail later on.

1.  `geo_predictors.fst`
2.  `concordance/geo_concordance.fst`
3.  `concordance/bg_centroids.rds`

For users who are not modifying or adding spatial datasets, it is
sufficient to call `getGeoProcessed(dataset = "essential")` to load the
three essential “geo” files.

### `/geo-raw`

A remote directory (i.e. not present in the Github repository)
containing *raw* spatial data files. Subdirectories refer to specific
spatial datasets.

Example: `/geo-raw/EPA-SLD`

`/geo-raw` can be downloaded and added to a user’s local fusionData
directory by calling `getGeoRaw()`. However, in practice, there is no
reason for a user to store raw spatial data locally unless it is for a
spatial dataset that they are actively processing or editing.

## Ingest survey data

“Ingesting” a survey requires transforming raw survey data into
“processed” (i.e. standardized) microdata files that meet certain
requirements. The fusionData codebase depends on the processed microdata
having recognizable structure and features.

The ingestion process for each survey is documented and defined by a .R
script (possibly multiple scripts) that must be written manually. The
goal is to produce a data.frame containing microdata observations that
(ideally) meet the following conditions:

-   Contains as many observations and variables as possible
-   Variable names and descriptions are taken from the official codebook
-   Codes used in the raw data are replaced with descriptive labels from
    the codebook
-   All “valid blanks” in the raw data are set to plausible values
-   All “invalid blanks” or missing values in the raw data are imputed
-   Ordered factors are used and defined whenever possible (as opposed
    to unordered)
-   Every column contains a variable description as a
    `labelled::var_label` attribute
-   Standard names for unique household/person identifiers and
    observation weights (including replicate weights)
-   Variables identifying respondent location are consistent with those
    defined in `geo-processed/geo_concordance.fst`

Let’s look at a few of the variables in the processed RECS 2015
microdata to get a sense of what preferred output looks like. Note that
the file name includes a `_H_` identifier, indicating that the microdata
in question is household-level. Surveys that include both household and
person-level respondent information have two such files – both “H” and
“P” microdata. The RECS has only household (“H”) microdata.

``` r
recs <- fst::read_fst("survey-processed/RECS/2015/RECS_2015_H_processed.fst")
head(select(recs, recs_2015_hid, weight, sizeofgarage, recs_iecc_zone, rep_1))
```

      recs_2015_hid weight   sizeofgarage           recs_iecc_zone rep_1
    1         10001  12090 Two-car garage IECC climate zones 3B-4B 16560
    2         10002  14400      No garage IECC climate zones 1A-2A 21500
    3         10003  23330      No garage     IECC climate zone 3A 12300
    4         10004  12170 Two-car garage     IECC climate zone 4A 18550
    5         10005  16720 One-car garage     IECC climate zone 5A  8080
    6         10006  26060      No garage IECC climate zones 6A-6B 37000

Notice that the household ID variable has a standardized name
(“recs\_2015\_hid”), as does the observation weights column (“weight”)
and the first of the 96 replicate weights (“rep\_1”). If the microdata
consisted of person-level observations nested within households (e.g. as
in the ACS), it would have have an additional “pid” integer variable to
uniquely identify each person within the household.

The variable “recs\_iecc\_zone” tells us something about a respondent’s
location ([IECC climate
zone](https://basc.pnnl.gov/images/iecc-climate-zone-map)). This and
other spatially-referenced variables are defined and named to be
consistent with variables in the `geo-processed/geo_concordance.fst`
file (more on this file below). This allows subsequent operations to
intelligently impute each respondent’s location (see `?imputePUMA`).

The name of the other variable (“sizeofgarage”) comes from the RECS
codebook. In the case of “sizeofgarage”, the raw data contained NA’s
(valid “skips”) for households without a garage. Those blanks are
replaced with an intelligible label (‘No garage’). In addition,
“sizeofgarage” is classed as an ordered factor, since the labels have a
natural ordering.

``` r
class(recs$sizeofgarage)
```

    [1] "ordered" "factor" 

``` r
levels(recs$sizeofgarage)
```

    [1] "No garage"                "One-car garage"          
    [3] "Two-car garage"           "Three-or-more-car garage"

You can see how, exactly, the raw data was transformed by viewing the
associated code in
`/survey-processed/RECS/2015/RECS_2015_H_processed.R`.

Given the variety of survey data structures and conventions, there is no
strict procedure for how the .R file should be written. However, there
are common steps and tools likely to be applicable to most surveys. The
`RECS_2015_H_processed.R` script is a good “template” in this regard,
since it includes many common operations – including imputation of NA’s
using the provided `imputeMissing()` function.

The RECS 2015 has a comparatively simple microdata and documentation
structure: household-level microdata in a single .csv file with an
associated .xls codebook. Other surveys require more complex steps to
assemble the necessary microdata. There is no limit on the number or
nature of .R files that can be used to ingest a survey. If multiple .R
files are used, the file names should include a two-digit sequence at
the front to indicate the order in which the scripts are employed
(`01*.R`, `02*.R`, etc.).

*The .R files should include liberal use of comments to help others
understand the code later. Good practice is for comments to explain
**why** a piece of code is included, not just **what** it does.*

In all cases, the .R script that eventually saves the `_processed.fst`
microdata file to disk must include the use of
`labelled::set_variable_labels` to assign variable descriptions
(ideally, taken from the official codebook) for each column. The same
script must then call the `createDictionary()` function to create and
save a standardized “dictionary.rds” file. You can see this at the end
of
[RECS\_2015\_H\_processed.R](https://github.com/ummel/fusionData/blob/master/survey-processed/RECS/2015/RECS_2015_H_processed.R).
Here is the resulting dictionary file for RECS 2015.

``` r
recs.dictionary <- readRDS("survey-processed/RECS/2015/RECS_2015_H_dictionary.rds")
head(recs.dictionary)
```

    # A tibble: 6 x 8
      survey vintage respondent variable  description   values           type      n
      <chr>  <chr>   <chr>      <chr>     <chr>         <chr>            <chr> <int>
    1 RECS   2015    H          adqinsul  Level of ins… [Not insulated]… ord    5686
    2 RECS   2015    H          agecdryer Age of cloth… [No clothes dry… ord    5686
    3 RECS   2015    H          agecenac  Age of centr… [No central air… ord    5686
    4 RECS   2015    H          agecwash  Age of cloth… [No clothes was… ord    5686
    5 RECS   2015    H          agedw     Age of dishw… [No dishwasher]… ord    5686
    6 RECS   2015    H          agefrzr   Age of most-… [No freezer], [… ord    5686

In practice, there is no reason for a typical user to ever open a
survey’s dictionary file. The preferred and much more useful way to
explore survey metadata and variable descriptions is described in the
next section.

## Document variables

The previous section showed how and where a survey’s “dictionary.rds”
file(s) are created. Whenever a dictionary file is added or updated, it
is necessary to run the `compileDictionary()` function to compile the
individual survey dictionaries into a single “universal” dictionary. The
usage is straightforward:

``` r
compileDictionary()
```

    ✓ Setting active project to '/home/kevin/Documents/Projects/fusionData'

    ✓ Saving 'dictionary' to 'data/dictionary.rda'

    • Document your data (see 'https://r-pkgs.org/data.html')

    ✓ Saving 'surveys' to 'data/surveys.rda'

    • Document your data (see 'https://r-pkgs.org/data.html')

    dictionary.rds dimensions: 926 x 7

    surveys.rds dimensions: 7 x 5

As the console output tells us, `compileDictionary()` updates two files:
`data/dictionary.rda` and `data/surveys.rda`. These files are part of
the Github repository and are used by both the “Universal Survey
Dictionary” and “Survey Harmonization Tool” that are part of fusionData.

The “Universal Survey Dictionary” is a Shiny app that can be accessed by
the following call:

``` r
# Open "Universal Survey Dictionary" Shiny app
universe()
```

This will open the app in a browser window. The tool allows the
“universe” of available variables – across all ingested surveys – to be
sorted and searched. A user should consult the universal dictionary
after initial ingestion of a new survey, because it is an effective way
to identify variables that need additional editing.

We may eventually make the app public so that potential fusionACS users
can browse the universe of available fusion variables.

## Harmonize variables

The statistical linchpin of the fusion process is the set of
“harmonized” variables common to a donor survey and the ACS. Identifying
conceptually similar variables across surveys and determining how they
can be modified to measure similar concepts is one of the most important
steps in the process. It is also potentially time-consuming and
error-prone.

The “Survey Harmonization Tool” was created to address these problems.
It is a Shiny app that makes it easier to detect, create, and save
“harmonies” among the variables of donor surveys and the ACS. The app
launches in a browser window with the following call:

``` r
# Open "Survey Harmonization Tool" Shiny app
harmony()
```

TO DO: Recording demonstrating app functionality.

When a user clicks “Submit harmony” within the app, the
currently-specified “harmony” (as defined by the selected variables and
settings) is saved to disk. Specifically, the details of that particular
harmony are added to the appropriate .R “harmony file” located at
`/harmony/harmonies`. For example, the file describing how to harmonize
RECS 2015 and ACS 2015 variables is
[RECS\_2015\_\_ACS\_2015.R](https://github.com/ummel/fusionData/blob/master/harmony/harmonies/RECS_2015__ACS_2015.R).
When `dget`-d, harmony files return a list of lists, where each element
defines a harmony. Like this one, defining the harmony between the
“fuelheat” variable in the RECS and the “hfl” variable in the ACS.

``` r
fuelheat__hfl = list(
    RECS = list(
      groups = 1:7,
      levels = c("Do not use space heating", "Electricity", "Fuel oil/kerosene", "Natural gas from underground pipes", "Propane (bottled gas)", "Some other fuel", "Wood (cordwood or pellets)"),
      breaks = "",
      adj = ""),
    ACS = list(
      groups = c(5, 6, 2, 3, 1, 6, 6, 4, 7),
      levels = c("Bottled, tank, or LP gas", "Coal or coke", "Electricity", "Fuel oil, kerosene, etc.", "No fuel used", "Other fuel", "Solar energy", "Utility gas", "Wood"),
      breaks = "",
      adj = ""),
    ordered = FALSE,
    comment = "",
    modified = "2021-07-03 12:10:17"),
    
```

This list object contains all of the information necessary to construct
RECS and ACS microdata containing a new variable called
"fuelheat\_\_hfl" – the harmonized version of the two associated heating
fuel variables. This is precisely what the `harmonize()` function does
(typically when called by `prepare()` as explained below) using all of
the harmonies available in the specified harmony file. Note that
harmonized variables are always indicated by a double-underscore
("\_\_").

This strategy – using the `harmony()` app to manually define harmonies
and then letting `harmonize()` take care of subsequent data manipulation
– makes the construction of harmonized microdata easier, faster, and
*much* safer.

Most users will eventually find themselves constructing harmonies via
the app. Submitted harmonies are saved to *local* harmony files, which
means you must commit and push those changes for them to show up in the
Github repository – and become available for others to use. This also
means it is important to pull the most recent version of the repository
when you begin working with fusionData. Otherwise, you risk duplicating
the efforts of someone else.

## Compile spatial data

fusionData allows for spatially-referenced data to be merged with survey
microdata, thereby expanding the set of potential predictor variables
available in the fusion process. The geographic “unit of analysis” in
this case consists of
[PUMA’s](https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html),
which are observed for ACS households and can be imputed (see
`?imputePUMA`) for donor households.

Ingestion of spatial datasets is generally less onerous than for survey
data; there are fewer requirements that the processed data must meet.
The general strategy will look familiar: Raw spatial data is stored in
`/geo-raw`. The raw data is transformed to a "\*\_processed.rds" file
stored in `/geo-processed`. The associated .R file is stored in the same
location.

A processed spatial .rds file has only two hard requirements it must
meet.

1.  It must contain a “vintage” column indicating the time period of
    each observation. The vintage can be a year, a year range
    (“2015-2016”), or the special value “always”. The “always” value
    indicates that a measurement is time-invariant (e.g. a long-term
    climate “normal”).
2.  It must contain a column (or columns) whose name and values are also
    found in the “geo\_concordance.fst” file.

Ordered factor variables should be classed as such; other categorical
variables can be character. It is not (currently) necessary to document
the variables, name them a certain way, or create a dictionary. Let’s
look at an example.

``` r
irs <- readRDS("geo-processed/IRS-SOI/IRS-SOI_2018_processed.rds")
head(irs[, 1:5])
```

    # A tibble: 6 x 5
      zcta10 vintage `Mean income per ret… `Mean income per pe… `Mean people per re…
      <chr>    <int>                 <int>                <int>                <dbl>
    1 35004     2018                 58600                28760                 2.04
    2 35005     2018                 41200                21200                 1.94
    3 35006     2018                 53100                25300                 2.10
    4 35007     2018                 62300                29240                 2.13
    5 35010     2018                 52900                25700                 2.06
    6 35014     2018                 50300                25900                 1.94

The `irs` object contains processed spatial data constructed from the
[IRS Statistics of Income
(SOI)](https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi)
zip code tax return data for 2018. The underlying raw data is stored
remotely at `/geo-raw/IRS-SOI/2018`. The script used to create the
“processed.rds” file is [available
here](https://github.com/ummel/fusionData/blob/master/geo-processed/IRS-SOI/IRS-SOI_AllVintages_processed.R).
The “zcta10” column indicates the Zip Code Tabulation Area (circa 2010)
associated with each observation.

The “zcta10” variable is also found in the “geo\_concordance.fst” file,
which contains information about how to link geographic units to PUMA’s.
Its creation relies heavily on data from the Missouri Census Data
Center’s [Geocorr
engine](https://mcdc.missouri.edu/applications/geocorr.html) (code
[here](https://github.com/ummel/fusionData/blob/master/geo-processed/concordance/02%20geo_concordance.R)).
The information on how to link zip codes to PUMA’s is used to aggregate
the IRS-SOI data to PUMA-level prior to merging with survey microdata.

The “geo\_concordance.fst” file contains a variety of variables that can
be used to identify the location of observations in a processed spatial
data file. Most of these are documented by
[Geocorr](https://mcdc.missouri.edu/applications/docs/maggot2014.html).
Others were added within the
`geo-processed/concordance/02 geo_concordance.R` file to allow
concordance with variables found in particular datasets. The concordance
file can be expanded over time as necessary.

``` r
concordance <- fst::fst("geo-processed/concordance/geo_concordance.fst")
names(concordance)
```

     [1] "puma10"           "puma_weight"      "state"            "state_name"      
     [5] "state_postal"     "county10"         "cousubfp10"       "tract10"         
     [9] "bg10"             "zcta10"           "cbsa10"           "cbsatype10"      
    [13] "metdiv10"         "csa10"            "sldu10"           "sldl10"          
    [17] "sdbest10"         "sdbesttype10"     "sldu12"           "sldl12"          
    [21] "ur12"             "ua12"             "cbsa13"           "cbsatype13"      
    [25] "metdiv13"         "csa13"            "county14"         "cousubfp14"      
    [29] "sldu14"           "sldl14"           "sdbest14"         "sdbesttype14"    
    [33] "cbsa15"           "cbsatype15"       "metdiv15"         "csa15"           
    [37] "sldu16"           "sldl16"           "cd111"            "cd113"           
    [41] "cd114"            "cd115"            "cd116"            "region"          
    [45] "division"         "recs_domain"      "recs_division"    "recs_ba_zone"    
    [49] "recs_iecc_zone"   "climate_division"

In some cases, the processed .rds file will include multiple variables
to achieve concordance. For example, a spatial dataset with block group
observations must include columns for “state”, “county10”, “tract10”,
and “bg10” in order to allow a smooth merge with the concordance file
(this is the case for the EPA-SLD dataset).

Unlike with processed *survey* data, the naming convention for processed
spatial data files is quite relaxed. The function `compileSpatial()`
automatically detects and compiles all files in `/geo-processed` ending
with "\_processed.rds". As long as a processed spatial data file has the
necessary suffix – and meets the two requirements above – it will be
compiled into the `geo_predictors.fst` file. Whenever a processed .rds
file is added or updated, it is necessary to run `compileSpatial()` to
update the `geo_predictors.fst` file.

The `geo_predictors.fst` file contains all variables and vintages across
available spatial datasets, aggregated to PUMA-level in preparation for
merging with survey microdata. The structure of this file is unusual,
but it is not intended to be worked with directly. It is designed to
allow the `prepare()` function (demonstrated below) to efficiently read
the necessary data from disk when it merges spatial variables for
particular donor and recipient surveys.

Consequently, unless a user is actively adding or editing processed
spatial data, the only “geo files” that are strictly necessary for the
fusion process are `geo_predictors.rds` and `geo_concordance.rds`, both
of which can be obtained by calling
`getGeoProcessed(dataset = "essential")`.

## Prepare for fusion

The following example shows how the fusionData `prepare()` function is
used to assemble complete, consistent, and harmonized microdata that can
then be passed to the [fusionModel
package](https://github.com/ummel/fusionModel) to fuse donor variables
to ACS microdata.

The simplest usage is shown below. In this case, we are requesting
microdata outputs at the household-level that will allow us to
(subsequently) fuse RECS 2015 donor variables to ACS 2015 recipient
microdata.

``` r
# Prepare RECS 2015 household microdata for fusion with ACS 2015 microdata
data <- prepare(donor = "RECS_2015", recipient = "ACS_2015", respondent = "household")
```

The resulting `data` object is a list containing two data frames. The
first slot contains the “prepared” donor microdata. The second slot
contains the “prepared” ACS recipient microdata. Notice that the RECS
microdata has more variables/columns than the ACS data. This is because
`prepare` donor output includes – by default – any variables in the
donor survey not used to create harmonies. The latter are potential
candidates for fusion (and there are many in the case of RECS).

``` r
lapply(data, dim)
```

    $RECS_2015
    [1] 5686  769

    $ACS_2015
    [1] 1226728     233

A key purpose of `prepare()` is to harmonize the donor and recipient
“shared” variables. This is done internally by the `harmonize()`
function, using the variable harmonies created by users via the
`harmony()` tool. In this case, `harmonize()` is using the
[RECS\_2015\_\_ACS\_2015.R](https://github.com/ummel/fusionData/blob/master/harmony/harmonies/RECS_2015__ACS_2015.R)
file to harmonize the donor and recipient microdata. Let’s look at a few
of the shared variables after harmonization.

``` r
v <- names(data$ACS_2015)[2:6]
head(data$RECS_2015[v])
```

      weight bedrooms__bdsp desktop__laptop education__schl elpay__elefp
    1  12090              3               2               2            2
    2  14400              2               1               2            2
    3  23330              4               2               1            2
    4  12170              3               2               4            2
    5  16720              3               2               2            2
    6  26060              0               2               1            1

``` r
head(data$ACS_2015[v])
```

      weight bedrooms__bdsp desktop__laptop education__schl elpay__elefp
    1    110              5               2               4            2
    2     91              4               2               5            2
    3    112              4               2               2            2
    4     80              3               2               3            2
    5    156              3               2               3            2
    6    100              1               2               4            2

Notice that the harmonized variable *values* are typically integers
(possibly factorized); that is, they contain no intelligible labels.
This is because `harmonize()` maps each original value/level to a
(integer) group assignment as specified in the relevant `.R` harmony
file. The one exception is when *numeric* variables in the two surveys
are conceptually identical and can be included “as is” or
(automatically) converted to percentiles.

Since RECS and ACS are both nationally representative surveys, the
distribution of the harmonized variables should look pretty similar
across the two data frames. We can confirm this for the `fuelheat__hfl`
variable, which creates harmony between the RECS and ACS heating fuel
variables (“fuelheat” and “hfl”, respectively). We can compare the
proportion of cases by harmonized value.

``` r
round(table(data$RECS_2015$fuelheat__hfl) / nrow(data[[1]]), 3)
```


        1     2     3     4     5     6     7 
    0.045 0.347 0.043 0.491 0.042 0.001 0.030 

``` r
round(table(data$ACS_2015$fuelheat__hfl) / nrow(data[[2]]), 3)
```


        1     2     3     4     5     6     7 
    0.012 0.364 0.055 0.469 0.064 0.008 0.028 

`prepare()` also merges spatial variables with both the donor and
recipient microdata. The function `assignLocation()` is used internally
to – among other things – impute one or more plausible PUMA’s for each
donor household. Pre-compiled spatial variables (those in
`geo_predictors.fst`) are then merged onto both the donor and recipient
microdata at the PUMA level. Spatial variables are indicated by the
double-dot (“..”) in the variable name, analogous to the way that
harmonized variables are indicated by the double-underscore ("\_\_").

Let’s look at the variables in the recipient ACS microdata.

``` r
names(data$ACS_2015)
```

      [1] "acs_2015_hid"            "weight"                 
      [3] "bedrooms__bdsp"          "desktop__laptop"        
      [5] "education__schl"         "elpay__elefp"           
      [7] "employhh__wkhp"          "fuelheat__hfl"          
      [9] "hhage__agep"             "hhsex__sex"             
     [11] "householder_race__rac1p" "internet__access"       
     [13] "kownrent__ten"           "moneypy__hincp"         
     [15] "ngpay__gasfp"            "numadult__agep"         
     [17] "numchild__agep"          "numfrig__refr"          
     [19] "numtablet__handheld"     "occupyyrange__mv"       
     [21] "sdescent__hisp"          "stoven__stov"           
     [23] "totrooms__rmsp"          "typehuq__bld"           
     [25] "yearmaderange__ybl"      "loc..ur12"              
     [27] "loc..cbsatype15"         "loc..region"            
     [29] "loc..recs_division"      "loc..recs_ba_zone"      
     [31] "loc..recs_iecc_zone"     "acs.pums..npa"          
     [33] "acs.pums..accssywstsb"   "acs.pums..accssywstsc"  
     [35] "acs.pums..acrn"          "acs.pums..acrhl"        
     [37] "acs.pums..acrht"         "acs.pums..anf1"         
     [39] "acs.pums..agsn"          "acs.pums..bthy"         
     [41] "acs.pums..bdsp"          "acs.pums..bldm"         
     [43] "acs.pums..bldofhd"       "acs.pums..bldofhm"      
     [45] "acs.pums..bld5"          "acs.pums..brdy"         
     [47] "acs.pums..bsnf"          "acs.pums..bsys"         
     [49] "acs.pums..cmpy"          "acs.pums..dlpy"         
     [51] "acs.pums..dsly"          "acs.pums..elep"         
     [53] "acs.pums..fbry"          "acs.pums..fsys"         
     [55] "acs.pums..flpx"          "acs.pums..gspy"         
     [57] "acs.pums..hndy"          "acs.pums..hflu"         
     [59] "acs.pums..hfle"          "acs.pums..hflf"         
     [61] "acs.pums..insp"          "acs.pums..lpty"         
     [63] "acs.pums..mdmy"          "acs.pums..mrgnb"        
     [65] "acs.pums..mrgyp"         "acs.pums..mrgp"         
     [67] "acs.pums..mrgtn"         "acs.pums..mrgty"        
     [69] "acs.pums..mrgxn"         "acs.pums..mrgm"         
     [71] "acs.pums..mrgc"          "acs.pums..othy"         
     [73] "acs.pums..rfry"          "acs.pums..rmsp"         
     [75] "acs.pums..rntn"          "acs.pums..rntp"         
     [77] "acs.pums..rwty"          "acs.pums..stly"         
     [79] "acs.pums..snky"          "acs.pums..stvy"         
     [81] "acs.pums..tlysx"         "acs.pums..tnow"         
     [83] "acs.pums..tnof"          "acs.pums..tnrn"         
     [85] "acs.pums..tlysb"         "acs.pums..vlpc"         
     [87] "acs.pums..vhnv"          "acs.pums..vh1v"         
     [89] "acs.pums..vh2v"          "acs.pums..vh3v"         
     [91] "acs.pums..wtph"          "acs.pums..yb19"         
     [93] "acs.pums..y194"          "acs.pums..y195"         
     [95] "acs.pums..y196"          "acs.pums..y197"         
     [97] "acs.pums..y198"          "acs.pums..y199"         
     [99] "acs.pums..y202"          "acs.pums..fsnf"         
    [101] "acs.pums..fsmcfhw"       "acs.pums..fsmcfhl"      
    [103] "acs.pums..fsmcfn"        "acs.pums..fsof"         
    [105] "acs.pums..fncp"          "acs.pums..fprn"         
    [107] "acs.pums..fwr5"          "acs.pums..fw51"         
    [109] "acs.pums..fw55"          "acs.pums..grnt"         
    [111] "acs.pums..grpp"          "acs.pums..hhle"         
    [113] "acs.pums..hhls"          "acs.pums..hhtm"         
    [115] "acs.pums..hhto"          "acs.pums..hhtnhm"       
    [117] "acs.pums..hhtnhf"        "acs.pums..hncp"         
    [119] "acs.pums..hgch"          "acs.pums..hwc6"         
    [121] "acs.pums..hw61"          "acs.pums..hw66"         
    [123] "acs.pums..hpc6"          "acs.pums..hpc61"        
    [125] "acs.pums..hpc661"        "acs.pums..hpr6"         
    [127] "acs.pums..hpr61"         "acs.pums..hpr661"       
    [129] "acs.pums..ktyh"          "acs.pums..lal1"         
    [131] "acs.pums..mltn"          "acs.pums..mv1m"         
    [133] "acs.pums..m1t2"          "acs.pums..m2t4"         
    [135] "acs.pums..m5t9"          "acs.pums..m1t1"         
    [137] "acs.pums..m2t2"          "acs.pums..nocc"         
    [139] "acs.pums..npfd"          "acs.pums..nppn"         
    [141] "acs.pums..nrnn"          "acs.pums..nrcg"         
    [143] "acs.pums..ocpp"          "acs.pums..prtn"         
    [145] "acs.pums..plmy"          "acs.pums..psfn"         
    [147] "acs.pums..r18n"          "acs.pums..r60n"         
    [149] "acs.pums..r601"          "acs.pums..r65n"         
    [151] "acs.pums..r651"          "acs.pums..rsmm"         
    [153] "acs.pums..rsmc"          "acs.pums..smcp"         
    [155] "acs.pums..smxnbb"        "acs.pums..smxnob"       
    [157] "acs.pums..ssmh"          "acs.pums..txpn"         
    [159] "acs.pums..wfnn"          "acs.pums..wfnw"         
    [161] "acs.pums..wf1w"          "acs.pums..wf2w"         
    [163] "acs.pums..wkxn"          "acs.pums..wkxrlhs"      
    [165] "acs.pums..wkxrlhwftsw"   "acs.pums..wkxrlhwftsd"  
    [167] "acs.pums..wkxrlhd"       "acs.pums..wkxf"         
    [169] "acs.pums..wrksttnf"      "acs.pums..wrkstthw"     
    [171] "acs.pums..wrkstthl"      "acs.pums..wrksttnh"     
    [173] "acs.pums..wrkf"          "acs.pums..elfv"         
    [175] "acs.pums..flfi"          "acs.pums..flfn"         
    [177] "acs.pums..gsfpip"        "acs.pums..gsfpir"       
    [179] "acs.pums..gsfn"          "acs.pums..wtfi"         
    [181] "acs.pums..wtfn"          "acs.sf..b010"           
    [183] "acs.sf..b060"            "acs.sf..b080"           
    [185] "acs.sf..b1900"           "acs.sf..b1910"          
    [187] "acs.sf..b1920"           "acs.sf..b25010"         
    [189] "acs.sf..b250350"         "acs.sf..b250390"        
    [191] "acs.sf..b25060"          "acs.sf..b250710"        
    [193] "acs.sf..b250770"         "acs.sf..b25080"         
    [195] "acs.sf..b25090"          "acs.sf..b2510"          
    [197] "climate..cddb6"          "climate..hddb6"         
    [199] "climate..cdd12b6"        "climate..hdd12b6"       
    [201] "climate..iccz"           "climate..bznf"          
    [203] "eia.seds..gslp"          "eia.seds..elcp"         
    [205] "eia.seds..ntgp"          "eia.seds..lpgp"         
    [207] "eia.seds..fllp"          "eia.seds..elmh"         
    [209] "eia.seds..ntth"          "eia.seds..lpgh"         
    [211] "eia.seds..flgh"          "irs.soi..mipr"          
    [213] "irs.soi..mipp"           "irs.soi..mppr"          
    [215] "irs.soi..mdpr"           "irs.soi..prsr"          
    [217] "irs.soi..prjr"           "irs.soi..phohr"         
    [219] "irs.soi..pppr"           "irs.soi..pvpr"          
    [221] "irs.soi..prcntelr"       "irs.soi..prfr"          
    [223] "irs.soi..prcntetr"       "irs.soi..prie"          
    [225] "irs.soi..priu"           "irs.soi..eftr"          
    [227] "irs.soi..palt2"          "irs.soi..pa2t5"         
    [229] "irs.soi..pa5t7"          "irs.soi..pa7t1"         
    [231] "irs.soi..pa1t2"          "irs.soi..pa2om"         
    [233] "nrel.urdb..rsed"        

The string to the left of the “..” identifies the spatial dataset that
the variable comes from. The string to the right is a unique,
syntactically-valid identifier. It’s not critical that specific spatial
variables be identifiable in the fusion process. And because
pre-processing of spatial datasets does not impose a stringent
naming/documentation convention (it is flexible by design), these
non-nonsensical-but-unique names are the safest way to identify spatial
variables.

Now let’s explore some of the additional arguments to `prepare()`:

1.  We can pass unquoted donor variable names and/or selectize
    statements to the `...` argument if we want to return a specific set
    of fusion variables for the donor (instead of all variables, the
    default behavior).

2.  The `implicates` argument controls how many PUMA’s are imputed for
    each donor household. Setting `implicates` higher results in more
    variability in the spatial predictors merged to a given household,
    reflecting our uncertainty about where the household is located. The
    use of implicates here mimics usage in standard multiple imputation
    techniques (5 implicates is typical).

3.  We can limit the spatial datasets merged to the microdata via the
    `spatial.datasets` argument. Default is to include all available
    datasets, and this is sensible in most cases.

4.  The `window` argument controls how wide a timespan we tolerate when
    merging spatial variables to microdata. The default (`window = 0`)
    means that spatial variables are merged only when their vintage
    matches that of the microdata. A larger `window` will generally mean
    more spatial variables in the output but at some cost in terms of
    temporal alignment.

5.  The `pca` argument controls whether/how principal components
    analysis (PCA) is used to reduce dimensionality of the spatial
    variables. `?prepare` provides additional details concerning the
    `pca` argument.

The following shows a more complex (and realistic) call to `prepare()`,
making use of the optional arguments.

``` r
# Prepare RECS 2015 household microdata for fusion with ACS 2015 microdata
data <- prepare(donor = "RECS_2015", 
                recipient = "ACS_2015", 
                respondent = "household",
                cooltype, agecenac, kwhcol,  # Request specific donor variables
                implicates = 5,
                window = 3,
                pca = c(30, 0.9))
```

``` r
lapply(data, dim)
```

    $RECS_2015
    [1] 26274    66

    $ACS_2015
    [1] 1226728      63

The number of observations in the donor microdata is now higher,
reflecting the use of `implicates = 5`. Note that the number of rows has
increased by less than a factor of five. This is because
`assignLocation()` collapses duplicate household-PUMA observations and
adjusts the sample “weight” column accordingly. This reduces the amount
of data without affecting subsequent statistical results.

The difference in the number of columns between the RECS and ACS
microdata is due to the former’s inclusion of our three requested donor
variables. Otherwise, both data frames are entirely consistent with one
another. They each include a unique household identifier variable and an
identical set of harmonized survey and spatial variables that can be
exploited by the fusion process.

By specifying the `pca` argument, the *numeric* spatial variables are
collapsed into a smaller number of components – indicated by the “pca..”
prefix – which we can confirm by looking at the recipient column names.

``` r
names(data$ACS_2015)
```

     [1] "acs_2015_hid"            "weight"                 
     [3] "bedrooms__bdsp"          "desktop__laptop"        
     [5] "education__schl"         "elpay__elefp"           
     [7] "employhh__wkhp"          "fuelheat__hfl"          
     [9] "hhage__agep"             "hhsex__sex"             
    [11] "householder_race__rac1p" "internet__access"       
    [13] "kownrent__ten"           "moneypy__hincp"         
    [15] "ngpay__gasfp"            "numadult__agep"         
    [17] "numchild__agep"          "numfrig__refr"          
    [19] "numtablet__handheld"     "occupyyrange__mv"       
    [21] "sdescent__hisp"          "stoven__stov"           
    [23] "totrooms__rmsp"          "typehuq__bld"           
    [25] "yearmaderange__ybl"      "loc..ur12"              
    [27] "loc..cbsatype15"         "loc..region"            
    [29] "loc..recs_division"      "loc..recs_ba_zone"      
    [31] "loc..recs_iecc_zone"     "climate..iccz"          
    [33] "climate..bznf"           "pca..PC1"               
    [35] "pca..PC2"                "pca..PC3"               
    [37] "pca..PC4"                "pca..PC5"               
    [39] "pca..PC6"                "pca..PC7"               
    [41] "pca..PC8"                "pca..PC9"               
    [43] "pca..PC10"               "pca..PC11"              
    [45] "pca..PC12"               "pca..PC13"              
    [47] "pca..PC14"               "pca..PC15"              
    [49] "pca..PC16"               "pca..PC17"              
    [51] "pca..PC18"               "pca..PC19"              
    [53] "pca..PC20"               "pca..PC21"              
    [55] "pca..PC22"               "pca..PC23"              
    [57] "pca..PC24"               "pca..PC25"              
    [59] "pca..PC26"               "pca..PC27"              
    [61] "pca..PC28"               "pca..PC29"              
    [63] "pca..PC30"              

## Make it rain

At this point, we are ready to fuse. This is straightforward using the
`train()` and `fuse()` functions from the fusionModel package. To make
this even easier, `prepare()` donor output includes a “fusion.vars”
attribute that can be passed directly to `train()` to identify the
variables available to be fused.

``` r
fit <- fusionModel::train(data = data$RECS_2015, 
                          y = attr(data$RECS_2015, "fusion.vars"), 
                          ignore = "recs_2015_hid", 
                          weight = "weight",
                          mc = TRUE,
                          maxcats = 10,
                          lasso = 0.95)
```

    Warning in fusionModel::train(data = data$RECS_2015, y = attr(data$RECS_2015, :
    Missing values were imputed for the following variables: loc..cbsatype15

We then `fuse()` (i.e. simulate) the fusion variables onto the
harmonized ACS microdata. This is a non-trivial exercise, since the
recipient ACS microdata has 1226728 observations. Setting
`induce = FALSE` eases the computation and memory burden considerably.
The call below shouldn’t require more than about 5GB of RAM.

``` r
sim <- fusionModel::fuse(data = data$ACS_2015, 
                         train.object = fit, 
                         induce = FALSE)
```

    Warning in fusionModel::fuse(data = data$ACS_2015, train.object = fit, induce =
    FALSE): Missing values were imputed for the following variables: loc..cbsatype15

A quick check that the fusion output looks plausible:

``` r
nrow(sim)
```

    [1] 1226728

``` r
head(sim)
```

         kwhcol                   agecenac                                 cooltype
    1  900.0472          20 years or older          Central air conditioning system
    2 4704.8795 No central air conditioner Individual window/wall or portable units
    3 1346.4376           5 to 9 years old          Central air conditioning system
    4 2715.9629           5 to 9 years old          Central air conditioning system
    5  439.1443         10 to 14 years old          Central air conditioning system
    6    0.0000 No central air conditioner                      No air conditioning

``` r
summary(data$RECS_2015$kwhcol)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        0.0   390.5  1122.0  1882.0  2554.0 20350.0 

``` r
summary(sim$kwhcol)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        0.0   391.8  1113.7  1825.6  2485.1 20350.0 

Onward and upward!
