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
    inputs passed to the [fusionModel
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
storing the remote files.

-   username: fusionACSdata
-   password: fusethis!

The download will take a few minutes. The files are automatically placed
in the appropriate sub-directories of `/fusionData`, with the
directories created if necessary. After successful download, your
fusionData “system” is ready to go.

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
“essential” spatial data files that are, in practice, all that most
users will need to perform data fusion locally. These files and their
roles are described in more detail later on.

1.  `geo_predictors.fst`
2.  `concordance/geo_concordance.fst`

For users who are not modifying or adding spatial datasets, it is
sufficient to call `getGeoProcessed(dataset = "essential")` to load the
essential “geo” files.

### `/geo-raw`

A remote directory (i.e. not present in the Github repository)
containing *raw* spatial data files. Subdirectories refer to specific
spatial datasets.

Example: `/geo-raw/EPA-SLD`

`/geo-raw` can be downloaded and added to a user’s local fusionData
directory by calling `getGeoRaw()`. However, in practice, there is no
reason for a user to store raw spatial data locally unless it is for a
spatial dataset that they are actively processing or editing.

### `/production`

Directory containing code and possibly data from “production” fusion
runs. This is likely a temporary inclusion. As additional fusion results
are produced, we should create a more structured way of storing and
organizing production data outputs.

## Ingest survey data

“Ingesting” a survey requires transforming raw survey data into
“processed” (i.e. standardized) microdata files that meet certain
requirements. The fusionData codebase depends on the processed microdata
having recognizable structure and features.

The ingestion process for each survey is documented and defined by a .R
script (possibly multiple scripts) that must be written manually. The
goal is to produce a data.frame containing microdata observations that
(ideally) meet the following conditions:

-   Contains as many observations and variables as possible.
-   Variable names and descriptions are taken from the official
    codebook, possibly modified for clarity.
-   Official variable names are coerced to lower-case alphanumeric,
    possibly using single underscores.
-   Codes used in the raw data are replaced with descriptive labels from
    the codebook; e.g. integer values are replaced with associated
    factor levels.
-   All “valid blanks” in the raw data are set to plausible values; NA’s
    are often actual zeros or some other knowable value based on the
    question structure.
-   All “invalid blanks” or missing values in the raw data are imputed;
    a generic imputation function is provided for this purpose.
-   Ordered factors are used and defined whenever possible (as opposed
    to unordered).
-   Standard column names are used for unique household identifiers
    (e.g. “acs\_2019\_hid”); for person-level microdata the
    within-household person identifier (integer) is always “pid”.
-   Standard column names are used for observation weights; “weight” for
    the primary weighting variable and “rep\_1”, etc. for replicate
    weights.
-   Variables identifying respondent location are consistent with those
    defined in `geo-processed/concordance/geo_concordance.fst`.

Let’s look at a few of the variables in the processed RECS 2015
microdata to get a sense of what preferred output looks like. Note that
the file name includes a `_H_` identifier, indicating that the microdata
in question is household-level. Surveys that include both household and
person-level respondent information have two such files – both “H” and
“P” microdata. The RECS has only household (“H”) microdata.

``` r
recs <- fst::read_fst("survey-processed/RECS/2015/RECS_2015_H_processed.fst")
head(select(recs, recs_2015_hid, weight, rep_1, sizeofgarage, recs_iecc_zone))
```

      recs_2015_hid weight rep_1   sizeofgarage           recs_iecc_zone
    1         10001  12090 16560 Two-car garage IECC climate zones 3B-4B
    2         10002  14400 21500      No garage IECC climate zones 1A-2A
    3         10003  23330 12300      No garage     IECC climate zone 3A
    4         10004  12170 18550 Two-car garage     IECC climate zone 4A
    5         10005  16720  8080 One-car garage     IECC climate zone 5A
    6         10006  26060 37000      No garage IECC climate zones 6A-6B

Notice that the household ID variable has a standardized name
(“recs\_2015\_hid”), as does the observation weights column (“weight”)
and the first of the 96 replicate weights (“rep\_1”). If the microdata
consisted of person-level observations nested within households (e.g. as
in the ACS), it would have have an additional “pid” integer variable to
uniquely identify each person within the household.

In the case of “sizeofgarage” (the original variable name in RECS), the
raw data contained NA’s (valid “skips”) for households without a garage.
Those blanks are replaced with an intelligible label (“No garage”). In
addition, “sizeofgarage” is classed as an ordered factor, since the
labels have a natural ordering.

``` r
class(recs$sizeofgarage)
```

    [1] "ordered" "factor" 

``` r
levels(recs$sizeofgarage)
```

    [1] "No garage"                "One-car garage"          
    [3] "Two-car garage"           "Three-or-more-car garage"

The variable “recs\_iecc\_zone” tells us something about each
respondent’s location ([IECC climate
zone](https://basc.pnnl.gov/images/iecc-climate-zone-map)). This and
other spatially-referenced variables are defined and named to be
consistent with variables in the
`geo-processed/concordance/geo_concordance.fst` file. This allows
subsequent operations to intelligently impute respondent location prior
to fusion. More details on spatial data and location imputation can be
found in subsequent sections.

It is important that the location variables in a donor survey be
precisely consistent with those defined in `geo_concordance.fst`. The
latter file can be modified, if necessary, to add new location variables
to allow such concordance. It is only strictly necessary that the donor
survey include the variable (or set of variables) that provide maximum
information about respondent location. For example, if a survey
contained a “county” variable, there is no reason to include “state” –
though the code shouldn’t break if it is included.

Some surveys (like RECS) have a complicated combination of location
variables that collectively define respondent location through their
spatial intersection. fusionData’s code base automatically handles this,
provided that the location variables are consistent (i.e. same name and
levels) in both the donor microdata and the `geo_concordance.fst` file.
There is no need to specify which variables are the location variables;
this is determined automatically by looking for overlap with column
names in the `geo_concordance.fst` file.

You can see how, exactly, the raw survey data was transformed by viewing
the associated code in
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
(ideally, taken from the official codebook) for each column. The script
must then call the `createDictionary()` function to create and save a
standardized “dictionary.rds” file. `createDictionary()` uses the
assigned variable descriptions and other information in the microdata to
build the dictionary in a standardized way. You can see this at the end
of
[RECS\_2015\_H\_processed.R](https://github.com/ummel/fusionData/blob/master/survey-processed/RECS/2015/RECS_2015_H_processed.R):

Here is the resulting dictionary file for RECS 2015.

``` r
recs.dictionary <- readRDS("survey-processed/RECS/2015/RECS_2015_H_dictionary.rds")
head(recs.dictionary)
```

    # A tibble: 6 × 8
      survey vintage respondent variable  description    values          type      n
      <chr>  <chr>   <chr>      <chr>     <chr>          <chr>           <chr> <int>
    1 RECS   2015    H          adqinsul  Level of insu… [Not insulated… ord    5686
    2 RECS   2015    H          agecdryer Age of clothe… [No clothes dr… ord    5686
    3 RECS   2015    H          agecenac  Age of centra… [No central ai… ord    5686
    4 RECS   2015    H          agecwash  Age of clothe… [No clothes wa… ord    5686
    5 RECS   2015    H          agedw     Age of dishwa… [No dishwasher… ord    5686
    6 RECS   2015    H          agefrzr   Age of most-u… [No freezer], … ord    5686

In practice, there is no reason for a typical user to ever open a
survey’s dictionary file. The preferred and much more useful way to
explore survey metadata and variable descriptions is via the
`universe()` function described in the next section.

*As of August 2021, American Community Survey (ACS) microdata has been
ingested for 2015 and 2019.* It is expected that additional vintages
will eventually be added to allow temporal alignment with the further
vintages of donor surveys. The .R scripts used to process the 2015 and
2019 ACS microdata can serve as templates for ingesting other vintages.

## Document variables

The previous section showed how and where a survey’s “dictionary.rds”
file(s) are created. Whenever a dictionary file is added or updated, it
is necessary to run the `compileDictionary()` function to compile all of
fusionData’s individual survey dictionaries into a single “universal”
dictionary. The usage is straightforward:

``` r
compileDictionary()
```

    ✓ Setting active project to '/home/kevin/Documents/Projects/fusionData'

    ✓ Saving 'dictionary' to 'data/dictionary.rda'

    • Document your data (see 'https://r-pkgs.org/data.html')

    ✓ Saving 'surveys' to 'data/surveys.rda'

    • Document your data (see 'https://r-pkgs.org/data.html')

    dictionary.rds dimensions: 958 x 7

    surveys.rds dimensions: 7 x 5

As the console output tells us, `compileDictionary()` updates two files:
`data/dictionary.rda` and `data/surveys.rda`. These files are part of
the Github repository and are used by both the “Universal Survey
Dictionary” and “Survey Harmonization Tool” Shiny apps that are part of
fusionData.

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

Once a donor survey has been successfully ingested and documented, it is
possible to start thinking about how to fuse that survey to the ACS.

The statistical linchpin of the fusion process is the set of
“harmonized” variables common to a donor survey and the ACS. Identifying
conceptually similar variables across surveys and determining how they
can be modified to measure similar concepts is one of the most important
steps in the process. It is also potentially time-consuming and
error-prone.

The “Survey Harmonization Tool” was created to make this process easier
and safer. It is a Shiny app that makes it easier to detect, specify,
and save “harmonies” constructed between variables in donor surveys and
variables in the ACS. The app launches in a browser window with the
following call:

``` r
# Open "Survey Harmonization Tool" Shiny app
harmony()
```

At present, the `harmony()` app only allows specification of harmonies
between a non-ACS donor survey and a specific ACS vintage
(e.g. harmonizing 2015 RECS to 2015 ACS).

Construction of a harmony generally follows these steps:

1.  Select a donor survey and vintage.
2.  Select the recipient ACS vintage.
3.  Select a “Donor variable” from the drop down list. The list is
    searchable to help locate variables associated with certain words.
4.  Select a “ACS variable” to use for the “other side” of the harmony.
5.  For *factor* variables, edit the “Group” columns in the spreadsheet
    objects to create the maximum-resolution harmony between the two
    variables. You can see the “live” outcome of the specified
    harmonization strategy in the table at the bottom of the page. For
    *continuous* variables, no additional modification is needed as long
    as the two variables measure similar concepts.
6.  Once the harmony is specified as you like, click “Submit harmony”.
    The button only becomes available to click if minimal safety checks
    are passed for a valid harmony.

When a user clicks “Submit harmony”, the currently-specified harmony (as
defined by the selected variables and settings) is saved to disk.
Specifically, the details of that particular harmony are added to the
appropriate .R “harmony file” located at `/harmony/harmonies`. For
example, the file describing how to harmonize RECS 2015 and ACS 2015
variables is
[RECS\_2015\_\_ACS\_2015.R](https://github.com/ummel/fusionData/blob/master/harmony/harmonies/RECS_2015__ACS_2015.R).

You should receive a pop-up message indicating if the harmony was
successfully added to the local .R harmony file (it will be created, if
necessary). You can confirm the harmony was added by selecting the “View
harmonies” panel.

Probably the easiest way to become an expert with the app is to view
existing harmonies that I’ve already constructed for the RECS and CEI.
This will show you the settings used and give you a sense of how and why
they were used.

------------------------------------------------------------------------

Additional details regarding “advanced” settings and examples. This may
all seem convoluted at first. But once you understand what these fields
are doing, it becomes quite easy, fast, and (almost) fun to construct
harmonies.

*Bin breakpoints*

The Bin breakpoints field is used to specify how a continuous variable
should be “binned” to turn it into a categorical variable – usually to
allow for harmonization with a factor variable in the other survey. This
is useful when an identical concept is measured on a continuous scale in
one survey (e.g. income in dollars) and as a factor variable in the
other survey (e.g. income range).

Example: Select “moneypy” for RECS 2015 to see how the ACS “hincp” is
binned to create harmony.

*Adjustment*

The Adjustment field provides a powerful way to modify or adjust
variables to accommodate non-standard harmonies. Text in the Adjustment
field is passed as-is to a dplyr::mutate() call within `harmonize()`
that modifies the associated variable prior to any other manipulation.
The text passed to the mutate() call can utilize any other variables in
the microdata. This is quite powerful and allows for complicated
harmonies to be accommodated.

Example: Select “vehq” for CEI 2015-2019. In this case, the “vehq”
(owned vehicles) and “vehql” (leased vehicles) variables in the CEI –
both continuous – are added together by specifying “vehq + vehql” in the
Adjustment field. The result is then binned to create harmony with the
ACS “veh” variable, which is a factor variable referring to all
available vehicles, whether owned or leased.

*Household aggregator*

Sometimes it is possible to create harmony between a household-level
(“H”) donor variable and a person-level (“P”) ACS variable, provided
that the latter is *aggregated* at the household level. In such cases,
the “Household aggregator” field tells `harmonize()` how to aggregate or
summarize the person-level ACS variable within each household. This
field is only applicable when the donor variable is a household variable
and the selected ACS variable is person-level (person-level donor
variables can always be harmonized directly with person-level ACS
variables).

Simple example: Select “hhage” for RECS 2015
(Respondent/head-of-household age). The ACS “agep” variable can be used
to create harmony, but it is a person-level variable. By setting the
Household aggregator field to “reference” we instruct `harmonize()` to
use the “reference person” value for “agep” to create a household-level
variable analogous to “hhage”.

Advanced example: Select “numchild” for RECS 2015 (Number of household
members age 17 or younger). The ACS “agep” variable can be used to
create harmony. Bin breakpoints are used to re-assign each household
member “agep” value to 1 if less than 18 and 0 if 18 or more (see the
“Group” column in the associated spreadsheet). The Household aggregator
field is the set to “sum” to instruct `harmonize()` to sum these values
at the household level, which creates harmony with “numchild”.

Very advanced example: Select “as\_comp1” for CEI 2015-2019 (Number of
males age 16 and over). Again, the ACS “agep” variable can be used to
create harmony (as above for “numchild”), but we need to additionally
restrict the harmony to males only. This is done by using the Adjustment
field to first set “agep” to 0 for all females, *then* bin the result,
*then* sum at the household level.

*Comments*

The Comments field is used to leave helpful information about why the
harmony was constructed as it was. Any harmony that makes use of one of
the “advanced” settings should probably have a comment explaining the
rationale.

------------------------------------------------------------------------

A harmony file can be `dget`-d to return a list of lists, where each
element defines a harmony. Like this one, defining the harmony between
the “fuelheat” variable in the RECS and the “hfl” variable in the ACS.

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
"fuelheat\_\_hfl“; i.e. the harmonized version of the two associated
heating fuel variables. This is precisely what the `harmonize()`
function does – typically when called by `prepare()` as explained below
– using all of the harmonies available in the specified harmony file.
Note that harmonized variables are always indicated by a
double-underscore (”\_\_").

Using the `harmony()` app to manually define harmonies and then letting
`harmonize()` take care of subsequent data manipulation makes the
construction of harmonized microdata easier, faster, and *much* safer.
It is generally advisable to use the `harmonize()` app to create and
edit harmonies. It is also possible to manually edit the .R harmony
files, if necessary, but be careful.

Most users will eventually find themselves constructing harmonies via
the app and, as a result, modifying their *local* version of .R harmony
files. This means you must commit and push those changes for them to
show up in the Github repository – and become available for others to
use. This also means it is important to pull the most recent version of
the repository when you begin working with fusionData. Otherwise, you
risk duplicating the efforts of someone else and/or failing to make use
of the most recent version of harmony (and other) files.

## Compile spatial data

fusionData allows for spatially-referenced data to be merged with survey
microdata, thereby expanding the set of potential predictor variables
available in the fusion process. The geographic “unit of analysis” in
this case consists of
[PUMA’s](https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html),
which are observed for ACS households and can be imputed for donor
households.

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
    found in the `geo_concordance.fst` file. These columns define the
    location of the measurement and – via the `geo_concordance.fst` file
    – are mapped to PUMA’s.

Ordered factor variables should be classed as such; other categorical
variables can be character. It is *not* (currently) necessary to
document the variables, name them a certain way, or create a dictionary.
Let’s look at an example.

``` r
irs <- readRDS("geo-processed/IRS-SOI/IRS-SOI_2018_processed.rds")
head(irs[, 1:5])
```

    # A tibble: 6 × 5
      zcta10 vintage `Mean income per return` `Mean income per p… `Mean people per …
      <chr>    <int>                    <int>               <int>              <dbl>
    1 35004     2018                    58600               28760               2.04
    2 35005     2018                    41200               21200               1.94
    3 35006     2018                    53100               25300               2.10
    4 35007     2018                    62300               29240               2.13
    5 35010     2018                    52900               25700               2.06
    6 35014     2018                    50300               25900               1.94

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
engine](https://mcdc.missouri.edu/applications/geocorr.html). The
information on how to link zip codes to PUMA’s is used to aggregate the
IRS-SOI data to PUMA-level prior to merging with survey microdata.

The `geo_concordance.fst` file contains a variety of variables that can
be used to identify the location of observations in a processed spatial
data file. Most of these are [documented by
Geocorr](https://mcdc.missouri.edu/applications/docs/maggot2014.html).
Others were added within the
[`geo-processed/concordance/geo_concordance.R`](https://github.com/ummel/fusionData/blob/master/geo-processed/concordance/geo_concordance.R)
file to allow concordance with variables found in particular datasets.
The concordance file can be expanded over time as necessary.

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

In some cases, the spatial dataset’s processed .rds file will include
multiple location variables that are used collectively to achieve
spatial concordance. For example, a spatial dataset with block group
observations must include columns for “state”, “county10”, “tract10”,
and “bg10” in order to allow a smooth merge with the concordance file
(this is the case for the EPA-SLD dataset).

Unlike with processed *survey* data, the naming convention for processed
spatial data files is quite relaxed. The function `compileSpatial()`
automatically detects and compiles all files in `/geo-processed` ending
with "\_processed.rds". As long as a processed spatial data file has the
necessary suffix – and meets the two hard requirements mentioned above –
it will be compiled into the `geo_predictors.fst` file.

Whenever a processed .rds file is added or updated, it is necessary to
run `compileSpatial()` to update the `geo_predictors.fst` file.

The `geo_predictors.fst` file contains all variables and vintages across
available spatial datasets, aggregated to PUMA-level in preparation for
merging with survey microdata. The structure of this file is unusual,
but it is not intended to be worked with directly. It is designed to
allow the `assemble()` function (demonstrated below) to efficiently read
the necessary data from disk when it merges spatial variables for
particular donor and recipient surveys.

Consequently, unless a user is actively adding or editing processed
spatial data, the only “geo files” that are strictly necessary for the
fusion process are `geo_predictors.fst` and `geo_concordance.fst`, both
of which can be obtained by calling
`getGeoProcessed(dataset = "essential")`.

## Prepare for fusion

The following example shows how the fusionData `prepare()` and
`assemble()` functions to generate complete, consistent, and harmonized
microdata that can then be passed to the [fusionModel
package](https://github.com/ummel/fusionModel) to fuse donor variables
to ACS microdata.

The simplest usage is shown below. In this case, we are requesting
microdata outputs at the household-level that will allow us to
(subsequently) fuse RECS 2015 donor variables to ACS 2015 recipient
microdata. `prepare()` and `assemble()` are separate functions only
because `prepare()` tends to be more expensive. It can usually be called
just once (possibly saving the output to disk), while `assemble()` is
then invoked multiple times to debug or test different assembly options.

``` r
# Prepare RECS 2015 household microdata for fusion with ACS 2015 microdata
prep <- prepare(donor = "RECS_2015", recipient = "ACS_2015", respondent = "household")
```

    Harmonizing RECS_2015 (donor) microdata at household level
    Harmonizing ACS_2015 (recipient) microdata at household level
    Identified 124 geographic intersections in the donor...
    Imputing PUMA for donor observations...
    Assigning location variables to recipient observations...

``` r
# Assemble using default options
data <- assemble(prep)
```

    Identifying donor fusion variables...
    Adding the following fusion variables:
     adqinsul, agecdryer, agecenac, agecwash, agedw, agefrzr, agerfri1, agerfri2, aircond, altfuelpev, amtmicro, appother, athome, attccool, attcheat, attic, atticfin, audit, auditchg, backup, basecool, basefin, baseheat, benother, blender, btuel, btuelahucol, btuelahuheat, btuelcdr, btuelcfan, btuelcok, btuelcol, btuelcw, btueldhum, btueldwh, btuelevapcol, btuelfrz, btuelhtbheat, btuelhtbpmp, btuelhum, btuellgt, btuelmicro, btuelnec, btuelplpmp, btuelrfg, btuelrfg1, btuelrfg2, btuelsph, btueltv1, btueltv2, btueltvrel, btuelwth, btufo, btufonec, btufosph, btufowth, btulp, btulpcdr, btulpcok, btulpnec, btulpsph, btulpwth, btung, btungcdr, btungcok, btunghtbheat, btungnec, btungplheat, btungsph, btungwth, cablesat, cdd30yr, cdd65, cdd80, cellar, cellphone, cenachp, coffee, coldma, combodvr, cooktuse, cooltype, crockpot, cufeetng, cufeetngcdr, cufeetngcok, cufeetnghtbheat, cufeetngnec, cufeetngplheat, cufeetngsph, cufeetngwth, cwasher, dbt1, dbt99, dishwash, dntheat, dolelahucol, dolelahuheat, dolelcdr, dolelcfan, dolelcok, dolelcol, dolelcw, doleldhum, doleldwh, dolelevapcol, dolelfrz, dolelhtbheat, dolelhtbpmp, dolelhum, dolellgt, dolelmicro, dolelnec, dolelplpmp, dolelrfg, dolelrfg1, dolelrfg2, dolelsph, doleltv1, doleltv2, doleltvrel, dolelwth, dolfonec, dolfosph, dolfowth, dollarel, dollarfo, dollarlp, dollarng, dollpcdr, dollpcok, dollpnec, dollpsph, dollpwth, dolngcdr, dolngcok, dolnghtbheat, dolngnec, dolngplheat, dolngsph, dolngwth, door1sum, drafty, dryer, dryrfuel, dryruse, dualcooktfuel, dualovenfuel, dvd, dwashuse, dwcycle, eelights, elcool, elfood, elother, elperiph, elwarm, elwater, energyasst, energyasst11, energyasst12, energyasst13, energyasst14, energyasst15, energyasstoth, equipage, equipaux, equipauxtype, equipm, equipmuse, escwash, esdishw, esdryer, esfreeze, esfrig, eslight, eswater, eswin, foodproc, foother, fopay, fowarm, fowater, freeaudit, fuelaux, fuelh2o, fuelh2o2, fuelpool, fueltub, gallonfo, gallonfonec, gallonfosph, gallonfowth, gallonlp, gallonlpcdr, gallonlpcok, gallonlpnec, gallonlpsph, gallonlpwth, gargcool, gargheat, gndhdd65, gwt, h2oheatapt, hdd30yr, hdd50, hdd65, heathome, highceil, hotma, ice, intdata, intdataacc, intstream, inwireless, kwh, kwhahucol, kwhahuheat, kwhcdr, kwhcfan, kwhcok, kwhcol, kwhcw, kwhdhum, kwhdwh, kwhevapcol, kwhfrz, kwhhtbheat, kwhhtbpmp, kwhhum, kwhlgt, kwhmicro, kwhnec, kwhplpmp, kwhrfg, kwhrfg1, kwhrfg2, kwhsph, kwhtv1, kwhtv2, kwhtvrel, kwhwth, lgtin4, lgtincan, lgtincfl, lgtincntl, lgtinled, lgtinnum, lgtoutcntl, lgtoutnum, locrfri2, lpcook, lpgpay, lpother, lpwarm, lpwater, micro, moisture, monpool, montub, morethan1h2o, ncombath, nhafbath, noacbroke, noacdays, noacel, noachelp, noheatbroke, noheatbulk, noheatdays, noheatel, noheathelp, noheatng, notmoist, numatticfan, numberac, numcfan, numfloorfan, numfreez, nummeal, numsmphone, numwholefan, oa_lat, othrooms, outgrill, outgrillfuel, outlet, oven, ovenfuel, ovenuse, payhelp, pelletamt, pelletbtu, periodel, periodfo, periodlp, periodng, playsta, pool, prkgplc1, protherm, prothermac, rebateapp, recbath, recycapp, ricecook, rooftype, scaleb, scalee, scaleg, sepcooktuse, sepdvr, sepovenuse, sizeofgarage, sizfreez, sizrfri1, sizrfri2, smartmeter, smarttherm, solar, solother, solwater, stories, stove, stovefuel, stovenfuel, studio, swampcol, swimpool, taxcreditapp, tempgone, tempgoneac, temphome, temphomeac, tempnite, tempniteac, thermain, thermainac, toast, toastovn, topfront, totalbtu, totalbtucdr, totalbtucok, totalbtuhtb, totalbtunec, totalbtupl, totalbtusph, totalbtuwth, totaldol, totaldolcdr, totaldolcok, totaldolhtb, totaldolnec, totaldolpl, totaldolsph, totaldolwth, totcsqft, tothsqft, totsqft_en, totucsqft, totusqft, tvaudiosys, tvcolor, tvonwd1, tvonwd2, tvonwe1, tvonwe2, tvsize1, tvsize2, tvtype1, tvtype2, typeglass, typerfr1, typerfr2, ugashere, ugcook, ugoth, ugwarm, ugwater, uprtfrzr, usecenac, useel, usefo, uselp, usemoisture, useng, usenotmoist, usesolar, usewood, usewwac, vcr, walltype, washload, wdother, wdpellet, wdwarm, wdwater, wheatage, wheatsiz, windows, winframe, woodamt, woodbtu, woodlogs, wsf, wwacage 
    Merging donor spatial predictor variables...
    Merging recipient spatial predictor variables...
    Assembling output data frames...
    Performing consistency checks...

The resulting `data` object is a list containing two data frames. The
first slot contains the “fusion ready” donor microdata. The second slot
contains the analogous ACS recipient microdata. Notice that the RECS
microdata has more variables/columns than the ACS data. This is because
`assemble` donor output includes – by default – *all* valid variables in
the donor survey not used to create harmonies. The latter are potential
candidates for fusion (and there are many in the case of RECS).

``` r
lapply(data, dim)
```

    $RECS_2015
    [1] 5686  641

    $ACS_2015
    [1] 1226728     234

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
are conceptually identical and are then included “as is” or
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

`prepare()` also calls the internal function `assignLocation()` to
impute one or more plausible PUMA’s for each donor household. The
`implicates` argument controls how many PUMA’s are imputed for each
donor household. Setting `implicates` higher results in more variability
in the spatial predictors merged to a given household, reflecting
uncertainty about where the household is located. The use of implicates
here mimics its usage in standard multiple imputation techniques (5
implicates is typical).

------------------------------------------------------------------------

The spatial imputation algorithm coded in `assignLocation()` requires
some explanation. The location variables in the donor microdata are used
to identify the set of possible PUMA’s assignable to each household,
with the initial likelihood of selection being proportional to the
number of housing units in the PUMA. However, we can also exploit the
fact that we observe harmonized variables for donor and ACS respondents
– and the PUMA of ACS respondents is known. That is, there is
information in the *harmonized* variables that can be used to alter the
likelihood of selecting a given PUMA.

A conceptual example: Imagine, based on location variables alone, we
know that a particular donor household is resident in one of five PUMA’s
(001 through 005), each with an equal number of housing units. Using
this information alone, we would assign each PUMA an equal probability
of selection (call it *P*). However, we also observe household income
for both donor and ACS households. The donor household is high income.
We notice in the ACS microdata that high income households are common in
PUMA 001 but rare in the other four. Conceptually, this information
should increase the probability of selecting PUMA 001.

In practice, we typically observe multiple harmonized variables – some
categorical and some continuous. `assignLocation()` calculates [Gower’s
distance](https://www.jstor.org/stable/2528823), using all of the
harmonized variables, to derive a pairwise similarity value (*S*)
between each donor household and a random sample of ACS households
located within the feasible set of PUMA’s. The calculation uses the
efficient [gower
package](https://cran.r-project.org/web/packages/gower/index.html); even
so, random sampling of the ACS is necessary to make it tractable. An ACS
household (or multiple if `implicates` &gt; 1) is then randomly
selected, where the probability of selection is *P* / *S* and the
associated PUMA is imputed to the donor household. That is, the naive,
population-based probability of selection (*P*) is modified by the
observable similarity of the donor household and each ACS household
(*S*).

------------------------------------------------------------------------

`assemble()` also merges spatial variables onto both the donor and
recipient microdata. Pre-compiled spatial variables (those in
`geo_predictors.fst`) are merged onto both the donor and recipient
microdata at the PUMA level, using imputed PUMA’s for the donor. Spatial
variables are indicated by the double-dot (“..”) in the variable name,
analogous to the way that harmonized variables are indicated by the
double-underscore ("\_\_").

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
     [15] "ngpay__gasfp"            "nhsldmem__np"           
     [17] "numadult__agep"          "numchild__agep"         
     [19] "numfrig__refr"           "numtablet__handheld"    
     [21] "occupyyrange__mv"        "sdescent__hisp"         
     [23] "stoven__stov"            "totrooms__rmsp"         
     [25] "typehuq__bld"            "yearmaderange__ybl"     
     [27] "loc..ur12"               "loc..cbsatype15"        
     [29] "loc..region"             "loc..recs_division"     
     [31] "loc..recs_ba_zone"       "loc..recs_iecc_zone"    
     [33] "acs.pums..npa"           "acs.pums..accssywstsb"  
     [35] "acs.pums..accssywstsc"   "acs.pums..acrn"         
     [37] "acs.pums..acrhl"         "acs.pums..acrht"        
     [39] "acs.pums..anf1"          "acs.pums..agsn"         
     [41] "acs.pums..bthy"          "acs.pums..bdsp"         
     [43] "acs.pums..bldm"          "acs.pums..bldofhd"      
     [45] "acs.pums..bldofhm"       "acs.pums..bld5"         
     [47] "acs.pums..brdy"          "acs.pums..bsnf"         
     [49] "acs.pums..bsys"          "acs.pums..cmpy"         
     [51] "acs.pums..dlpy"          "acs.pums..dsly"         
     [53] "acs.pums..elep"          "acs.pums..fbry"         
     [55] "acs.pums..fsys"          "acs.pums..flpx"         
     [57] "acs.pums..gspy"          "acs.pums..hndy"         
     [59] "acs.pums..hflu"          "acs.pums..hfle"         
     [61] "acs.pums..hflf"          "acs.pums..insp"         
     [63] "acs.pums..lpty"          "acs.pums..mdmy"         
     [65] "acs.pums..mrgnb"         "acs.pums..mrgyp"        
     [67] "acs.pums..mrgp"          "acs.pums..mrgtn"        
     [69] "acs.pums..mrgty"         "acs.pums..mrgxn"        
     [71] "acs.pums..mrgm"          "acs.pums..mrgc"         
     [73] "acs.pums..othy"          "acs.pums..rfry"         
     [75] "acs.pums..rmsp"          "acs.pums..rntn"         
     [77] "acs.pums..rntp"          "acs.pums..rwty"         
     [79] "acs.pums..stly"          "acs.pums..snky"         
     [81] "acs.pums..stvy"          "acs.pums..tlysx"        
     [83] "acs.pums..tnow"          "acs.pums..tnof"         
     [85] "acs.pums..tnrn"          "acs.pums..tlysb"        
     [87] "acs.pums..vlpc"          "acs.pums..vhnv"         
     [89] "acs.pums..vh1v"          "acs.pums..vh2v"         
     [91] "acs.pums..vh3v"          "acs.pums..wtph"         
     [93] "acs.pums..yb19"          "acs.pums..y194"         
     [95] "acs.pums..y195"          "acs.pums..y196"         
     [97] "acs.pums..y197"          "acs.pums..y198"         
     [99] "acs.pums..y199"          "acs.pums..y202"         
    [101] "acs.pums..fsnf"          "acs.pums..fsmcfhw"      
    [103] "acs.pums..fsmcfhl"       "acs.pums..fsmcfn"       
    [105] "acs.pums..fsof"          "acs.pums..fncp"         
    [107] "acs.pums..fprn"          "acs.pums..fwr5"         
    [109] "acs.pums..fw51"          "acs.pums..fw55"         
    [111] "acs.pums..grnt"          "acs.pums..grpp"         
    [113] "acs.pums..hhle"          "acs.pums..hhls"         
    [115] "acs.pums..hhtm"          "acs.pums..hhto"         
    [117] "acs.pums..hhtnhm"        "acs.pums..hhtnhf"       
    [119] "acs.pums..hncp"          "acs.pums..hgch"         
    [121] "acs.pums..hwc6"          "acs.pums..hw61"         
    [123] "acs.pums..hw66"          "acs.pums..hpc6"         
    [125] "acs.pums..hpc61"         "acs.pums..hpc661"       
    [127] "acs.pums..hpr6"          "acs.pums..hpr61"        
    [129] "acs.pums..hpr661"        "acs.pums..ktyh"         
    [131] "acs.pums..lal1"          "acs.pums..mltn"         
    [133] "acs.pums..mv1m"          "acs.pums..m1t2"         
    [135] "acs.pums..m2t4"          "acs.pums..m5t9"         
    [137] "acs.pums..m1t1"          "acs.pums..m2t2"         
    [139] "acs.pums..nocc"          "acs.pums..npfd"         
    [141] "acs.pums..nppn"          "acs.pums..nrnn"         
    [143] "acs.pums..nrcg"          "acs.pums..ocpp"         
    [145] "acs.pums..prtn"          "acs.pums..plmy"         
    [147] "acs.pums..psfn"          "acs.pums..r18n"         
    [149] "acs.pums..r60n"          "acs.pums..r601"         
    [151] "acs.pums..r65n"          "acs.pums..r651"         
    [153] "acs.pums..rsmm"          "acs.pums..rsmc"         
    [155] "acs.pums..smcp"          "acs.pums..smxnbb"       
    [157] "acs.pums..smxnob"        "acs.pums..ssmh"         
    [159] "acs.pums..txpn"          "acs.pums..wfnn"         
    [161] "acs.pums..wfnw"          "acs.pums..wf1w"         
    [163] "acs.pums..wf2w"          "acs.pums..wkxn"         
    [165] "acs.pums..wkxrlhs"       "acs.pums..wkxrlhwftsw"  
    [167] "acs.pums..wkxrlhwftsd"   "acs.pums..wkxrlhd"      
    [169] "acs.pums..wkxf"          "acs.pums..wrksttnf"     
    [171] "acs.pums..wrkstthw"      "acs.pums..wrkstthl"     
    [173] "acs.pums..wrksttnh"      "acs.pums..wrkf"         
    [175] "acs.pums..elfv"          "acs.pums..flfi"         
    [177] "acs.pums..flfn"          "acs.pums..gsfpip"       
    [179] "acs.pums..gsfpir"        "acs.pums..gsfn"         
    [181] "acs.pums..wtfi"          "acs.pums..wtfn"         
    [183] "acs.sf..b010"            "acs.sf..b060"           
    [185] "acs.sf..b080"            "acs.sf..b1900"          
    [187] "acs.sf..b1910"           "acs.sf..b1920"          
    [189] "acs.sf..b25010"          "acs.sf..b250350"        
    [191] "acs.sf..b250390"         "acs.sf..b25060"         
    [193] "acs.sf..b250710"         "acs.sf..b250770"        
    [195] "acs.sf..b25080"          "acs.sf..b25090"         
    [197] "acs.sf..b2510"           "climate..cddb6"         
    [199] "climate..hddb6"          "climate..cdd12b6"       
    [201] "climate..hdd12b6"        "climate..iccz"          
    [203] "climate..bznf"           "eia.seds..gslp"         
    [205] "eia.seds..elcp"          "eia.seds..ntgp"         
    [207] "eia.seds..lpgp"          "eia.seds..fllp"         
    [209] "eia.seds..elmh"          "eia.seds..ntth"         
    [211] "eia.seds..lpgh"          "eia.seds..flgh"         
    [213] "irs.soi..mipr"           "irs.soi..mipp"          
    [215] "irs.soi..mppr"           "irs.soi..mdpr"          
    [217] "irs.soi..prsr"           "irs.soi..prjr"          
    [219] "irs.soi..phohr"          "irs.soi..pppr"          
    [221] "irs.soi..pvpr"           "irs.soi..prcntelr"      
    [223] "irs.soi..prfr"           "irs.soi..prcntetr"      
    [225] "irs.soi..prie"           "irs.soi..priu"          
    [227] "irs.soi..eftr"           "irs.soi..palt2"         
    [229] "irs.soi..pa2t5"          "irs.soi..pa5t7"         
    [231] "irs.soi..pa7t1"          "irs.soi..pa1t2"         
    [233] "irs.soi..pa2om"          "nrel.urdb..rsed"        

The string to the left of the “..” identifies the spatial dataset that
the variable comes from (e.g. “irs.soi”). The string to the right is a
unique, syntactically-valid identifier. It’s not critical that specific
spatial variables be identifiable in the fusion process. And because
pre-processing of spatial datasets does not impose a stringent
naming/documentation convention (it is flexible by design), these
non-nonsensical-but-unique names are the safest way to identify spatial
variables.

The "loc..\*" variables refer to the location variables directly
observed in the donor and assignable to recipient microdata on the basis
of respondent PUMA.

Now let’s explore some of the additional arguments to `assemble()`:

1.  We can request certain fusion variables by passing a character
    vector to `fusion.variables`. The input is checked internally
    against the data and only valuid fusion candidates are returned
    (with helpful message to console).

2.  We can limit the spatial datasets merged to the microdata via the
    `spatial.datasets` argument. Default is to include all available
    datasets, and this is sensible in most cases.

3.  The `window` argument controls how wide a timespan (+/- `window`
    years from the data vintage) is tolerated when merging spatial
    variables to microdata. The default (`window = 0`) means that
    spatial variables are merged only when their vintage matches that of
    the microdata. A larger `window` will generally mean more spatial
    variables in the output but at some cost in terms of temporal
    alignment.

4.  The `pca` argument controls whether/how principal components
    analysis (PCA) is used to reduce dimensionality of the spatial
    variables. `?prepare` provides additional details concerning the
    `pca` argument.

5.  Setting `replicates = TRUE` will cause replicate observation weights
    to be returned along with the central/primary `weight` column.

The following shows a more complex (and realistic) call to `prepare()`
and `assemble()`, making use of the optional arguments.

``` r
# Prepare RECS 2015 household microdata for fusion with ACS 2015 microdata
prep <- prepare(donor = "RECS_2015", 
                recipient = "ACS_2015", 
                respondent = "household",
                implicates = 5)
```

    Harmonizing RECS_2015 (donor) microdata at household level
    Harmonizing ACS_2015 (recipient) microdata at household level
    Identified 124 geographic intersections in the donor...
    Imputing PUMA for donor observations...
    Assigning location variables to recipient observations...

``` r
data <- assemble(prep,
                 fusion.variables = c("cooltype", "agecenac", "kwhcol"),
                 window = 3,
                 pca = c(30, 0.9))
```

    Identifying donor fusion variables...
    Adding the following fusion variables:
     agecenac, cooltype, kwhcol 
    Performing principal components analysis...
    Merging donor spatial predictor variables...
    Merging recipient spatial predictor variables...
    Assembling output data frames...
    Performing consistency checks...

``` r
lapply(data, dim)
```

    $RECS_2015
    [1] 26213    67

    $ACS_2015
    [1] 1226728      64

The number of observations in the donor microdata is now higher,
reflecting the use of `implicates = 5`. Note that the number of rows has
actually increased by less than a factor of five. This is because
`assignLocation()` collapses duplicate household-PUMA observations and
adjusts the sample “weight” column accordingly. This reduces the amount
of data without affecting subsequent statistical results.

The difference in the number of columns between the RECS and ACS
microdata is due to the former’s inclusion of our three requested fusion
variables. Otherwise, both data frames are entirely consistent with one
another; `assemble()` performs formal checks to ensure this is the case.
Each microdata set includes a unique household identifier variable and
an identical set of harmonized survey and spatial variables that can be
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
    [15] "ngpay__gasfp"            "nhsldmem__np"           
    [17] "numadult__agep"          "numchild__agep"         
    [19] "numfrig__refr"           "numtablet__handheld"    
    [21] "occupyyrange__mv"        "sdescent__hisp"         
    [23] "stoven__stov"            "totrooms__rmsp"         
    [25] "typehuq__bld"            "yearmaderange__ybl"     
    [27] "loc..ur12"               "loc..cbsatype15"        
    [29] "loc..region"             "loc..recs_division"     
    [31] "loc..recs_ba_zone"       "loc..recs_iecc_zone"    
    [33] "climate..iccz"           "climate..bznf"          
    [35] "pca..PC1"                "pca..PC2"               
    [37] "pca..PC3"                "pca..PC4"               
    [39] "pca..PC5"                "pca..PC6"               
    [41] "pca..PC7"                "pca..PC8"               
    [43] "pca..PC9"                "pca..PC10"              
    [45] "pca..PC11"               "pca..PC12"              
    [47] "pca..PC13"               "pca..PC14"              
    [49] "pca..PC15"               "pca..PC16"              
    [51] "pca..PC17"               "pca..PC18"              
    [53] "pca..PC19"               "pca..PC20"              
    [55] "pca..PC21"               "pca..PC22"              
    [57] "pca..PC23"               "pca..PC24"              
    [59] "pca..PC25"               "pca..PC26"              
    [61] "pca..PC27"               "pca..PC28"              
    [63] "pca..PC29"               "pca..PC30"              

## Make it rain

At this point, we are ready to fuse. This is straightforward using the
`train()` and `fuse()` functions from the fusionModel package. To make
this even easier, `assemble()` donor output includes a “fusion.vars”
attribute that can be passed directly to `train()` to identify the
variables available to be fused.

    library(fusionModel)

``` r
fit <- train(data = data$RECS_2015, 
             y = attr(data, "fusion.vars"), 
             ignore = "recs_2015_hid", 
             weight = "weight",
             cores = 2,
             maxcats = 10,
             complexity = 0.01)
```

    3 fusion variables
    62 initial predictor variables
    26213 observations
    Searching for derivative relationships...
    Determining order of fusion variables...
    Building fusion models...

We then `fuse()` (i.e. simulate) the fusion variables onto the
harmonized ACS microdata. This is a non-trivial exercise, since the
recipient ACS microdata has 1226728 observations. Ensuring
`induce = FALSE` (the default) eases the computation and memory burden
considerably. The call below shouldn’t require more than about 5GB of
RAM.

``` r
sim <- fuse(data = data$ACS_2015, train.object = fit)
```

A quick check that the fusion output looks plausible:

``` r
nrow(sim)
```

    [1] 1226728

``` r
head(sim)
```

         kwhcol                   agecenac                                 cooltype
    1  576.3857 No central air conditioner Individual window/wall or portable units
    2 2367.5607         10 to 14 years old          Central air conditioning system
    3 3459.5585      Less than 2 years old          Central air conditioning system
    4 4432.3754         10 to 14 years old          Central air conditioning system
    5    0.0000 No central air conditioner                      No air conditioning
    6 2298.4798           5 to 9 years old          Central air conditioning system

``` r
summary(data$RECS_2015$kwhcol)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
          0     390    1113    1875    2533   20350 

``` r
summary(sim$kwhcol)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        0.0   371.1  1071.4  1836.8  2513.6 20350.0 

Onward and upward!
