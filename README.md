fusionData
================
Kevin Ummel (<ummel@berkeley.edu>)

- <a href="#overview" id="toc-overview">Overview</a>
- <a href="#setup-and-install" id="toc-setup-and-install">Setup and
  install</a>
- <a href="#usage-and-structure" id="toc-usage-and-structure">Usage and
  structure</a>
- <a href="#ingest-survey-data" id="toc-ingest-survey-data">Ingest survey
  data</a>
- <a href="#document-variables" id="toc-document-variables">Document
  variables</a>
- <a href="#harmonize-variables" id="toc-harmonize-variables">Harmonize
  variables</a>
- <a href="#compile-spatial-data" id="toc-compile-spatial-data">Compile
  spatial data</a>
- <a href="#prepare-fusion-inputs" id="toc-prepare-fusion-inputs">Prepare
  fusion inputs</a>
- <a href="#generate-fusion-outputs"
  id="toc-generate-fusion-outputs">Generate fusion outputs</a>

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
5.  **Prepare fusion inputs**: Prepare harmonized donor and ACS
    microdata fusion inputs.
6.  **Generate fusion outputs**: Fuse donor variables to ACS microdata
    using [fusionModel](https://github.com/ummel/fusionModel).

## Setup and install

The fusionData *master* branch can be cloned to its own project
directory on your local machine using RStudio and [these
instructions](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html).

Use the following setup parameters:

``` r
Repository URL: https://github.com/ummel/fusionData
Project directory name: fusionData
```

You will be prompted to enter your Github username and password. You may
be prompted to install some package dependencies. If you use
multi-factor authentication for your Github credentials, you will need
to enable an SSH key, and will need to use
`git@github.com:ummel/fusionData.git` as the Repository URL in the above
setup parameters.

Although fusionData is structured (and loadable) as a *R* package, it
also acts as a code and data *repository* that is shared and
continuously modified by authorized users. fusionData grows over time as
new surveys and spatial datasets – and the code needed to process and
manipulate them – are added.

Now that the shared Github repository has been cloned to your local
drive (in a `/fusionData` project directory), we can install the package
locally and load it. This step will install any required package
dependencies and may take awhile. You will be redirected to a browser
window to enter credentials for the Google Drive account storing
fusionData’s remote files. The password is provided separately for
approved users.

``` r
# Do initial local install of the fusionData package
devtools::install(quick = TRUE)

# Load the fusionData package
library(fusionData)
```

Whenever the fusionData package is loaded, it checks that the current
working directory is set to your local `/fusionData` directory (it will
issue an error otherwise). This is because the package works with (and
expects) a particular directory structure locally that is mimicked in
the Github repository.

fusionData includes a convenience function called `installPackage()` to
safely update (re-install) your local installation of the fusionData
package. If you modify important code yourself or pull changes from the
Github repository, it is necessary to re-install the fusionData
*package* locally to get all of the functionality. For example, if a
function is added or modified on Github, your local installation won’t
reflect the changes until you pull and re-install. `installPackage()`
does a number of operations that our initial `devtools::install()`
didn’t, so let’s go ahead and run it now:

``` r
installPackage()
```

For full functionality, it is also necessary to download at least some
of the remotely-stored survey microdata and processed spatial data
files. The following section (“Usage and structure”) provides more
detail about this and the associated reasoning. The functions
`getSurveyProcessed()` and `getGeoProcessed()` offer an easy way to do
this. For example, to run most of the code in this README you will
minimally need:

``` r
# Download RECS 2015 processed survey microdata file
getSurveyProcessed(survey = "RECS_2015")

# Download only the essential remote spatial data files
getGeoProcessed(dataset = "essential")
```

The downloads may take a few minutes. The files are automatically placed
in the appropriate sub-directories of `/fusionData` on your local drive,
with the directories created if necessary. After successful download,
your fusionData “system” is ready to go.

## Usage and structure

As you modify code and files in your local `/fusionData` project
directory, you will need to commit and *push* those changes to the
Github repository for them to be accessible to other users. In addition,
it is good practice to *pull* the latest version of the repository from
Github prior to making any modifications. That way, you know you are
working from the latest shared version. This is most easily done using
the “Git” panel in the RStudio IDE.

Since Github places limits on file/repository size, we store certain
data files “remotely” – that is, outside of the Github repository. The
remotely-stored data files are integral to the overall fusionData
“system”, but they are not present in the Github repository itself.
Instead, the remote files (and associated directory structure) are
stored in Google Drive and can be automatically and safely added to a
user’s *local* `/fusionData` folder using provided functions. Once the
remote files are added, the user’s local fusionData package is fully
functional. Remote data files are fairly static. So, a user typically
only needs to update (re-download) remote files to their local
fusionData directory if important changes have been made.

You don’t generally need to keep track of which files are stored
remotely and which are in the Github repository (the
[.gitignore](https://github.com/ummel/fusionData/blob/master/.gitignore)
file handles this).

**In summary: The Github repository stores the code needed to build and
document the fusionData architecture. Large and/or infrequently-modified
data files are stored remotely. Users can download/upload remote files
to/from their local `/fusionData` directory as-needed. When users add or
modify *code*, the changes are committed locally and then pushed to the
Github repository where they become subject to code reviews, versioning
control, and accessible to other users.**

Below is an overview of the top-level directories in the fusionData
repository, including both Github-based and “remote” elements.

### `/.github`

Auto-generated files for producing fusionData’s [public documentation
website](https://ummel.github.io/fusionData/). *Most users do not need
to access/modify this directory.*

### `/R`

`.R` scripts defining functions for doing “fusionData things” (not all
are exported). *Most users do not need to access/modify this directory.*

### `/man`

Documentation (i.e. “manual”) of functions in `/R` and data in `/data`,
as usual for R packages. *Most users do not need to access/modify this
directory.*

### `/data`

Package-wide `.rda` data files. Loadable via `data()`, as usual for R
packages. *Most users do not need to access/modify this directory.*

### `/data-raw`

`.R` scripts needed to create any package-wide `.rda` objects in
`/data`, as usual for R packages. *Most users do not need to
access/modify this directory.*

### `/universe`

Directory for the “Universal Survey Dictionary” Shiny app. The app
itself can be run by calling `universe()`. *Most users do not need to
access/modify this directory.*

### `/harmony`

Directory for the “Survey Harmonization Tool” Shiny app. The app itself
can be run by calling `harmony()`. *Most users do not need to
access/modify this directory.*

### `/survey-processed`

Contains processed survey data and associated code. Sub-directories
refer to specific surveys and vintages. *Users responsible for ingesting
raw survey microdata will add their custom processing code to this
directory.*

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
containing *raw* survey data files. Sub-directories refer to specific
surveys and vintages. *Users responsible for ingesting raw survey
microdata will add the raw data files to this directory.*

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

Contains processed spatial data and associated code. Sub-directories
refer to specific spatial datasets. *Most users do not need to
access/modify this directory.* For users who are not modifying or adding
spatial datasets, it is sufficient to call
`getGeoProcessed(dataset = "essential")` to load the essential “geo”
files.

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

Importantly, the `/geo-processed` remote content *also* includes two
“essential” spatial data files that are, in practice, all that most
users will need to perform data fusion locally. These files and their
roles are described in more detail later on.

1.  `geo_predictors.fst`
2.  `concordance/geo_concordance.fst`

### `/geo-raw`

A remote directory (i.e. not present in the Github repository)
containing *raw* spatial data files. Sub-directories refer to specific
spatial datasets. *Most users do not need to access/modify this
directory.*

Example: `/geo-raw/EPA-SLD`

`/geo-raw` can be downloaded and added to a user’s local fusionData
directory by calling `getGeoRaw()`. However, in practice, there is no
reason for a user to store raw spatial data locally unless it is for a
spatial dataset that they are actively processing or editing.

### `/fusion`

Contains production-level fusion input and output files generated by
`fusionInput()` and `fusionOutput()`. Users will call these functions to
perform fusion but typically don’t need to access/modify the directory
itself. Almost all fusion input/output files are stored remotely given
their size. The only files that are pushed to the Github repository are
.txt log files that contain information about the `fusionInput()` and
`fusionOutput()` function calls.

### `/production`

A now-DEPRECATED directory containing code and possibly data from older
“production” fusion runs. Superseded by `/fusion`.

## Ingest survey data

“Ingesting” a survey requires transforming raw survey data into
“processed” (i.e. standardized) microdata files that meet certain
requirements. The fusionData codebase depends on the processed microdata
having recognizable structure and features.

The ingestion process for each survey is documented and defined by a .R
script (possibly multiple scripts) that must be written manually. The
goal is to produce a data.frame containing microdata observations that
(ideally) meet the following conditions:

- Contains as many observations and variables as possible.
- Variable names and descriptions are taken from the official codebook,
  possibly modified for clarity.
- Official variable names are coerced to lower-case alphanumeric,
  possibly using single underscores.
- Codes used in the raw data are replaced with descriptive labels from
  the codebook; e.g. integer values are replaced with associated factor
  levels.
- All “valid blanks” in the raw data are set to plausible values; NA’s
  are often actual zeros or some other knowable value based on the
  question structure.
- All “invalid blanks” or missing values in the raw data are imputed; a
  generic imputation function is provided for this purpose.
- Ordered factors are used and defined whenever possible (as opposed to
  unordered).
- Standard column names are used for unique household identifiers
  (e.g. “acs_2019_hid”); for person-level microdata the within-household
  person identifier (integer) is always “pid”.
- Standard column names are used for observation weights; “weight” for
  the primary weighting variable and “rep_1”, etc. for replicate
  weights.
- Variables identifying respondent location are consistent with those
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
(“recs_2015_hid”), as does the observation weights column (“weight”) and
the first of the 96 replicate weights (“rep_1”). If the microdata
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

The variable “recs_iecc_zone” tells us something about each respondent’s
location ([IECC climate
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
strict procedure for how the .R file(s) should be written. However,
there are common steps and tools likely to be applicable to most
surveys. The `RECS_2015_H_processed.R` script is a good “template” in
this regard, since it includes many common operations – including
imputation of NA’s using the provided `imputeMissing()` function.

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
[RECS_2015_H\_processed.R](https://github.com/ummel/fusionData/blob/master/survey-processed/RECS/2015/RECS_2015_H_processed.R):

Here is the resulting dictionary file for RECS 2015.

``` r
recs.dictionary <- readRDS("survey-processed/RECS/2015/RECS_2015_H_dictionary.rds")
head(recs.dictionary)
```

    # A tibble: 6 × 8
      survey vintage respondent variable  description             values type      n
      <chr>  <chr>   <chr>      <chr>     <chr>                   <chr>  <chr> <int>
    1 RECS   2015    H          adqinsul  Level of insulation     [Not … ord    5686
    2 RECS   2015    H          agecdryer Age of clothes dryer    [No c… ord    5686
    3 RECS   2015    H          agecenac  Age of central air con… [No c… ord    5686
    4 RECS   2015    H          agecwash  Age of clothes washer   [No c… ord    5686
    5 RECS   2015    H          agedw     Age of dishwasher       [No d… ord    5686
    6 RECS   2015    H          agefrzr   Age of most-used freez… [No f… ord    5686

In practice, there is no reason for a typical user to ever open a
survey’s dictionary file. The preferred and much more useful way to
explore survey metadata and variable descriptions is via the
`universe()` function described in the next section.

## Document variables

The previous section showed how and where a survey’s “dictionary.rds”
file(s) are created. Whenever a dictionary file is added or updated, it
is necessary to run the `compileDictionary()` function to compile all of
fusionData’s individual survey dictionaries into a single “universal”
dictionary. The usage is straightforward:

``` r
compileDictionary()
```

As the console output reveals, `compileDictionary()` updates two files:
`data/dictionary.rda` and `data/surveys.rda`. These files are part of
the Github repository and are used by both the “Universal Survey
Dictionary” and “Survey Harmonization Tool” Shiny apps that are part of
fusionData.

If this is the first time you are calling `compileDictionary()` for a 
new survey, you will need to add the survey abbreviation and description 
of the survey to the R script `universe/app.R` on line 32 so it can be 
called on by `universe()` (described below).

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
[RECS_2015\_\_ACS_2015.R](https://github.com/ummel/fusionData/blob/master/harmony/harmonies/RECS_2015__ACS_2015.R).

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

Very advanced example: Select “as_comp1” for CEI 2015-2019 (Number of
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
modified = "2021-07-03 12:10:17")
```

This list object contains all of the information necessary to construct
RECS and ACS microdata containing a new variable called
“fuelheat\_\_hfl”; i.e. the harmonized version of the two associated
heating fuel variables. This is precisely what the `harmonize()`
function does – typically when called by `prepare()` as explained below
– using all of the harmonies available in the specified harmony file.
Note that harmonized variables are always indicated by a
double-underscore (“\_\_“).

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
`/geo-raw`. The raw data is transformed to a “\*\_processed.rds” file
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
      zcta10 vintage `Mean income per return` `Mean income per person` Mean people…¹
      <chr>    <int>                    <int>                    <int>         <dbl>
    1 35004     2018                    58600                    28760          2.04
    2 35005     2018                    41200                    21200          1.94
    3 35006     2018                    53100                    25300          2.10
    4 35007     2018                    62300                    29240          2.13
    5 35010     2018                    52900                    25700          2.06
    6 35014     2018                    50300                    25900          1.94
    # … with abbreviated variable name ¹​`Mean people per return`

The `irs` object contains processed spatial data constructed from the
[IRS Statistics of Income
(SOI)](https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi)
zip code tax return data for 2018. The underlying raw data is stored
remotely at `/geo-raw/IRS-SOI/2018`. The script used to create the
“processed.rds” file is [available
here](https://github.com/ummel/fusionData/blob/master/geo-processed/IRS-SOI/IRS-SOI_AllVintages_processed.R).
The “zcta10” column indicates the Zip Code Tabulation Area (circa 2010)
associated with each observation.

The “zcta10” variable is also found in the “geo_concordance.fst” file,
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
with “\_processed.rds”. As long as a processed spatial data file has the
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

## Prepare fusion inputs

Once a donor survey is successfully ingested and harmonized, it is
possible to move on to the final step: fusion! The fusion process
consists of two parts. First, the necessary “input” data files are
prepared. Second, those inputs are used to generate the final fusion
“output”.

As of fusionData v1.0, these two steps are helpfully wrapped into the
`fusionInput()` and `fusionOutput()` functions, respectively. These
functions safely execute all of the steps required for successful
fusion. In practice, users need only call the two high-level functions
and ensure that the resulting console output and log files look good.

For a worked example, let’s prepare the required inputs for fusion of
the RECS 2015 to the ACS 2015 (in test mode, of course!). This requires
the processed microdata for both surveys to be present in your local
`/fusionData` installation. If you haven’t already, you can download
these files using:

``` r
getSurveyProcessed(survey = "RECS_2015")
getSurveyProcessed(survey = "ACS_2015")
```

You will also need to install the [fusionModel
package](https://github.com/ummel/fusionModel):

``` r
devtools::install_github("ummel/fusionModel")
```

Now let’s make a call to `fusionInput()`. The function arguments are
fully documented in `?fusionInput`. If you run this code in an
interactive session, you will be prompted at the console to approve
certain steps before proceeding (again, this is all documented). The
console output is designed to be very informative and describe what is
being done behind the scenes.

``` r
input.dir <- fusionInput(donor = "RECS_2015",
                         recipient = "ACS_2015",
                         respondent = "household",
                         fuse = c("btung", "btuel", "cooltype"),
                         force = c("moneypy", "householder_race", "education", "nhsldmem", "kownrent", "recs_division"),
                         note = "Hello world. This is a worked example for the package README!")
```

    2023-03-30 16:39:06 MDT 
    R version 4.2.3 (2023-03-15) 
    Platform: x86_64-pc-linux-gnu 
    fusionData v1.0.0
    fusionModel v2.2.2

    fusionInput(donor = "RECS_2015", recipient = "ACS_2015", respondent = "household", 
        fuse = c("btung", "btuel", "cooltype"), force = c("moneypy", 
            "householder_race", "education", "nhsldmem", "kownrent", 
            "recs_division"), note = "Hello world. This is a worked example for the package README!", 
        test_mode = TRUE, ncores = getOption("fusionData.cores"))

    fusionInput() is running in TEST mode.

    User-supplied note:
     Hello world. This is a worked example for the package README! 

    Result files will be saved to:
     /home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/input 

    The local /input directory already exists.

    |=== Check for custom pre-processing script ===|

    None found.

    |=== prepare() microdata ===|

    Harmonizing RECS_2015 (donor) microdata at household level
    Harmonizing ACS_2015 (recipient) microdata at household level
    Identified 124 geographic intersections in the donor...
    Imputing PUMA for donor observations...
    Assigning location variables to recipient observations...

    |=== assemble() microdata ===|

    Identifying donor fusion variables...
    Including the following fusion variables:
     btuel, btung, cooltype 
    Applying integer scaling to spatial predictor variables...
    Merging donor spatial predictor variables...
    Merging recipient spatial predictor variables...
    Assembling output data frames...
    Performing consistency checks...

    |=== Check for custom .R scripts ===|

    None found.

    |=== Check categorical harmonized variables ===|

    Similarity scores for 18 categorical harmonized variables:
    # A tibble: 18 × 2
       `Harmonized variable`   `Similarity score`
       <chr>                                <dbl>
     1 numtablet__handheld                  0.676
     2 employhh__wkhp                       0.727
     3 occupyyrange__mv                     0.786
     4 stoven__stov                         0.856
     5 ngpay__gasfp                         0.868
     6 hhsex__sex                           0.878
     7 moneypy__hincp                       0.905
     8 fuelheat__hfl                        0.929
     9 householder_race__rac1p              0.933
    10 internet__access                     0.933
    11 education__schl                      0.945
    12 elpay__elefp                         0.955
    13 kownrent__ten                        0.983
    14 numfrig__refr                        0.993
    15 typehuq__bld                         0.993
    16 yearmaderange__ybl                   0.996
    17 desktop__laptop                      0.998
    18 sdescent__hisp                       0.999
    Retaining all categorical harmonized variables.

    |=== Check location variables ===|

    The representative location variable 'loc..recs_division' has 10 levels.
    The following location variables have been flagged for possible exclusion:
    # A tibble: 1 × 2
      `Location variable` `Number of levels`
      <chr>                            <int>
    1 loc..recs_iecc_zone                 11
    Retaining all location variables.

    |=== Check fusion and predictor variables ===|

    Identified 3 fusion variables (0 blocks):
    [1] "btung"    "btuel"    "cooltype"

    Identified 24 harmonized variables and 240 total predictors

    Identified 6 predictors to force and use for validation:
    [1] "moneypy__hincp"          "householder_race__rac1p"
    [3] "education__schl"         "nhsldmem__np"           
    [5] "kownrent__ten"           "loc..recs_division"     

    |=== Run fusionModel::prepXY() ===|

    fusionModel v2.2.2 | https://github.com/ummel/fusionModel

    Missing values imputed for the following 'x' variable(s):
     acs.sf..b060, acs.sf..b080, nrel.urdb..rsed 
    Identifying 'x' that pass absolute Spearman correlation threshold
    Fitting full models for each 'y'
    Iteratively constructing preferred fusion order
    Retained 176 of 240 predictor variables
    Total processing time: 3.12 secs 

    Results of prepXY() saved to: RECS_2015_2015_H_prep.rds (0.00151 MB) 

    |=== Write training and prediction datasets to disk ===|

    Writing training dataset...
    Training dataset saved to: RECS_2015_2015_H_train.fst (1.05 MB) 

    Writing prediction dataset...
    Prediction dataset saved to: RECS_2015_2015_H_predict.fst (1.76 MB) 
    Test mode: saved partial prediction data. Expected production file size is ~ 216 MB

    |=== Upload /input files to Google Drive ===|


    |=== fusionInput() is finished! ===|

    fusionInput() total processing time: 33.6 secs 

    fusionInput() log file saved to:
     /home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/input/RECS_2015_2015_H_inputlog.txt

Both `fusionInput()` and `fusionOutput()` can be run in “test mode” – in
fact, this is the default behavior. When in test mode, result files are
saved within the `/fusion_` directory to prevent any conflict with (or
overwrite) of production data in `/fusion` (no underscore).
`fusionInput()` returns the path to the directory where files are saved.
We can confirm that the path used `/fusion_` as expected:

``` r
input.dir
```

    [1] "/home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/input"

Note that the path includes the correct directory hierarchy. The path
ends with “RECS/2015/2015/H/input” to indicate that the directory in
question contains fusion input files associated with the RECS donor
survey from 2015, for fusion to ACS 2015, at household-level.

Now let’s see the names of the files created by `fusionInput()`:

``` r
list.files(input.dir)
```

    [1] "RECS_2015_2015_H_inputlog.txt" "RECS_2015_2015_H_predict.fst" 
    [3] "RECS_2015_2015_H_prep.rds"     "RECS_2015_2015_H_train.fst"   

Every call to `fusionInput()` generates the same four generic result
files:

1.  `*_inputlog.txt`: A copy of the extensive console output you saw
    above.
2.  `*_prep.rds`: Information about the variables to be used in the
    eventual fusion model.
3.  `*_train.fst`: Donor survey processed and harmonized training
    microdata, ready for fusion.
4.  `*_predict.fst`: Recipient ACS processed and harmonized prediction
    microdata, ready for fusion.

It is usually necessary for a user to run `fusionInput()` more than once
for a given donor survey, since the process can flag issues with
variable harmonization that need to be addressed before finalizing the
fusion inputs.

### Person-to-household aggregation

If `donor` refers to a survey with both household- and person-level microdata *and* `respondent = "household"` *and* `fuse` includes person-level variables, then we have a situation where person-level fusion variables need to be aggregated to household-level prior to fusion. For example, most variables in the ASEC 2019 microdata are at the person level. When fusing with ACS 2019 data, these variables need to be aggregated to the household level.

This is done automatically within [`assemble`](https://ummel.github.io/fusionData/reference/assemble.html). In this scenario, person-level fusion variables are aggregated based on their class. By default, numeric variables return the household total (sum), unordered factors return the level of the household's reference person, and ordered factors return the household's maximum level. This is one reason why specifying variables as ordered or unordered factors is important within the ingestion process.

If the default aggregation methods are not correct for a specific variable, then you can override them in one of two ways: using the `agg_fun` argument or the `agg_adj` argument. Lets look at an example using ASEC and two variables at the person level: `kidcneed`, which flags if a child under the age of 14 needs paid childcare while their parents work, and `schllunch`, which is the value of school lunch meals provided to children for free at school.

First, download the processed microdata for both the ASEC 2019 and ACS 2019 surveys:

```         
getSurveyProcessed(survey = "ACS_2019")
getSurveyProcessed(survey = "ACES_2019")
```

```{r, echo = TRUE}
asec <- fst::read_fst("survey-processed/ASEC/2019/ASEC_2019_P_processed.fst")
head(select(asec, asec_2019_hid, weight, rep_1, schllunch, kidcneed))
```

By default, `schllunch` would be summed across all household members (because it is numeric) and `kidcneed` would take the reference person's value (because it is an unordered factor). However, (somewhat confusingly) in the person-level data, ASEC records `schllunch` as the same for all members of the *family*. Summing `schllunch` across household members would result in double counting the total school lunch value of that household, even if there are multiple families in the household. This problem occurs with many of the ASEC variables related to poverty because the *family-level* response is recorded for each *individual* in the family.

The default behavior for `kidcneed` would be the value of the reference person. As the reference person is the householder, who is never a child under the age 14, this would result in the aggregated variable being always "not in universe". Instead, we want to take the modal value across members of the household. This will ignore the NIU values and be a 1 if the majority of children need paid child care, and 0 if not.

Let's implement these two custom adjustments to the aggregation process within `fusionInput()`.

```{r, echo = TRUE}

input.dir <- fusionInput(donor = "ASEC_2019",
                         recipient = "ACS_2019",
                         respondent = "household",
                         # variables we always want as predictors:
                         force = c("hhincome", "race", "educ", "numprec", "hhtenure", "state"), 
                         # the two person-level variables we want to aggregate and fuse
                         fuse = c("kidcneed", "schllunch"),
                         # here we provide a list of specific custom functions for aggregation
                         agg_adj = list(
                           schllunch = ~if.else(duplicated(data.table(asec_2019_hid, famid)), 0, schllunch)
                         ),
                         # here we provide a list of specific pre-packaged functions for aggregation (which still override the defaults)
                         agg_fun = list(
                           kidcneed = "mode"
                         ),
                         note = "ASEC example of custom aggregation")


```

For `kidcneed` the custom aggregation uses a package-specific function called "mode", which returns to modal value across household members. Any other function that takes in a vector and returns a single value can be passed to the `agg_fun` argument.

For `schllunch` the aggregation is more specific. We want to take the value of the first person within the *family*. This is different to reference person (which we could otherwise get with the package-specific function "ref" passed to `agg_fun`) because there could be multiple families within a household. Because it is not an existing function, it needs to be passed to `agg_adj`.

Note that in the `schllunch` custom aggregation function, we use the convenience utility function `if.else()`. It wraps [`if_else`](https://dplyr.tidyverse.org/reference/if_else.html) and can be used identically but preserves factor levels and ordering in the result if possible.

The results files for this call to `fusionInput()` will now be at the *household* level, and the person level variables will be aggregated as we specified.


## Generate fusion outputs

Once the necessary input files are ready-to-go, it is straightforward to
complete the fusion process using `fusionOutput()`. The function
arguments are fully documented in `?fusionOutput`. A minimal call simply
indicates where to find the required input files. Everything else is
handled automatically:

``` r
output.dir <- fusionOutput(input = input.dir)
```

    2023-03-30 17:05:37 MDT 
    R version 4.2.3 (2023-03-15) 
    Platform: x86_64-pc-linux-gnu 
    fusionData v1.0.0
    fusionModel v2.2.2

    fusionOutput(input = input.dir, output = NULL, M = NULL, note = NULL, 
        test_mode = TRUE, upload = FALSE, ncores = getOption("fusionData.cores"), 
        margin = 2, ... = )

    fusionOutput() is running in TEST mode.

    The input files directory is:
     /home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/input 

    Result files will be saved to:
     /home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/output 

    The local /output directory already exists.

    |=== Load training data inputs ===|

    Loading training microdata: RECS_2015_2015_H_train.fst 
    Loading prepXY() results: RECS_2015_2015_H_prep.rds 

    |=== Run fusionModel::train() ===|

    Running in 'test' mode using fast(er) hyper-parameter settings:
    $boosting
    [1] "goss"

    $num_leaves
    [1] 8

    $min_data_in_leaf
    [1] 57

    $num_iterations
    [1] 50

    $bagging_fraction
    [1] 1

    $feature_fraction
    [1] 0.3

    $learning_rate
    [1] 0.2

    $max_depth
    [1] 3

    $max_bin
    [1] 16

    $min_data_in_bin
    [1] 57

    $max_cat_threshold
    [1] 8

    Training fusion model

    fusionModel v2.2.2 | https://github.com/ummel/fusionModel

    Missing values imputed for the following 'x' variable(s):
     acs.sf..b060, acs.sf..b080, nrel.urdb..rsed 
    3 fusion variables
    176 initial predictor variables
    5686 observations
    Using specified set of predictors for each fusion variable
    Using OpenMP multithreading within LightGBM (3 cores)
    Training step 1 of 3: btung
    -- R-squared of cluster means: 0.972 
    -- Number of neighbors in each cluster:
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       10.0    38.0   122.5   188.1   343.0   497.0 
    Training step 2 of 3: cooltype
    Training step 3 of 3: btuel
    -- R-squared of cluster means: 0.963 
    -- Number of neighbors in each cluster:
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       10.0    34.0    82.0   146.1   229.0   494.0 
    Fusion model saved to:
     /home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/output/RECS_2015_2015_H_model.fsn 
    Total processing time: 5.58 secs 

    |=== Fuse onto training data for internal validation ===|

    Running in 'test' mode, so internal validation skipped.

    |=== Fuse onto prediction data ===|

    Loading prediction microdata: RECS_2015_2015_H_predict.fst 

    Fusing to ACS microdata (2 implicates)
    3 fusion variables
    176 initial predictor variables
    10000 observations
    Missing values imputed for the following variable(s):
     acs.sf..b060, acs.sf..b080, nrel.urdb..rsed 
    Generating 2 implicates 
    Using OpenMP multithreading within LightGBM (3 cores)
    Fusion step 1 of 3: btung
    -- Predicting LightGBM models
    -- Simulating fused values
    Fusion step 2 of 3: cooltype
    -- Predicting LightGBM models
    -- Simulating fused values
    Fusion step 3 of 3: btuel
    -- Predicting LightGBM models
    -- Simulating fused values
    Writing fusion output to .fsd file 
    Fusion results saved to:
     /home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/output/RECS_2015_2015_H_fused.fsd 
    Total processing time: 0.78 secs 

    |=== Upload /output files to Google Drive ===|

    'upload = FALSE'; file upload skipped at request of user.

    |=== fusionOutput() is finished! ===|

    fusionOutput() total processing time: 6.4 secs 

    fusionOutput() log file saved to:
     /home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/output/RECS_2015_2015_H_outputlog.txt

As before, let’s check the location of the result files and their names:

``` r
output.dir
```

    [1] "/home/kevin/Documents/Projects/fusionData/fusion_/RECS/2015/2015/H/output"

``` r
list.files(output.dir)
```

    [1] "RECS_2015_2015_H_fused.fsd"     "RECS_2015_2015_H_model.fsn"    
    [3] "RECS_2015_2015_H_outputlog.txt"

Every call to `fusionOutput()` generates the same three generic result
files:

1.  `*_outputlog.txt`: A copy of the extensive console output above.
2.  `*_model.fsn`: The fusionModel object used to simulate the fusion
    variables.
3.  `*_fused.fsd`: The recipient microdata with simulated values for the
    fusion variables, across multiple implicates.

In addition, if *not* running in test mode, two additional files are
produced:

1.  `*_valid.fsd`: The *training* microdata with simulated values for
    the fusion variables, across multiple implicates.
2.  `*_validation.rds`: Results from internal validation exercises using
    the simulated variables in `*_valid.fsd`.

Just to confirm, let’s look at the final, fused microdata in
“RECS_2015_2015_H\_fused.fsd”.

``` r
fsd.file <- list.files(output.dir, full.names = TRUE)[1]
fused <- fusionModel::read_fsd(fsd.file)
```

    fusionModel v2.2.2 | https://github.com/ummel/fusionModel

``` r
dim(fused)
```

    [1] 20000     4

``` r
table(fused$M)
```


        1     2 
    10000 10000 

``` r
head(fused)
```

       M  btung                                 cooltype btuel
    1: 1 139800          Central air conditioning system 48500
    2: 1      0          Central air conditioning system 72300
    3: 1  37900 Individual window/wall or portable units 45000
    4: 1      0          Central air conditioning system 57900
    5: 1 103600          Central air conditioning system 29900
    6: 1  30000          Central air conditioning system  7740

When running in test mode, the ACS prediction dataset is restricted to
the first 10,000 rows and, by default, only two implicates are simulated
(for speed and file size). This is a quick(er) way to check if there are
any obvious issues or problems. It is a good idea to make sure both
`fusionInput()` and `fusionOutput()` are “passing” start-to-finish in
test mode before trying to your final (and more time-consuming) fusion.

In practice, we sometimes need to “do fusion” in a secure server
environment. Since installing the complete fusionData package just to
access `fusionOutput()` is overkill (and a pain), a copy of
`fusionOutput()` is silently exported in the fusion*Model* package. This
allows someone to install and load only the fusionModel package in a
server environment and then use `fusionOutput()` to do the fusion step –
assuming the server has access to the necessary input files.
