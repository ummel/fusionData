# Install fusionData package locally

Fast and safe way to (re-)install the local version of the `fusionData`
package directly from source, mirroring RStudio's 'Install Package'
build action. Compiles internal survey dictionary files
([`compileDictionary`](https://ummel.github.io/fusionData/reference/compileDictionary.md)),
updates package documentation and namespace via
[`roxygenise`](https://roxygen2.r-lib.org/reference/roxygenize.html),
and reinstalls the package.

## Usage

``` r
installPackage()
```

## Details

When underlying survey microdata or spatial reference datasets are
updated, running this function ensures that regenerated `.rda` files in
`data/` are properly compiled, documented, and made accessible
throughout the package.

This function unloads the active `fusionData` namespace prior to
installation to prevent file-locking errors, and passes `build = FALSE`
to reinstall directly from local source files in seconds without
archiving an intermediate `.tar.gz` bundle.

## Examples

``` r
if (FALSE) { # \dontrun{
installPackage()
} # }
```
