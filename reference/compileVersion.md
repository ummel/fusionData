# Compile fusionACS Database Version and Optional Public Release

Compiles a full-resolution, multi-implicate version of the `fusionACS`
microdata database on the Yale HPC facility (Milgram cluster) from
processed ACS microdata, donor survey implicates, and UrbanPop synthetic
population files. Optionally generates and uploads a single-implicate
pseudo-sample release to the fusionACS package GitHub repository using
`piggyback`. The release is then accessible to the public via
`fusionACS::get_microdata()`.

## Usage

``` r
compileVersion(version_date = as.character(Sys.Date()), public_release = TRUE)
```

## Arguments

- version_date:

  Character. Version release identifier formatted as `"YYYY-MM-DD"`.
  Defaults to the current system date (`as.character(Sys.Date())`).

- public_release:

  Logical. If `TRUE` (default), generates and uploads the
  single-implicate public pseudo-sample dataset to the fusionACS package
  GitHub repository.

## Value

Invisibly returns the absolute path to the compiled version directory
(`target_dir`).

## Details

Execution is strictly restricted to the Yale HPC Milgram cluster and
requires the working directory to be set to
`/gpfs/milgram/project/rao/shared/fusionACS/fusionData`. Parallel thread
allocation for Apache Arrow operations is automatically configured based
on SLURM CPU task allocation (`SLURM_CPUS_PER_TASK`).

The function executes in two distinct phases:

1.  **Full Database Compilation (`!dir.exists(target_dir)`):**
    Aggregates geographic concordance tables, spatial block group
    crosswalks, UrbanPop synthetic population location lookups, ACS
    household and person microdata, and donor survey implicates into
    structured Apache Arrow Parquet datasets at
    `fusionACS/versions/<version_date>`. To optimize performance and
    storage, unchanged inputs relative to the most recent prior version
    are automatically detected and linked via filesystem symbolic links.
    If an unhandled error occurs mid-compilation, an `on.exit` hook
    automatically removes the incomplete target directory to prevent
    corruption.

2.  **Public Release Generation (`public_release = TRUE`):** Constructs
    a public-facing pseudo-sample in
    `fusionACS/public_releases/<version_date>`. A single implicate
    (`M = 1`) is isolated for donor surveys, and UrbanPop households are
    assigned to 2010/2020 Census Block Groups via a weighted exponential
    random sampling algorithm. Output components are linked, bundled
    into three partitioned uncompressed `.tar` archives (dereferencing
    symlinks via `tar -h`), and uploaded as GitHub release assets under
    tag `<version_date>`.

If the compiled `target_dir` already exists on disk and
`public_release = TRUE`, the function automatically skips database
compilation and proceeds directly to public release generation and
upload. If an identical GitHub release tag already exists, the user is
prompted before overwriting.

## Note

The public release phase actively queries the GitHub API with local
cache clearing
([`piggyback::.pb_cache_clear()`](https://docs.ropensci.org/piggyback/reference/dot-pb_cache_clear.html))
to account for propagation delays during release deletion and creation.

## See also

[`pb_upload`](https://docs.ropensci.org/piggyback/reference/pb_upload.html)
