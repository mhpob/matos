# Create summary reports of receiver project data from the OTN data push

Create summary reports of receiver project data from the OTN data push

## Usage

``` r
matos_tag_summary(matos_project = NULL, matched = NULL, ...)
```

## Arguments

- matos_project:

  MATOS project number or name that you wish to have summarized

- matched:

  Default is NULL: OTN matched detections will be downloaded from MATOS
  and unzipped. If you do not wish to download your files, this argument
  also accepts a character vector of file paths of your matched
  detections. These can be CSVs or zipped folders.

- ...:

  Arguments passed to
  [`otndo::make_tag_push_summary`](https://otndo.obrien.page/reference/make_tag_push_summary.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# You can just use your ACT project number
matos_tag_summary(87)

# Or provide an optional date to summarize "What's New".
matos_tag_summary(87, since = "2018-11-01")
} # }
```
