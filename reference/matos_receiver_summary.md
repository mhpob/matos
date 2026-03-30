# Create summary reports of receiver project data from the OTN data push

Create summary reports of receiver project data from the OTN data push

## Usage

``` r
matos_receiver_summary(
  matos_project = NULL,
  qualified = NULL,
  unqualified = NULL,
  deployment = NULL,
  ...
)
```

## Arguments

- matos_project:

  MATOS project number or name that you wish to have summarized

- qualified, unqualified:

  Default is NULL: OTN qualified or unqualified detections will be
  downloaded from MATOS and unzipped. If you do not wish to download
  your files (or you're not a member of ACT), this argument also accepts
  a character vector of file paths of your qualified/unqualified
  detections. These can be CSVs or zipped folders.

- deployment:

  File path of user-supplied master OTN receiver deployment metadata.

- ...:

  Arguments passed to
  [`otndo::make_receiver_push_summary`](https://otndo.obrien.page/reference/make_receiver_push_summary.html)

## No files provided

If you only provide your ACT project number or title and leave all of
the arguments as their defaults, this function will ask you to log in
then proceed to download all of the necessary files. If you provide
already-downloaded files you can speed up this process substantially.

## Output

This function creates an HTML report that can be viewed in your web
browser.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using only the ACT/MATOS project number:
matos_receiver_summary(87)

# Providing a local file:
matos_receiver_summary(87, deployment = "my_master_deployment_metadata.xlsx")

# Get a summary fo what has changed since a particular date:
matos_receiver_summary(87, since = "2022-05-01")
} # }
```
