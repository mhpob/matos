# Download all data extraction files that were updated after a certain date

This is a loop around
[`get_extract_file`](https://www.obrien.page/matos/reference/get_extract_file.md).

## Usage

``` r
get_extract_updates(
  project,
  since = NULL,
  detection_type = c("all", "matched", "external", "qualified", "sentinel",
    "unqualified"),
  out_dir = getwd(),
  overwrite = FALSE,
  to_vue = FALSE,
  progress = TRUE,
  quiet = TRUE
)
```

## Arguments

- project:

  Either the project code, the project number (the number in your
  project page URL), or the full name of the project (the big name in
  bold on your project page, *not* the "Project Title").

- since:

  Only list files uploaded after this date. Optional, but must be in
  YYYY-MM-DD format.

- detection_type:

  one of, or a vector of, "all" (default), "matched", "external",
  "qualified", "sentinel_tag", or "unqualified". Partial matching is
  allowed, and will repair to the correct argument if spaces or the
  words detection(s)" are included. More information on data types can
  be found on [OTN's
  website](https://members.oceantrack.org/data/otn-detection-extract-documentation-matched-to-animals).

- out_dir:

  Character. What directory/folder do you want the file saved into?
  Default is the current working directory. Passed to
  [`httr::write_disk`](https://httr.r-lib.org/reference/write_disk.html)
  via
  [`get_extract_file`](https://www.obrien.page/matos/reference/get_extract_file.md).

- overwrite:

  Logical. Do you want a file with the same name overwritten? Passed to
  [`httr::write_disk`](https://httr.r-lib.org/reference/write_disk.html)
  via
  [`get_extract_file`](https://www.obrien.page/matos/reference/get_extract_file.md).

- to_vue:

  Logical. Should the data be converted to match that of VUE's CSV
  export?

- progress:

  Logical. Do you want a progress bar? Default is TRUE.

- quiet:

  Logical. Do you want to silence matos' updates? Default is TRUE.

## Value

Saves the requested files to your computer and provides a list of file
paths of the downloaded files.

## Examples

``` r
if (FALSE) { # all(skip_example_on_cran(), skip_example_on_runiverse())
# Download files from the MDWEA project updated in the November 2023 data push
#   (you'll need to use a project for which you have permissions).
get_extract_updates(project = "MDWEA", since = "2023-11-01")

# Match the VUE CSV export style
get_extract_updates(project = 160, to_vue = TRUE)
}
```
