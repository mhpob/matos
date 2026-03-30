# List OTN data extraction files

This function list the file names, types, upload date, and URLs of OTN
data extraction files – basically everything you see in the *Data
Extraction Files* section of your project page. Because it is from your
project page, you will be prompted to log in.

## Usage

``` r
list_extract_files(
  project,
  detection_type = c("all", "matched", "external", "qualified", "sentinel",
    "unqualified"),
  since = NULL
)
```

## Arguments

- project:

  Either the project code, the project number (the number in your
  project page URL), or the full name of the project (the big name in
  bold on your project page, *not* the "Project Title").

- detection_type:

  one of, or a vector of, "all" (default), "matched", "external",
  "qualified", "sentinel_tag", or "unqualified". Partial matching is
  allowed, and will repair to the correct argument if spaces or the
  words detection(s)" are included. More information on data types can
  be found on [OTN's
  website](https://members.oceantrack.org/data/otn-detection-extract-documentation-matched-to-animals).

- since:

  Only list files uploaded after this date. Optional, but must be in
  YYYY-MM-DD format.

## Value

A data frame with columns of "project", "file_type", "detection_type",
'detection_year', 'upload_date', 'file_name', and "url".

## Details

`list_extract_files` is a wrapper around a web-scraping routine:

1.  find the project number if not provided, 2) download the HTML
    table, 3) parse the URL for each file, and 4) combine the table and
    URLs into a data frame. This function is most useful when
    investigating what files you have available, and then downloading
    them with
    [`get_extract_file`](https://www.obrien.page/matos/reference/get_extract_file.md).

`list_extract_files` lists files associated with the ACT_MATOS OTN node.
These are files listed on the *Data Extraction Files* page.

## Examples

``` r
if (FALSE) { # all(skip_example_on_cran(), skip_example_on_runiverse())
# List all extraction files using project number
list_extract_files(87)

# Or, just grab the matched receiver detections
list_extract_files(project = 87, detection_type = "qualified")

# OR list files using the project name
list_extract_files("umces boem offshore wind energy")
}
```
