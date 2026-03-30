# Download OTN data extraction files from the MATOS website

`get_extract_file` downloads files from the MATOS website. This is
best-used in conjunction with
[`list_extract_files`](https://www.obrien.page/matos/reference/list_extract_files.md).

## Usage

``` r
get_extract_file(
  file = NULL,
  project = NULL,
  url = NULL,
  out_dir = getwd(),
  overwrite = FALSE,
  to_vue = FALSE,
  quiet = FALSE
)
```

## Arguments

- file:

  A character vector listing the name of the file, or a numeric listing
  the index as found from
  [`list_extract_files`](https://www.obrien.page/matos/reference/list_extract_files.md).

- project:

  A character vector listing the full name of the project, or a numeric
  listing the project number.

- url:

  The URL of the file to be downloaded.

- out_dir:

  Character. What directory/folder do you want the file saved into?
  Default is the current working directory.

- overwrite:

  Logical. Do you want a file with the same name overwritten? Passed to
  httr::write_disk.

- to_vue:

  Logical. Convert to VUE export format?

- quiet:

  Logical. Do you want to silence matos' updates? Default is FALSE.

## Examples

``` r
if (FALSE) { # all(skip_example_on_cran(), skip_example_on_runiverse())
# If you know the direct URL to your file, you don't need the file or project names:
get_extract_file(url = "https://matos.asascience.com/projectfile/download/327")

get_extract_file(file = 1, project = 87)
}
```
