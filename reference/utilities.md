# Internal functions used by `matos`

Non-exported utility functions used by other functions in `matos`.

## Usage

``` r
get_file_list(project_number, data_type, force = FALSE)

get_file_list_mem(project_number, data_type)

get_project_number(project_name, matos_projects = NULL)

get_project_name(project_number, matos_projects = NULL)

html_table_to_df(html_file_list)

login_check(url = "https://matos.asascience.com/report/submit")

project_check(project, return_projects = FALSE)

scrape_file_urls(html_file_list)

download_process(url, out_dir, overwrite, to_vue = FALSE, quiet = FALSE)
```

## Arguments

- project_number:

  Number of the project

- data_type:

  one of "dataextractionfiles" for OTN detection extracts or
  "downloadfiles" for the uploaded project files.

- force:

  Do you want to reset the cache and re-ping the database? Defaults to
  false.

- project_name:

  Character string of the full MATOS project name. This will be the big
  name in bold at the top of your project page, not the "Project Title"
  below it. Will be coerced to all lower case, so capitalization doesn't
  matter.

- matos_projects:

  Data frame. Used to pass the MATOS project list from `project_check`.

- html_file_list:

  Listed files in HTML form. Always the result of `get_file_list`

- url:

  The (protected) URL that the overlapping function is trying to call.

- project:

  MATOS project ID. Can be the name or number of the project.

- return_projects:

  Logical. Do you want `project_check` to return the list of projects?
  Used to not ping the website too much in one function call.

- out_dir:

  Character. To what directory would you like your files downloaded?
  Defaults to the current working directory.

- overwrite:

  Logical. Do you want to overwrite existing files that have the same
  name (`TRUE`) or protect yourself against doing this (`FALSE`, the
  default)?

- to_vue:

  Logical. Should the data be converted to match that of VUE's CSV
  export? Defaults to FALSE.

- quiet:

  Logical. Do you want to silence matos' updates? Default is FALSE.

## Details

`get_file_list` checks to see if it should re-evaluate itself, then
wraps `get_file_list_mem` which is the actual workhorse.

`get_file_list_mem` memoised function which scrapes the HTML associated
with the project or data extraction files page provided with a given
project.

`get_project_number` finds the internal MATOS number associated with
each project by scraping the HTML of the main MATOS projects page.

`get_project_name` finds the MATOS project name associated with the
given project number by scraping the HTML of the main MATOS projects
page.

`html_table_to_df` converts the HTML table provided by `get_file_list`
into a R-usable data frame.

`login_check` pings protected URLs and calls `matos_login` when referred
to the login page.

`project_check`

`scrape_file_urls` is used internally by `html_table_to_df` to extract
the URLs associates with each "Download" link.

`download_process` is used internally by `get_project_file` and
`get_extract_file`
