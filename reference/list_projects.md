# List MATOS projects

This function scrapes the table found at
<https://matos.asascience.com/project> and combines it with project
metadata stored on the [Ocean Tracking Network
Geoserver](https://members.oceantrack.org/geoserver/web/). This table
provides the full name of the project, collection code, MATOS project
number, MATOS project page URL, project status, full name, citation,
website, project type, area, and abstract. You do not need to log in via
`matos_login` or have any permissions to view/download this table.

## Usage

``` r
list_projects(
  what = c("all", "mine"),
  read_access = TRUE,
  quiet = FALSE,
  force = FALSE,
  warn_multimatch = TRUE
)
```

## Arguments

- what:

  What list of projects do you want returned: all projects ("all",
  default) or your projects ("mine")?

- read_access:

  If listing your projects, do you want to only list projects for which
  you have file-read permission? Defaults to TRUE, though there is
  significant speed up if switched to FALSE.

- quiet:

  Do you want to suppress messages regarding matched projects? Defaults
  to FALSE.

- force:

  Do you want to reset the cache and re-ping the database? Defaults to
  FALSE.

- warn_multimatch:

  Warn you if there have been multiple project matches? Defaults to
  TRUE.

## Examples

``` r
if (FALSE) { # all(skip_example_on_cran(), skip_example_on_runiverse())
# List all projects, the default:
list_projects()

# List your projects (which may contain some for which you do not have read access):
list_projects("mine", read_access = F)
}
```
