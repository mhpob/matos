# List personal MATOS projects

This function lists the functions for which the logged-on user has
permissions.

## Usage

``` r
list_my_projects(read_access = TRUE, force = FALSE, warn_multimatch = TRUE)
```

## Arguments

- read_access:

  Do you want to only list projects for which you have file-read
  permission? Defaults to TRUE, though there is significant speed up if
  switched to FALSE.

- force:

  Do you want to reset the cache and re-ping the database? Defaults to
  false.

- warn_multimatch:

  Warn you if there have been multiple project matches? Defaults to
  TRUE.

## Examples

``` r
if (FALSE) { # all(skip_example_on_cran(), skip_example_on_runiverse())
# After logging in, just type the following:
list_my_projects()
}
```
