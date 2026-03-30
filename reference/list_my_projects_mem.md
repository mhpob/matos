# Memoised list_my_projects function for internal use

Memoised list_my_projects function for internal use

## Usage

``` r
list_my_projects_mem(read_access, warn_multimatch)
```

## Arguments

- read_access:

  Do you want to only list projects for which you have file-read
  permission? Defaults to TRUE, though there is significant speed up if
  switched to FALSE.

- warn_multimatch:

  Warn you if there have been multiple project matches? Defaults to
  TRUE.
