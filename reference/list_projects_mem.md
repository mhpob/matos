# Memoised list_projects function for internal use.

Memoised list_projects function for internal use.

## Usage

``` r
list_projects_mem(what, read_access, quiet, warn_multimatch)
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

- warn_multimatch:

  Warn you if there have been multiple project matches? Defaults to
  TRUE.
