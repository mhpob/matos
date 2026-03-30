# Search for tags on the MATOS website

This function is an interface to [MATOS' tag search
page](https://matos.asascience.com/search), with the result of a CSV
downloaded into your working directory. Be aware: these downloads can
take a *long* time, especially if you have many tags or are searching
over a long period of time.

## Usage

``` r
get_tag_search(tags, start_date, end_date, import = F)
```

## Arguments

- tags:

  Character vector of tags. Will be coerced into CSV when POSTing to the
  website.

- start_date:

  Character string listing the start date in MM/DD/YYYY format. If no
  dates are provided, all tag detections are returned.

- end_date:

  Character string listing the end date in MM/DD/YYYY format. If no
  dates are provided, all tag detections are returned.

- import:

  Should the downloaded data be imported into R as a data frame? Default
  is FALSE.

## Examples

``` r
if (FALSE) { # \dontrun{
get_tag_search(
  tags = paste0("A69-1601-254", seq(60, 65, 1)),
  start_date = "03/01/2016",
  end_date = "04/01/2016"
)
} # }
```
