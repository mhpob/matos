# matos

{matos} is an API to the [Mid-Atlantic Acoustic Telemetry Observing
System website](https://matos.asascience.com/), powered by a suite of
[httr](https://httr.r-lib.org/) and
[rvest](https://rvest.tidyverse.org/) functions.

Please note that you will need a MATOS account, [which you can sign up
for here](https://matos.asascience.com/account/signup), in order to
interface with any project-specific files.

## Installation

You can install the most-up-to-date version from
[R-universe](https://mhpob.r-universe.dev/matos) or
[GitHub](https://github.com/mhpob/matos).

R-universe:

``` r
install.packages(
  "matos",
  repos = c(
    "https://mhpob.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

GitHub:

``` r
# install.packages("remotes")
remotes::install_github("mhpob/matos")
```

## Quick start

There are functions to list, download, and upload files to MATOS – check
out the vignettes for some worked examples. If you don’t want to read
and just want to get your detection files, here are the bare bones to
get you up and running:

### `list_my_projects`

Logs you into the MATOS database and pings each project in turn to see
if you have read/write access, listing those that you do.

### `list_extract_files`

Once you have a project name or number from `list_my_projects`, use this
function to list all of the files containing Ocean Tracking Network
matched detections.

### `get_extract_file`

Now that you have the URLs of the detection extract files from
`list_extract_files`, put it in here to download the file to your
computer.

## Development

As is noted above, this package is undergoing a ton of development. If
there’s something I missed, please [open an issue on
GitHub](https://github.com/mhpob/matos/issues) or [email me (Mike
O’Brien: obrien@umces.edu) directly](mailto:obrien@umces.edu).
