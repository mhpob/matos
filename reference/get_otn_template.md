# Download Ocean-Tracking-Network-style metadata templates

Download Ocean-Tracking-Network-style metadata templates

## Usage

``` r
get_otn_template(
  template_type = c("receiver", "tag", "glider"),
  out_dir = NULL,
  quiet = FALSE
)
```

## Arguments

- template_type:

  Character string. One of: "receiver" (default), the deployment data
  submittal template; "tag", the tagging data submittal template; or
  "glider", the wave and Slocum glider metadata template.

- out_dir:

  Optional character string noting where you would like the file to be
  downloaded. Defaults to the working directory.

- quiet:

  suppress status messages from `download.file`

## Value

Ocean Tracking Network metadata template in XLSX format.

## Examples

``` r
if (FALSE) { # all(skip_example_on_cran(), skip_example_on_runiverse())
# Tag metadata template downloaded to working directory
get_otn_template()

# Glider metadata template downloaded to temporary directory
get_otn_template("glider", out_dir = tempdir())
}
```
