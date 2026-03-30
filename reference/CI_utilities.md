# Miscellaneous functions for package checking, building, and CI

`skip_example_on_ci` and `skip_example_on_runiverse` check the
environment for variables called "CI" and "MY_UNIVERSE", respectively,
and return `TRUE` if it does not exist. Used to run examples if the
package is being built locally and there's a chance that `vdat.exe`
exists. If the package is being built on a continuous integration
platform like GitHub Actions, the "CI" variable will be `TRUE` and
`skip_example_on_ci` will return `FALSE`. If it is being built locally,
"CI" will be "" and `skip_example_on_ci` will return `TRUE`. Similarly,
if the package is being built on R-Universe, the "MY_UNIVERSE" variable
will have your universe's name.

## Usage

``` r
skip_example_on_ci()

skip_example_on_runiverse()

skip_example_on_cran()
```
