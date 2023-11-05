skip_on_Runiverse <- function() {
  skip_if(testthat:::env_var_is_true("MY_UNIVERSE"), "On R-universe.")
}
