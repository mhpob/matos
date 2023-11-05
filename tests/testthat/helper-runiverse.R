skip_on_Runiverse <- function() {
  skip_if(env_var_is_true("MY_UNIVERSE"), "On R-universe.")
}
