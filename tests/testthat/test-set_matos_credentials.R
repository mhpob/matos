test_that("errors if credentials exist", {
  # Create faux creds
  home <- Sys.getenv("HOME")
  renv_path <- file.path(home, ".Renviron")

  if (!file.exists(renv_path)) {
    file.create(renv_path)
  }

  renv <- readLines(renv_path)

  write("MATOS_USER=\"abc\"", renv_path, sep = "\n", append = TRUE)
  write("MATOS_USER=\"123\"", renv_path, sep = "\n", append = TRUE)

  # Test
  expect_error(
    set_matos_credentials(),
    "Some MATOS credentials already exist"
  )

  # Clean up
  write(renv, renv_path)
})

test_that("", {
  skip_on_os(c("windows", "mac"))
  # Create faux creds
  home <- Sys.getenv("HOME")
  renv_path <- file.path(home, ".Renviron")

  if (!file.exists(renv_path)) {
    file.create(renv_path)
  }

  renv <- readLines(renv_path)

  # Test
  expect_message(
    set_matos_credentials(),
    "your platform is not supported"
  ) |>
    expect_message("your platform is not supported") |>
    expect_message("Your MATOS credentials have been stored in your \\.Renviron")

  # Clean up
  write(renv, renv_path)
})
