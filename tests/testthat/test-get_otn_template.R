test_that("default downloads otn deployment metadata", {
  skip_if_offline()

  expect_invisible(
    file_location <- get_otn_template()
  )

  expect_type(file_location, "character")
  expect_true(file.exists(file_location))

  unlink(file_location)
})

test_that("explicit download of otn deployment metadata", {
  skip_if_offline()

  expect_invisible(
    file_location <- get_otn_template("receiver")
  )

  expect_type(file_location, "character")
  expect_true(file.exists(file_location))

  unlink(file_location)
})

test_that("download otn tag metadata", {
  skip_if_offline()

  expect_invisible(
    file_location <- get_otn_template("tag")
  )

  expect_type(file_location, "character")
  expect_true(file.exists(file_location))

  unlink(file_location)
})

test_that("download otn glider metadata", {
  skip_if_offline()

  expect_invisible(
    file_location <- get_otn_template("glider")
  )

  expect_type(file_location, "character")
  expect_true(file.exists(file_location))

  unlink(file_location)
})

test_that("custom out directory works", {
  skip_if_offline()

  td <- file.path(tempdir(), "test-get_otn_template")
  dir.create(td)

  expect_invisible(
    file_location <- get_otn_template(out_dir = td)
  )

  expect_type(file_location, "character")
  expect_true(file.exists(file_location))
  expect_equal(
    normalizePath(td),
    normalizePath(dirname(file_location))
  )

  unlink(td, recursive = TRUE)
})
