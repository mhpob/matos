test_that("displays message on start", {
  expect_message(
    matos:::.onAttach(),
    "By continuing.*MATOS\\.User\\.Agreement"
  )
})
