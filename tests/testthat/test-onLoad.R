test_that("functions already memoised", {
  expect_error(
    matos:::.onLoad(),
    "`f` must not be memoised"
  )
})
