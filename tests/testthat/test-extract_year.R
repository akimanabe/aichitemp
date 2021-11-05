test_that("function exists", {
  expect_type(extract_year, "closure")
})

test_that("function extracts location", {
  expect_equal(
    extract_year("../../data-raw/aichi0-2002.csv"),
    as.numeric("2002")
  )
})
