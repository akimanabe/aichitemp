test_that("function exists", {
  expect_type(extract_location, "closure")
})

test_that("function extracts location", {
  expect_equal(
    extract_location("../../data-raw/aichi0-2002.csv"),
    "愛知(南知多）"
  )
})
