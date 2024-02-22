test_that("get_years() returns a list", {
  expect_type(get_years(data = sgtourism::df_prep), "list")
})
