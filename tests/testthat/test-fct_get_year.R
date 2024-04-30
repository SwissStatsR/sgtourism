test_that("get_years() returns a list", {
  expect_type(get_years(data = df_ueberblick), "list")
})
