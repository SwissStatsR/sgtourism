test_that("filter_data returns an none-empty dataframe with a new Reference variable", {
  input <- list(
    beobachtungsregion = "Kanton St.Gallen",
    beobachtungsjahr = 2015,
    referenzjahr = 2016
  )
  data_test <- sgtourism::df_ueberblick

  # data should have the following variable names
  expect_true("Aggregat" %in% names(data_test))
  expect_true("Jahr" %in% names(data_test))

  df_beobachtungsregion <- filter_data(
    data = data_test,
    input_region = input$beobachtungsregion,
    input = input
  )
  expect_s3_class(df_beobachtungsregion, "data.frame")
  expect_true(nrow(df_beobachtungsregion) > 1)
  expect_true(ncol(df_beobachtungsregion) > 1)
  expect_true("Referenz" %in% names(df_beobachtungsregion))
  expect_true("Monat" %in% names(df_beobachtungsregion))
  expect_true("Referenz" %in% names(df_beobachtungsregion))
})
