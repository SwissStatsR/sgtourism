test_that("create_months_ui() returns a shiny.tag with correct text label", {
  input <- list(
    beobachtungsregion = "Schweiz",
    beobachtungsjahr = 2015,
    referenzjahr = 2016
  )
  df_beobachtungsregion <- filter_data(
    data = sgtourism::df_ueberblick,
    input_region = input$beobachtungsregion,
    input = input
  )

  # variable names needed in create_months_ui()
  expect_true("Referenz" %in% names(df_beobachtungsregion))
  expect_true("Jahr" %in% names(df_beobachtungsregion))
  expect_true("Monat" %in% names(df_beobachtungsregion))

  month_ui <- create_months_ui(
    data = df_beobachtungsregion,
    input = input,
    label_text = "My text label",
    ns = function(x){}
  )
  expect_s3_class(month_ui, "shiny.tag")
  expect_equal(month_ui$children[[1]]$children[[1]], "My text label")
})

test_that("select_months() returns correct numeric vector", {
  input_monat_single <- list(
    monat = "2020-01-01"
  )
  input_monat_multiple <- list(
    monat = c("2020-01-01", "2020-04-01")
  )
  month_single <- select_months(
    input = input_monat_single
  )
  month_multiple <- select_months(
    input = input_monat_multiple
  )
  expect_equal(month_single, c(1))
  expect_equal(month_multiple, c(1, 2, 3, 4))
})
