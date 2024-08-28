test_that("plot_line_years_title() works", {
  input <- list(
    beobachtungsregion = "Kanton St.Gallen"
  )
  expect_equal(plot_line_years_title(input = input, title = "Logiern\u00e4chte"), tags$b("Logiern\u00e4chte Kanton St.Gallen"))
})

test_that("plot_line_years() returns a echarts4r object", {
  input <- list(
    beobachtungsregion = "Kanton St.Gallen",
    beobachtungsjahr = 2015,
    referenzjahr = 2016
  )
  months_selected <- c(1,2)

  df_beobachtungsregion <- filter_data(
    data = sgtourism::df_ueberblick,
    input_region = input$beobachtungsregion,
    input = input
  )

  expect_true("Monat" %in% names(df_beobachtungsregion))
  expect_true("Jahr" %in% names(df_beobachtungsregion))
  expect_true("Logiernaechte" %in% names(df_beobachtungsregion))

  entwicklung_logiernaechte <- plot_line_years(
    data = df_beobachtungsregion,
    months_selected = months_selected,
    variable = "Logiernaechte"
  )

  expect_s3_class(entwicklung_logiernaechte, "echarts4r")
  expect_identical(entwicklung_logiernaechte$x$opts$legend$data[[1]], "2015")
  expect_identical(entwicklung_logiernaechte$x$opts$legend$data[[2]], "2016")
})

test_that("plot_bar_regions_title() works", {
  input <- list(
    referenzjahr = "2015",
    beobachtungsjahr = "2016"
  )
  expect_equal(plot_bar_regions_title(input = input, title = "Ver\u00e4nderung Logiern\u00e4chte"), tags$b(paste("Ver\u00e4nderung Logiern\u00e4chte 2015 - 2016")))
})

test_that("plot_bar_regions() returns a echarts4r object", {

  input <- list(
    beobachtungsregion = "Kanton St.Gallen",
    referenzregion = "Schweiz",
    beobachtungsjahr = 2015,
    referenzjahr = 2016
  )
  months_selected <- c(1,2)

  df_beobachtungsregion <- filter_data(
    data = sgtourism::df_ueberblick,
    input_region = input$beobachtungsregion,
    input = input
  )
  df_referenzregion <- filter_data(
    data = sgtourism::df_ueberblick,
    input_region = input$referenzregion,
    input = input
  )

  expect_true("Monat" %in% names(df_beobachtungsregion))
  expect_true("Jahr" %in% names(df_beobachtungsregion))
  expect_true("Referenz" %in% names(df_beobachtungsregion))
  expect_true("Ankuenfte" %in% names(df_beobachtungsregion))

  veraenderung_logiernaechte <- plot_bar_regions(
    input = input,
    df_beobachtungsregion = df_beobachtungsregion,
    df_referenzregion = df_referenzregion,
    months_selected = months_selected,
    variable = "Ankuenfte"
  )

  expect_s3_class(veraenderung_logiernaechte, "echarts4r")
  expect_identical(names(veraenderung_logiernaechte$x$data), c("Kanton St.Gallen", "Schweiz"))
})

