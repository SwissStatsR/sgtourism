test_that("create_gt_title() works", {
  input <- list(
    beobachtungsregion = "Kanton St.Gallen",
    beobachtungsjahr = 2015,
    referenzjahr = 2016
  )
  expect_snapshot(create_gt_title(input = input, input_region = input$beobachtungsregion))
})

test_that("create_gt_data_beobachtung() returns a none-empty data.frame with expected variable names and create_gt_beobachtungsregion() return a gt tbl", {

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
  df_box_beobachtung <- get_box_data(
    data = df_beobachtungsregion,
    months_selected = months_selected
  )

  gt_data_data_beobachtung <- create_gt_data_beobachtung(
    df_beobachtungsregion = df_beobachtungsregion,
    df_box_beobachtung = df_box_beobachtung,
    months_selected = months_selected
  )

  expect_s3_class(gt_data_data_beobachtung, "data.frame")
  expect_true(nrow(gt_data_data_beobachtung) > 1)
  expect_true(ncol(gt_data_data_beobachtung) > 1)
  expect_equal(names(gt_data_data_beobachtung), c("Country", "beobachtungsjahrLN", "percent_changeLN", "dur_stay_beobachtung", "dur_stay_diff_abs", "marktanteil", "marktanteil_diff"))

  gt_beobachtungsregion <- create_gt_beobachtungsregion(
    data = gt_data_data_beobachtung,
    input = input
  )

  expect_s3_class(gt_beobachtungsregion, "gt_tbl")
})

test_that("create_gt_data_referenzregion() returns a none-empty data.frame with expected variable names and create_gt_referenzregion() return a gt tbl", {

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
  df_box_beobachtung <- get_box_data(
    data = df_beobachtungsregion,
    months_selected = months_selected
  )
  df_referenzregion <- filter_data(
    data = sgtourism::df_ueberblick,
    input_region = input$referenzregion,
    input = input
  )
  df_box_referenz <- get_box_data(
    data = df_referenzregion,
    months_selected = c(1,2)
  )

  gt_data_beobachtung <- create_gt_data_beobachtung(
    df_beobachtungsregion = df_beobachtungsregion,
    df_box_beobachtung = df_box_beobachtung,
    months_selected = months_selected
  )

  gt_table_data_referenz <- create_gt_data_referenzregion(
    df_referenzregion = df_referenzregion,
    df_box_referenz = df_box_referenz,
    months_selected = months_selected,
    gt_data_beobachtung = gt_data_beobachtung
  )

  expect_s3_class(gt_table_data_referenz, "data.frame")
  expect_true(nrow(gt_table_data_referenz) > 1)
  expect_true(ncol(gt_table_data_referenz) > 1)
  expect_equal(names(gt_table_data_referenz), c("Country", "beobachtungsjahrLN", "percent_changeLN", "dur_stay_beobachtung", "dur_stay_diff_abs", "marktanteil", "marktanteil_diff"))

  gt_referenzregion <- create_gt_referenzregion(
    gt_table_data_referenz = gt_table_data_referenz,
    gt_data_beobachtung = gt_data_beobachtung,
    input = input
  )

  expect_s3_class(gt_referenzregion, "gt_tbl")
})

