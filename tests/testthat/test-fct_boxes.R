test_that("get_box_data() returns an none-empty dataframe and create_box() returns a character string", {
  input <- list(
    beobachtungsregion = "Kanton St.Gallen",
    referenzregion = "Schweiz",
    beobachtungsjahr = 2021,
    referenzjahr = 2018,
    markt = sgtourism::meta_countries$Country2[4]
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

  # data should have the following variable names
  expect_true("Monat" %in% names(df_beobachtungsregion))
  expect_true("Referenz" %in% names(df_beobachtungsregion))
  expect_true("Ankuenfte" %in% names(df_beobachtungsregion))
  expect_true("Logiernaechte" %in% names(df_beobachtungsregion))

  df_box_beobachtung <- get_box_data(
    data = df_beobachtungsregion,
    months_selected = c(1,2),
    input = input
  )

  expect_s3_class(df_box_beobachtung, "data.frame")
  expect_true(nrow(df_box_beobachtung) > 1)
  expect_true(ncol(df_box_beobachtung) > 1)
  expect_equal(names(df_box_beobachtung), c("Indicator", "beobachtungsjahr", "referenzjahr", "diff_abs", "percent_change"))

  df_box_referenz <- get_box_data(
    data = df_referenzregion,
    months_selected = c(1,2)
  )
  expect_s3_class(df_box_referenz, "data.frame")
  expect_true(nrow(df_box_referenz) > 1)
  expect_true(ncol(df_box_referenz) > 1)
  expect_equal(names(df_box_referenz), c("Indicator", "beobachtungsjahr", "referenzjahr", "diff_abs", "percent_change"))

  box_ankuenfte <- create_box(
    input = input,
    df_box_beobachtung = df_box_beobachtung,
    df_box_referenz = df_box_referenz,
    indicator = "Ankuenfte"
  )
  expect_equal(class(box_ankuenfte), "character")

  box_dur_stay <- create_box_dur_stay(
    input = input,
    df_box_beobachtung = df_box_beobachtung,
    df_box_referenz = df_box_referenz,
    indicator = "dur_stay"
  )
  expect_equal(class(box_dur_stay), "character")

  box_bed_occ <- create_box_pct(
    input = input,
    df_box_beobachtung = df_box_beobachtung,
    df_box_referenz = df_box_referenz,
    indicator = "bed_occ"
  )
  expect_equal(class(box_bed_occ), "character")

  df_box_beobachtung_by_markt <- get_box_data_markt(
    data = df_beobachtungsregion,
    months_selected = c(1,2),
    input = input
  )
  df_box_referenz_by_markt <- get_box_data_markt(
    data = df_beobachtungsregion,
    months_selected = c(1,2),
    input = input
  )
  box_ankuenfte_markt_country <- create_box(
    input = input,
    df_box_beobachtung = df_box_beobachtung_by_markt,
    df_box_referenz = df_box_referenz_by_markt,
    indicator = "Ankuenfte"
  )

  expect_equal(class(box_ankuenfte_markt_country), "character")
})

test_that("create_box_title() returns shiny tags with correct text", {
  input <- list(
    beobachtungsjahr = 2015
  )
  title_box_with_popover <- create_box_title(
    title = "Some text",
    input_id = "myID",
    input = input,
    popover_content = "Some text showed when clicking on the question icon followed by the text"
  )
  expect_s3_class(title_box_with_popover, "shiny.tag")
  expect_equal(title_box_with_popover$children[[1]], tags$b("Some text 2015"))
  expect_s3_class(title_box_with_popover$children[[2]], "shiny.tag.list")
})
