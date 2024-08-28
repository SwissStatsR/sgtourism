test_that("text_with_popover_icon() returns shiny tags with correct text", {
  text_with_popover <- text_with_popover_icon(
    text = "Some text",
    input_id = "myID",
    popover_content = "Some text showed when clicking on the question icon followed by the text"
  )
  expect_s3_class(text_with_popover, "shiny.tag")
  expect_equal(text_with_popover$children[[1]], "Some text")
  expect_s3_class(text_with_popover$children[[2]], "shiny.tag.list")
})
