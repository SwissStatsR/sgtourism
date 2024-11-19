library(hexSticker)
library(magick)
library(rsvg)

sticker_obj <- sticker(
  subplot = "data-raw/bag-holiday-tourism.svg",
  package = "sgtourism",
  h_fill = "#009933",
  p_color = "white",
  p_y = 0.7,
  p_size = 22,
  s_width = 0.35,
  h_size = 0,
  s_x = 1,
  s_y = 1.25,
  filename = "man/figures/logo.png")

logo <- magick::image_read("man/figures/logo.png")
magick::image_scale(logo, "400") |>
  magick::image_write(path = "man/figures/logo.png", format = "png")
