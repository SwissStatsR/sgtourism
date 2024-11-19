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

# sticker_sgtourism <- sticker(
#   subplot = "inst/app/www/sg_flag.png",
#   package = "sgtourism",
#   h_fill = "#009933",
#   h_color = "black",
#   p_color = "white",
#   url = "github.com/statistikSG/sgtourism",
#   u_color = "white",
#   u_size = 4,
#   p_y = 1.35,
#   p_size = 22,
#   s_width = 0.3,
#   h_size = 1,
#   s_x = 1,
#   s_y = 0.68,
#   filename = "man/figures/logo.png")
#
# logo <- magick::image_read("man/figures/logo.png")
# magick::image_scale(logo, "400") |>
#   magick::image_write(path = "data-raw/logo.png", format = "png")
