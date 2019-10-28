library(magick)
library(OpenImageR)



all_imgs <- fs::dir_ls(
  "D:/CSS/picture/picture classification/all together/division/test2/test3/cooler", 
  recursive = TRUE, 
  type = "file",
  glob = "*.jpg"
)


setwd("D:/CSS/picture/picture classification/all together/division/test2/test3/cooler")


for (im in all_imgs) {
  out <- try(magick::image_modulate(magick::image_read(im), brightness = 80, saturation = 120, hue = 90), silent = F)
  if (inherits(out, "try-error")) {
     fs::file_delete(im)
    message("removed image: ", im)
  }
}


readImage("D:/CSS/picture/picture classification/all together/division/test2/test3/cooler/19799_20190410113300_1.jpg")





all_imgs <- fs::dir_ls(
  "D:/CSS/picture/picture classification/all together/division/test2/test3/cooler", 
  recursive = TRUE, 
  type = "file",
  glob = "*.jpg"
)




