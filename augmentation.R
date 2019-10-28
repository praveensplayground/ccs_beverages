library(OpenImageR)
library(EBImage)
library(imager)
library(magick)
library(lime)
library(adimpro)




setwd("D:/CSS/picture/picture classification/all together/worst")

pics<-list.files()



for (i in 1: length(pics)){
  
  
  setwd("D:/CSS/picture/picture classification/all together/worst")
  photo<-readImage(pics[i])*0.6
  setwd("D:/CSS/picture/picture classification/aug22/worst")
  writeImage(photo, paste0("aug1",pics[i]))
  # print(photo)
  
  setwd("D:/CSS/picture/picture classification/all together/worst")
  photo<-readImage(pics[i])
  photo<-gblur(photo, sigma=2)
  setwd("D:/CSS/picture/picture classification/aug22/worst")
  writeImage(photo, paste0("aug2",pics[i]))
  # 
  setwd("D:/CSS/picture/picture classification/all together/worst")
  photo<-readImage(pics[i])*1.3
  setwd("D:/CSS/picture/picture classification/aug22/worst")
  writeImage(photo, paste0("aug3",pics[i]))
  # 
  setwd("D:/CSS/picture/picture classification/all together/worst")
  # photo<-readImage(pics[i])
  photo<-load.image(pics[i])
  # photo<-flipImage(photo, mode = 'vertical')
  photo<-mirror(photo, "x")
  setwd("D:/CSS/picture/picture classification/aug22/worst")
  save.image(photo,file= paste0("aug4",pics[i]),quality = 0.9)
  # writeImage(photo, paste0("aug4",pics[i]))
  

  # setwd("D:/CSS/picture/picture classification/all together/worst")
  # photo<-readImage(pics[i])
  # photo<-image_flip(photo)
  # setwd("D:/CSS/picture/picture classification/aug22/worst")
  # writeImage(photo, paste0("aug5",pics[i]))
  # rotate.image(img, angle = 90, compress=NULL)
}


aug_worst<-sample(list.files(),1200)

file.copy(aug_worst, "D:/CSS/picture/picture classification/all together/worst")




setwd("D:/CSS/picture/picture classification/all together/good")

pics<-list.files()


for (i in 1: length(pics)){
  
  
  
  setwd("D:/CSS/picture/picture classification/all together/good")
  photo<-readImage(pics[i])*0.7
  setwd("D:/CSS/picture/picture classification/aug22/good")
  writeImage(photo, paste0("aug1",pics[i]))
  # print(photo)
  
  setwd("D:/CSS/picture/picture classification/all together/good")
  photo<-readImage(pics[i])
  photo<-gblur(photo, sigma=2)
  setwd("D:/CSS/picture/picture classification/aug22/good")
  writeImage(photo, paste0("aug2",pics[i]))
  
  setwd("D:/CSS/picture/picture classification/all together/good")
  photo<-readImage(pics[i])*1.5
  setwd("D:/CSS/picture/picture classification/aug22/good")
  writeImage(photo, paste0("aug3",pics[i]))
  
  setwd("D:/CSS/picture/picture classification/all together/good")
  # photo<-readImage(pics[i])
  photo<-load.image(pics[i])
  # photo<-flipImage(photo, mode = 'vertical')
  photo<-mirror(photo, "x")
  setwd("D:/CSS/picture/picture classification/aug22/good")
  save.image(photo,file= paste0("aug4",pics[i]),quality = 0.9)
  # writeImage(photo, paste0("aug4",pics[i]))
  
  
}

aug_good<-sample(list.files(),900)

file.copy(aug_good, "D:/CSS/picture/picture classification/all together/good")




setwd("D:/CSS/picture/picture classification/all together/bad")

pics<-list.files()


for (i in 1: length(pics)){
  
  # for (j in 1: length(y)){
  
  setwd("D:/CSS/picture/picture classification/all together/bad")
  photo<-readImage(pics[i])*0.7
  setwd("D:/CSS/picture/picture classification/aug22/bad")
  writeImage(photo, paste0("aug1",pics[i]))
  # print(photo)
  
  setwd("D:/CSS/picture/picture classification/all together/bad")
  photo<-readImage(pics[i])
  photo<-gblur(photo, sigma=2)
  setwd("D:/CSS/picture/picture classification/aug22/bad")
  writeImage(photo, paste0("aug2",pics[i]))
  
  setwd("D:/CSS/picture/picture classification/all together/bad")
  photo<-readImage(pics[i])*1.5
  setwd("D:/CSS/picture/picture classification/aug22/bad")
  writeImage(photo, paste0("aug3",pics[i]))
  
  setwd("D:/CSS/picture/picture classification/all together/bad")
  # photo<-readImage(pics[i])
  photo<-load.image(pics[i])
  # photo<-flipImage(photo, mode = 'vertical')
  photo<-mirror(photo, "x")
  setwd("D:/CSS/picture/picture classification/aug22/bad")
  save.image(photo,file= paste0("aug4",pics[i]),quality = 0.9)
  # writeImage(photo, paste0("aug4",pics[i]))
  
  # }
}

aug_bad<-sample(list.files(),1000)

file.copy(aug_bad, "D:/CSS/picture/picture classification/all together/bad")


