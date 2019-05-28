library(keras)
# library(h5)
library(reticulate)
# library(h5py)
library(tensorflow)
install.packages("tensorflow", gpu=T)
py_install("h5py")
py_install("scipy")
py_install("keras")
library(OpenImageR)
library(EBImage)
library(imager)
library(magick)
library(dplyr)

install_tensorflow(method = "conda")
devtools::install_github("rstudio/tensorflow")
library(devtools)
install.packages("devtools")
devtools::install_github("rstudio/tensorflow")

install_keras()
install_keras(tensorflow = "gpu")

install_tensorflow()
install_tensorflow(version = "gpu")

devtools::install_github("rstudio/tensorflow")

library(tensorflow)
library(keras)
library(OpenImageR)
library(EBImage)
library(imager)
library(magick)
library(dplyr)
library(data.table)
library(lubridate)

h<-250
w<-250
b_sz<-32

conv_base <- 
    application_resnet50(
 # application_vgg16(
# application_inception_v3(
  # application_xception(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(h,w, 3)
  )



#library(keras)
#install_keras()

#use_condaenv("r-tensorflow")


model <- keras_model_sequential() %>% 
   # conv_base %>%
   # layer_flatten() %>%
   # layer_dense(units = 256, activation = "relu") %>%
   # layer_dense(units = 3, activation = "sigmoid")
  
  #1
  conv_base%>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 3, activation = "softmax")

  
# #2
# conv_base%>%
# layer_flatten() %>%
# # layer_global_average_pooling_2d() %>%
# layer_dense(units = 512, activation = 'relu') %>%
# layer_dropout(rate = 0.5) %>%
# layer_dense(units = 256, activation = 'relu')%>%
# layer_dropout(rate = 0.4) %>%
# layer_dense(units = 128, activation = "relu") %>%
# layer_dropout(rate = 0.3) %>%
# layer_dense(units = 3, activation = "softmax")

  
# 3
# conv_base%>%
#   layer_flatten() %>%
#   layer_global_average_pooling_2d() %>%
#   layer_dropout(rate = 0.4) %>%
#   layer_dense(units = 1024, activation = 'relu') %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 200, activation = 'softmax')

# 4
# layer_conv_2d(filter=32,kernel_size=c(3,3),padding="same",input_shape=c(h,w,3) ) %>%  
#   layer_activation("relu") %>%  
#   layer_conv_2d(filter=32 ,kernel_size=c(3,3))  %>%  
#   layer_activation("relu") %>%
#   #Defining a Pooling layer which reduces the dimentions of the #features map and reduces the computational complexity of the model
#   layer_max_pooling_2d(pool_size=c(2,2)) %>%  
#   #dropout layer to avoid overfitting
#   layer_dropout(0.25) %>%
#   layer_conv_2d(filter=32 , kernel_size=c(3,3),padding="same") %>% layer_activation("relu") %>%  
#   layer_conv_2d(filter=32,kernel_size=c(3,3) ) %>%  
#   layer_activation("relu") %>%  
#   layer_max_pooling_2d(pool_size=c(2,2)) %>%  
#   layer_dropout(0.25) %>%
#   #flatten the input  
#   layer_flatten() %>%  
#   layer_dense(512) %>%  
#   layer_activation("relu") %>%  
#   layer_dropout(0.5) %>%  
#   #output layer-10 classes-10 units  
#   layer_dense(10) %>%  
#   #applying softmax nonlinear activation function to the output layer #to calculate cross-entropy  
#   layer_activation("softmax") 



freeze_weights(conv_base)

# this should only be 4 cz only the top 4 can now be trained.
length(model$trainable_weights)

summary(model)


train_datagen = image_data_generator(
  rescale = 1/255,
     # rotation_range = 10,
    # width_shift_range = 0.2,
    # height_shift_range = 0.2,
     shear_range = 0.2,
    # channel_shift_range = 0.5, 
  #  zoom_range = 0.2,
     horizontal_flip = TRUE,
  fill_mode = "nearest",
   zca_whitening=TRUE
  # brightness_range = c(0.5,1.5)
)



train_dir<-"D:/CSS/picture/picture classification/all together"
valid_dir<-"D:/CSS/picture/picture classification/AssetImages/praveen/validation"

valid_datagen <- image_data_generator(rescale = 1/255
                                      )  

train_generator <- flow_images_from_directory(
  train_dir,                  # Target directory  
  train_datagen,              # Data generator
  target_size = c(h, w),  # Resizes all images to 250 × 250
  classes = c("good","bad","worst"),
  batch_size = b_sz,
  class_mode = "binary"       # binary_crossentropy loss for binary labels
)


valid_generator <- flow_images_from_directory(
  valid_dir,                  # Target directory  
  valid_datagen,               # Data generator
  target_size = c(h,w),  # Resizes all images to 250 × 250
  classes = c("good","bad","worst"),
  batch_size = b_sz,
  class_mode = "binary"       # binary_crossentropy loss for binary labels
)


model %>% compile(
  loss = "sparse_categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 2e-4),
  metrics = c("accuracy")
)


start_time<-Sys.time()

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = train_generator$n/b_sz,
  epochs = 30,
   validation_data = valid_generator,
  # validation_split = 0.1,
   validation_steps = 50,
  callbacks = list(
    # save best model after every epoch
    callback_model_checkpoint("D:/CSS/picture/r/models/asset_pics_model2.h5", save_best_only = TRUE)
  )
)

end_time<-Sys.time()

round(end_time-start_time,2)



setwd("D:/CSS/picture/r/models")
save_model_hdf5(model,"D:/CSS/picture/r/models/asset_pics_model2.h5")
model<-("D:/CSS/picture/r/models/asset_pics_model2.h5")




    ##################################
    ##                              ##
    ##   fine tuning by unfreeze    ##
    ##                              ##
    ##################################


# unfreeze_weights(conv_base, from = 300)
# freeze_weights(conv_base)
unfreeze_weights(conv_base, from = "res4c_branch2c")
length(model$trainable_weights)


model %>% compile(
  loss = "sparse_categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-5),
  metrics = c("accuracy")
)


history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = train_generator$n/32,
  epochs = 50,
  validation_data = valid_generator,
  validation_steps = 50,
  callbacks = list(
    # save best model after every epoch
    callback_model_checkpoint("D:/CSS/picture/r/models/asset_pics_model_fine_tune.h5", save_best_only = TRUE)
  )
)

setwd("D:/CSS/picture/r/models")
model %>% save_model_hdf5("my_model.h5")



    ########################################
    ##                                    ##
    ##       NR and cooler photos model   ##
    ##                                    ##
    ########################################       



model2 <- keras_model_sequential() %>% 
  # conv_base %>%
  # layer_flatten() %>%
  # layer_dense(units = 256, activation = "relu") %>%
  # layer_dense(units = 3, activation = "sigmoid")
  
  #1
  conv_base%>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 3, activation = "softmax")




freeze_weights(conv_base)

# this should only be 4 cz only the top 4 can now be trained.
length(model$trainable_weights)

summary(model2)


train_datagen = image_data_generator(
  rescale = 1/255,
  # rotation_range = 10,
  # width_shift_range = 0.2,
  # height_shift_range = 0.2,
  # shear_range = 0.2,
  # channel_shift_range = 0.5, 
  #  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest",
  zca_whitening=TRUE
  # brightness_range = c(0.5,1.5)
)



train_dir<-"D:/CSS/picture/picture classification/all together/division/train"
valid_dir<-"D:/CSS/picture/picture classification/all together/division/valid"

valid_datagen <- image_data_generator(rescale = 1/255)
  

train_generator <- flow_images_from_directory(
  train_dir,                  # Target directory  
  train_datagen,              # Data generator
  target_size = c(h, w),  # Resizes all images to 250 × 250
  classes = c("cooler","nr"),
  batch_size = 32,
  class_mode = "binary"       # binary_crossentropy loss for binary labels
)


valid_generator <- flow_images_from_directory(
  valid_dir,                  # Target directory  
  valid_datagen,               # Data generator
  target_size = c(h,w),  # Resizes all images to 250 × 250
  classes = c("cooler","nr"),
  batch_size = 32,
  class_mode = "binary"       # binary_crossentropy loss for binary labels
)


model2 %>% compile(
  loss = "sparse_categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 2e-5),
  metrics = c("accuracy")
)


start_time<-Sys.time()

history <- model2 %>% fit_generator(
  train_generator,
  steps_per_epoch = train_generator$n/32,
  epochs = 30,
  validation_data = valid_generator,
  # validation_split = 0.1,
  validation_steps = 50,
  callbacks = list(
    # save best model after every epoch
    callback_model_checkpoint("D:/CSS/picture/r/models/nr_and_cooler.h5", save_best_only = TRUE)
  )
)

end_time<-Sys.time()

round(end_time-start_time,2)




# fine tune nr and cooler

unfreeze_weights(conv_base, from = "res4c_branch2c")
length(model$trainable_weights)


model2 %>% compile(
  loss = "sparse_categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-5),
  metrics = c("accuracy")
)


history <- model2 %>% fit_generator(
  train_generator,
  steps_per_epoch = train_generator$n/32,
  epochs = 20,
  validation_data = valid_generator,
  validation_steps = 10,
  callbacks = list(
    # save best model after every epoch
    callback_model_checkpoint("D:/CSS/picture/r/models/nr_and_cooler_fine_tune.h5", save_best_only = TRUE)
  )
)










testdir_nr<-"D:/CSS/picture/picture classification/all together/division/test"

# testdir_nr<-"D:/CSS/picture/picture classification/all together/division/valid"


test_datagen <- image_data_generator(rescale = 1/255)
# test_datagen <- image_data_generator()


test_generator2 <- flow_images_from_directory(
  testdir_nr,                  # Target directory  
  test_datagen,               # Data generator
  target_size = c(250, 250),  # Resizes all images to 250 × 250
  classes = c("cooler","nr"),
  batch_size = 32,
  class_mode = "binary",       # binary_crossentropy loss for binary labels
  shuffle = FALSE
)


predictions2<-predict_generator(model2, test_generator2, steps=test_generator2$n/32)

p2<-data.table(predictions2)

sapply(p2, function(x)sum(x))


setwd("D:/CSS/picture/picture classification/all together/division/test/cooler")

files2<-data.frame(list.files())

# predict_dataset_cooler<-data.table(files2$list.files..,p2$V1,p2$V2)

predict_dataset_cooler<-data.table(p2$V1,p2$V2)


predict_dataset_cooler$class<-ifelse(predict_dataset_cooler$V1>predict_dataset_cooler$V2,"cooler","nr")

setwd("D:/CSS/picture/reports")

c<-data.table(test_generator2$filenames,predict_dataset_cooler$class)

c$name<-substr(c$V1,8,37)


c$photo_number<-substr(c$V1,33,33)

c$year<-substr(c$V1,14,17)

c$month<-substr(c$V1,18,19)

c$date<-substr(c$V1,20,21)

c$time<-paste0(substr(c$V1,22,23),":",substr(c$V1,24,25))

c$dms<-substr(c$V1,8,12)




write.csv(c,"report_cooler_5-26_v2_validation.csv")

## you should extract the 



coolers<-c%>%
  filter(V2=="cooler")

setwd("D:/CSS/picture/picture classification/all together/division/test/cooler")

file.copy(coolers$name, "D:/CSS/picture/picture classification/test 1/bad")








######  



setwd("D:/CSS/picture/r/models")
new_model <- load_model_hdf5("asset_pics_model2.h5")
testdir<-"D:/CSS/picture/picture classification/AssetImages/praveen/validation"
testdir<-"D:/CSS/picture/picture classification/test 1"



test_datagen <- image_data_generator(rescale = 1/255)
# test_datagen <- image_data_generator()


test_generator <- flow_images_from_directory(
  testdir,                  # Target directory  
  test_datagen,               # Data generator
  target_size = c(250, 250),  # Resizes all images to 250 × 250
   classes = c("good","bad","worst"),
  batch_size = 32,
  shuffle = FALSE
  # class_mode = "binary"       # binary_crossentropy loss for binary labels
)


predictions<-predict_generator(model, test_generator, steps=test_generator$n/32)

p<-data.table(predictions)

sapply(p, function(x)sum(x))



##


setwd("D:/CSS/picture/picture classification/test 1/bad")
files<-data.frame(list.files())

predict_dataset<-data.table(files$list.files..,p$V1,p$V2,p$V3)

predict_dataset$class<-ifelse(p$V1>p$V2 & p$V1>p$V2,"good",ifelse(p$V2>p$V1 & p$V2>p$V3,"bad","worst"))

predict_dataset$dms<-substr(predict_dataset$V1,1,5)

predict_dataset$year<-substr(predict_dataset$V1,7,10)

predict_dataset$month<-substr(predict_dataset$V1,11,12)

predict_dataset$date<-substr(predict_dataset$V1,13,14)

predict_dataset2<-predict_dataset%>%select(V1,dms,year,month,date,class)

names(predict_dataset2)<-c("photo name","dms","year","month","date","class")


setwd("D:/CSS/picture/reports")

write.csv(predict_dataset2,"report_5-23-2019.csv",row.names = F)






##### Image augmentation  ###

library(OpenImageR)
library(EBImage)
library(imager)
library(magick)
library(lime)




setwd("D:/CSS/picture/picture classification/all together/worst")

pics<-list.files()



for (i in 1: length(pics)){
  
  
  setwd("D:/CSS/picture/picture classification/all together/worst")
  photo<-readImage(pics[i])*0.4
  setwd("D:/CSS/picture/picture classification/augmented/worst")
  writeImage(photo, paste0("aug1",pics[i]))
  # print(photo)
  
  setwd("D:/CSS/picture/picture classification/all together/worst")
  photo<-readImage(pics[i])
  photo<-gblur(photo, sigma=2)
  setwd("D:/CSS/picture/picture classification/augmented/worst")
  writeImage(photo, paste0("aug2",pics[i]))
  
  setwd("D:/CSS/picture/picture classification/all together/worst")
  photo<-readImage(pics[i])*1.5
  setwd("D:/CSS/picture/picture classification/augmented/worst")
  writeImage(photo, paste0("aug3",pics[i]))
  
  setwd("D:/CSS/picture/picture classification/all together/worst")
  photo<-readImage(pics[i])
  photo<-image_flop(photo)
  setwd("D:/CSS/picture/picture classification/augmented/worst")
  writeImage(photo, paste0("aug4",pics[i]))
  
}


aug_worst<-sample(list.files(),1200)

file.copy(aug_worst, "D:/CSS/picture/picture classification/all together/worst")


setwd("D:/CSS/picture/picture classification/all together/good")

pics<-list.files()


for (i in 1: length(pics)){
  

    
    setwd("D:/CSS/picture/picture classification/all together/good")
    photo<-readImage(pics[i])*0.4
    setwd("D:/CSS/picture/picture classification/augmented/good")
    writeImage(photo, paste0("aug1",pics[i]))
    # print(photo)
    
    setwd("D:/CSS/picture/picture classification/all together/good")
    photo<-readImage(pics[i])
    photo<-gblur(photo, sigma=2)
    setwd("D:/CSS/picture/picture classification/augmented/good")
    writeImage(photo, paste0("aug2",pics[i]))
    
    setwd("D:/CSS/picture/picture classification/all together/good")
    photo<-readImage(pics[i])*1.5
    setwd("D:/CSS/picture/picture classification/augmented/good")
    writeImage(photo, paste0("aug3",pics[i]))
    
    # setwd("D:/CSS/picture/picture classification/all together/good")
    # photo<-readImage(pics[i])
    # photo<-flipImage(photo,"vertical")
    # setwd("D:/CSS/picture/picture classification/augmented/good")
    # writeImage(photo, paste0("aug4",pics[i]))
    

}

aug_good<-sample(list.files(),900)

file.copy(aug_good, "D:/CSS/picture/picture classification/all together/good")







setwd("D:/CSS/picture/picture classification/all together/bad")

pics<-list.files()


for (i in 1: length(pics)){
  
  # for (j in 1: length(y)){
    
    setwd("D:/CSS/picture/picture classification/all together/bad")
    photo<-readImage(pics[i])*0.4
    setwd("D:/CSS/picture/picture classification/augmented/bad")
    writeImage(photo, paste0("aug1",pics[i]))
    # print(photo)
    
    setwd("D:/CSS/picture/picture classification/all together/bad")
    photo<-readImage(pics[i])
    photo<-gblur(photo, sigma=2)
    setwd("D:/CSS/picture/picture classification/augmented/bad")
    writeImage(photo, paste0("aug2",pics[i]))
    
    setwd("D:/CSS/picture/picture classification/all together/bad")
    photo<-readImage(pics[i])*1.5
    setwd("D:/CSS/picture/picture classification/augmented/bad")
    writeImage(photo, paste0("aug3",pics[i]))
    
    # setwd("D:/CSS/picture/picture classification/all together/bad")
    # photo<-readImage(pics[i])
    # photo<-flipImage(photo,"horizontal")
    # setwd("D:/CSS/picture/picture classification/augmented/bad")
    # writeImage(photo, paste0("aug4",pics[i]))
    
  # }
}


aug_bad<-sample(list.files(),1000)

file.copy(aug_bad, "D:/CSS/picture/picture classification/all together/bad")






library(OpenImageR)
library(EBImage)
library(imager)
library(magick)
library(lime)

setwd("D:/CSS/picture/r/models")

new_model <- load_model_hdf5("asset_pics_model.h5")

new_model %>% predict_proba(valid_generator)


setwd("D:/CSS/picture/picture classification/test")

testdir<-"D:/CSS/picture/picture classification/test"


test_ids <- data.frame(id = list.files(testdir)) %>% as_tibble()



test<-list()

pics<-list.files()



# for(i in 1:206){
#   test[[i]]<-readImage(pics[[i]])
# } 



for(i in 1:206){
  test[[i]]<-image_load(pics[[i]], target_size = c(250,250))
  } 



test_pics <- flow_images_from_directory(
  testdir,                  # Target directory  
  target_size = c(250, 250)  # Resizes all images to 250 × 250
)






c<-unlist(test)





new_model %>% predict(test_pics)

new_model %>% predict_classes(c[1])


setwd("D:/CSS/picture/r")

save(model, file = "CNNmodel.RData")








predictions <-  predict_classes(model, test_generator)
probabilities <- predict_proba(model, test_array)





library(keras)
library(tidyverse)
library(EBImage)
library(tidyr)
library(future)
library(furrr)
library(caret)


get_images <- function(data, path_, w, h){
  imgs <- data %>% select(id) %>% 
    mutate(ImageData = future_map(id, ~preprocess_image(.,path_ = path_, w = w, h = h))) %>%
    select(ImageData)
  return(list2tensor(imgs$ImageData))
}




if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")



x_test <- get_images(data = test_ids, path_ = testdir, w = 250, h = 250)



