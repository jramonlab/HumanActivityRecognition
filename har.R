# Estimated time for whole script is.... 9 minutes
# On Amd-A8 64 bits 4 cores 3,6 GHz. 8 GB RAM
#
start_time <- Sys.time()

if(!require(readr)) install.packages("readr")
library(readr)

##################################################################
# DOWNLOAD dataset and create data frame
# https://archive.ics.uci.edu/ml/datasets/Wearable+Computing%3A+Classification+of+Body+Postures+and+Movements+%28PUC-Rio%29
########################################

url_dataset <- "http://groupware.les.inf.puc-rio.br/static/har/dataset-har-PUC-Rio-ugulino.zip"
skip_lines <- 0

# Example with few obsrvations
#url_dataset <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00250/example-data.dat"
#skip_lines <- 12

# File tree structure. Set the working directory where the file is
wd <- getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
wd <- getwd()

system("mkdir data")
system("mkdir rda")
system("mkdir figs")
system("mkdir report")

temp_file <- "./data/dataset-har-PUC-Rio-ugulino.zip"
download.file(url_dataset, temp_file)

# list all files names inside of a .zip file
fname <- as.character(unzip(temp_file, list = TRUE)$Name)

har <- read.csv(unz(temp_file, fname), 
                skip = skip_lines, 
                header = TRUE, 
                sep = ";", dec = ",")

# z4 is coerced to factor, but it is an integer
har$z4 <- as.integer(har$z4)

save(har, file = "rda/har.rda")
##################################################################
# EDA
#####
load("rda/har.rda")

if(!require(beepr)) install.packages("beepr")
library(beepr)

if(!require(stringr))install.packages("stringr")
if(!require(dplyr))  install.packages("dplyr")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(tidyr))  install.packages("tidyr")

if(!require(caret))       install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart))       install.packages("rpart")
if(!require(randomForest))install.packages("randomForest")

# Needed for proper decimal digits on tibbles
options(pillar.sigfig = 5, pillar.subtle = FALSE, pillar.bold = TRUE)
# No text truncation on tibble columns
options(tibble.width = Inf)

# Is there any NAs ?
nas <- apply(har, MARGIN = 2, function(x) any(is.na(x) | is.infinite(x)))
if (any(nas) == FALSE) 
{
  cat("Great, no NAs or Infinite value in the dataset")
}else{
  cat("Attention, some NA or Infinite value was found in the dataset")
}

# proportion of classes ?
har %>% group_by(class) %>% summarize(n = n(), p = n()/nrow(.)) %>% arrange(desc(p))

# proportion of users for all class ?
har %>% group_by(user) %>% summarize(n = n(), p = n()/nrow(.)) %>% arrange(desc(p))

# check proportion of classes and users ?
har %>% group_by(class, user) %>% summarize(n = n(), p = n()/nrow(.)) %>% arrange(desc(p))

# Grouping by class letÂ´s observe the sensor values
har %>% group_by(class) %>% summarize(x1_avg = mean(x1),
                                      y1_avg = mean(y1),
                                      z1_avg = mean(z1),
                                      x2_avg = mean(x2),
                                      y2_avg = mean(y2),
                                      z2_avg = mean(z2),
                                      x3_avg = mean(x3),
                                      y3_avg = mean(y3),
                                      z3_avg = mean(z3),
                                      x4_avg = mean(x4),
                                      y4_avg = mean(y4),
                                      z4_avg = mean(z4))



# boxplot by classes
har %>% ggplot(aes(class, x1)) + geom_boxplot()


sensor_1 <- data.frame(har$x1, har$y1, har$z1, har$class) %>% setNames(c("x", "y", "z", "class"))
sensor_2 <- data.frame(har$x2, har$y2, har$z2, har$class) %>% setNames(c("x", "y", "z", "class"))
sensor_3 <- data.frame(har$x3, har$y3, har$z3, har$class) %>% setNames(c("x", "y", "z", "class"))
sensor_4 <- data.frame(har$x4, har$y4, har$z4, har$class) %>% setNames(c("x", "y", "z", "class"))


sensor_1 %>% ggplot(aes(x = class, y = value, color = variable)) + 
  geom_boxplot(aes(y = x, col = "x")) + 
  geom_boxplot(aes(y = y, col = "y")) + 
  geom_boxplot(aes(y = z, col = "z")) + 
  xlab("Classes") + 
  ggtitle("Sensor 1 (Waist) Variability")

ggsave("figs/S1-waist_BOX.png", width = 5, height = 5)

sensor_2 %>% ggplot(aes(x = class, y = value, color = variable)) + 
  geom_boxplot(aes(y = x, col = "x")) + 
  geom_boxplot(aes(y = y, col = "y")) + 
  geom_boxplot(aes(y = z, col = "z")) + 
  xlab("Classes") + 
  ggtitle("Sensor 2 (Left Thigh) Variability")

ggsave("figs/S2-left-thigh_BOX.png", width = 5, height = 5)

sensor_3 %>% ggplot(aes(x = class, y = value, color = variable)) + 
  geom_boxplot(aes(y = x, col = "x")) + 
  geom_boxplot(aes(y = y, col = "y")) + 
  geom_boxplot(aes(y = z, col = "z")) + 
  xlab("Classes") + 
  ggtitle("Sensor 3 (Right Ankle) Variability")

ggsave("figs/S3-right-ankle_BOX.png", width = 5, height = 5)

sensor_4 %>% ggplot(aes(x = class, y = value, color = variable)) + 
  geom_boxplot(aes(y = x, col = "x")) + 
  geom_boxplot(aes(y = y, col = "y")) + 
  geom_boxplot(aes(y = z, col = "z")) + 
  xlab("Classes") + 
  ggtitle("Sensor 4 (Right Upper Arm) Variability")

ggsave("figs/S4-right-upper-arm_BOX.png", width = 5, height = 5)

##################################################################
# data partition for validation and train(10%)
#
set.seed(1)

val_index <- createDataPartition(y = har$class, times = 1, p = 0.1, list = FALSE)
har_val <- har[val_index,]
har_set <- har[-val_index,]

# har_set is splitted now into test and train set
#
test_index <- createDataPartition(y = har_set$class, times = 1, p = 0.1, list = FALSE)
har_set_test <- har_set[test_index,]
har_set_train <- har_set[-test_index,]

# Proportion of classes after splitting
#
har_val %>% group_by(class) %>% summarize(n = n(), p = n()/nrow(.)) %>% arrange(desc(p))
har_set %>% group_by(class) %>% summarize(n = n(), p = n()/nrow(.)) %>% arrange(desc(p))

har_set_test %>% group_by(class) %>% summarize(n = n(), p = n()/nrow(.)) %>% arrange(desc(p))
har_set_train %>% group_by(class) %>% summarize(n = n(), p = n()/nrow(.)) %>% arrange(desc(p))

rm(test_index, val_index)

save(har_set_train, file = "rda/har_set_train.rda")

#
##################################################################
# Modeling (only sensors)
###############?
# CLASSIFICATION TREES
#
fit_part <- train(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4 ,
                  method = "rpart", 
                  tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 10)),
                  data = har_set_train) 

#load("rda/fit_part.rda")

plot(fit_part)
plot(fit_part$finalModel)
text(fit_part$finalModel, cex = 0.3)
beep()

# Plot Z1 Vs Y3
#
har_set_train %>% ggplot(aes(z1, y3, color = class)) +
  geom_point(alpha = 0.3)

ggsave("figs/z1_Vs_y3.png", width = 5, height = 5)

# Predictions
y_hat_part <- predict(fit_part, har_set_test)
acc <- confusionMatrix(y_hat_part, reference = har_set_test$class)$overall["Accuracy"]
acc

save(fit_part, file = "rda/fit_part.rda")

# Full tree
#
# cp = 0
fit <- rpart(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4, 
             data = har_set_train, 
             control = rpart.control(cp = 0))

plot(fit)
text(fit, cex = 0.5)

# Predictions
y_hat_part <- predict(fit, har_set_test, type = "class")
acc <- confusionMatrix(y_hat_part, reference = har_set_test$class)$overall["Accuracy"]
acc

acc_results <- data_frame(METHOD = "Classification tree(rpart)", 
                          TUNING = "CP=0(690 splits)",
                          SENSORS = "1,2,3,4",
                          ACCURACY = acc)


# Pruning the tree
#
fit_part_pruned <- prune(fit, cp = 0.01)
plot(fit_part_pruned, margin = 0)
text(fit_part_pruned, cex = 0.8)
# Predictions
y_hat_part_pruned <- predict(fit_part_pruned, har_set_test, type = "class")
acc_pruned <- confusionMatrix(y_hat_part_pruned, reference = har_set_test$class)$overall["Accuracy"]
acc_pruned

save(fit_part_pruned, file = "rda/fit_part_pruned.rda")

acc_results <- bind_rows(acc_results,
                         data_frame(
                         METHOD = "Classification tree(rpart) pruned", 
                         TUNING = "CP=0.01(10 splits)",
                         SENSORS = "1,2,3,4",
                         ACCURACY = acc_pruned))

###############?
# RANDOM FOREST
#
set.seed(14)
mtry <- seq(1,12,1) # number of variables randomly selected for each tree

fit_forest1 <- randomForest(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4, 
                           data = har_set_train,
                           ntree = 100,
                           do.trace = 10,
                           tuneGrid = data.frame(mtry = mtry))

plot(fit_forest1)# 50 trees
fit_forest1$mtry # mtry = 3
mtry <- fit_forest1$mtry

save(fit_forest1, file = "rda/fit_forest1.rda")

# Train control is needed n order to shorten the run time
control <- trainControl(method="oob", number = 25, p = 0.9)
fit_forest <- train(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4, 
                    method = "rf",
                    data = har_set_train,
                    ntree = 50,
                    do.trace = 10,
                    trControl = control,
                    tuneGrid = data.frame(mtry = mtry))

# Predictions
y_hat_rforest <- predict(fit_forest, har_set_test)
acc_rforest <- confusionMatrix(y_hat_rforest, reference = har_set_test$class)$overall["Accuracy"]
acc_rforest

acc_results <- bind_rows(acc_results,
                         data_frame(
                         METHOD = "Classification tree(Random Forest)", 
                         TUNING = "Trees=50,mtry=3",
                         SENSORS = "1,2,3,4",
                         ACCURACY = acc_rforest))

save(fit_forest, file = "rda/fit_forest.rda")

#################################################################
#Checking variable importance

imp <- varImp(fit_forest)
imp

# Let´s see how it performs with only 2 sensors. 
# We pick the most important ones (1 and 2)
mtry <- seq(1,12,1) # number of variables randomly selected for each tree

fit_forest_2s <- randomForest(class ~ x1 + y1 + z1 + x2 + y2 + z2, 
                            data = har_set_train,
                            ntree = 100,
                            do.trace = 10,
                            tuneGrid = data.frame(mtry = mtry))

# Predictions
y_hat_rforest_2s <- predict(fit_forest_2s, har_set_test)
acc_rforest_2s <- confusionMatrix(y_hat_rforest_2s, reference = har_set_test$class)$overall["Accuracy"]
acc_rforest_2s

save(fit_forest, file = "rda/fit_forest_2s.rda")

acc_results <- bind_rows(acc_results,
                         data_frame(
                           METHOD = "Classification tree(Random Forest)", 
                           TUNING = "Trees=50,mtry=3",
                           SENSORS = "1,2",
                           ACCURACY = acc_rforest_2s))

# Now, We pick the most important ones (1 and 2 and 3)
fit_forest_3s <- randomForest(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3, 
                              data = har_set_train,
                              ntree = 100,
                              do.trace = 10,
                              tuneGrid = data.frame(mtry = mtry))

# Predictions
y_hat_rforest_3s <- predict(fit_forest_3s, har_set_test)
acc_rforest_3s <- confusionMatrix(y_hat_rforest_3s, reference = har_set_test$class)$overall["Accuracy"]
acc_rforest_3s

save(fit_forest, file = "rda/fit_forest_3s.rda")

acc_results <- bind_rows(acc_results,
                         data_frame(
                           METHOD = "Classification tree(Random Forest)", 
                           TUNING = "Trees=50,mtry=3",
                           SENSORS = "1,2,3",
                           ACCURACY = acc_rforest_3s))

###############################################
#
acc_results
save(acc_results, file = "rda/acc_results.rda")
#
###################################################################


##################################################################
# FINAL RESULTS. Prediction on har_val
#
load("rda/fit_part.rda")
load("rda/fit_part_pruned.rda")
load("rda/fit_forest.rda")

# rpart (cp = 0)
#
y_hat_part <- predict(fit_part, har_val)
acc_part <- confusionMatrix(y_hat_part, reference = har_val$class)$overall["Accuracy"]

acc_final_results <- data_frame(METHOD = "Classification tree(rpart)", 
                          TUNING = "CP=0(690 splits)",
                          SENSORS = "1,2,3,4",
                          ACCURACY = acc_part)


# rpart pruned (cp = 0.01)
#
y_hat_part_pruned <- predict(fit_part_pruned, har_val, type = "class")
acc_part_pruned <- confusionMatrix(y_hat_part_pruned, reference = har_val$class)$overall["Accuracy"]

acc_final_results <- bind_rows(acc_final_results,
                               data_frame(
                               METHOD = "Classification tree(rpart) pruned", 
                               TUNING = "CP=0.01(10 splits)",
                               SENSORS = "1,2,3,4",
                               ACCURACY = acc_part_pruned))


# random forest
#
y_hat_rf <- predict(fit_forest, har_val)
acc_rf <- confusionMatrix(y_hat_rf, reference = har_val$class)$overall["Accuracy"]
cm_rf <- confusionMatrix(y_hat_rf, reference = har_val$class)


acc_final_results <- bind_rows(acc_final_results,
                               data_frame(
                               METHOD = "Classification tree(Random Forest)", 
                               TUNING = "Trees=50,mtry=3",
                               SENSORS = "1,2,3,4",
                               ACCURACY = acc_rf))


save(acc_final_results, file = "rda/acc_final_results.rda")
save(cm_rf, file = "rda/cm_rf.rda")

#################################################################
cat("Run time : ", Sys.time() - start_time , " minutes")
beep()

acc_final_results
