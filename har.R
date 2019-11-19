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

library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

if(!require(beepr))install.packages("beepr")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(rpart)
library(randomForest)

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


# Grouping by class let´s observe the sensor values
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


# No clear chart
# sensors <- data.frame(har$x1, har$y1, har$z1, 
#                       har$x2, har$y2, har$z2,
#                       har$x3, har$y3, har$z3,
#                       har$x4, har$y4, har$z4,
#                       har$class) %>%
#   setNames(c("x1", "y1", "z1",
#              "x2", "y2", "z2",
#              "x3", "y3", "z3",
#              "x4", "y4", "z4","class"))
# 
# sensors %>% ggplot(aes(x = class, y = value, color = variable)) + 
#   geom_boxplot(aes(y = x1, col = "x1")) + 
#   geom_boxplot(aes(y = y1, col = "y1")) + 
#   geom_boxplot(aes(y = z1, col = "z1")) + 
#   geom_boxplot(aes(y = x2, col = "x2")) + 
#   geom_boxplot(aes(y = y2, col = "y2")) + 
#   geom_boxplot(aes(y = z2, col = "z2")) + 
#   geom_boxplot(aes(y = x3, col = "x3")) + 
#   geom_boxplot(aes(y = y3, col = "y3")) + 
#   geom_boxplot(aes(y = z3, col = "z3")) + 
#   geom_boxplot(aes(y = x4, col = "x4")) + 
#   geom_boxplot(aes(y = y4, col = "y4")) + 
#   geom_boxplot(aes(y = z4, col = "z4")) + 
#   xlab("Classes") + 
#   ggtitle("Sensor 1 (Waist) Variability")

##################################################################
# data partition for validation (10%)
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

#
##################################################################
# Modeling (only sensors)
# CLASSIFICATION TREES
#
fit_part <- train(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4 ,
                  method = "rpart", 
                  tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 10)),
                  data = har_set_train) 

load("rda/fit_part.rda")

plot(fit_part)
plot(fit_part$finalModel)
text(fit_part$finalModel, cex = 0.3)
beep()

# Plot Z1 Vs Y3
#
har_set_train %>% ggplot(aes(z1, y3, color = class)) +
  geom_point(alpha = 0.3)

# Predictions
#
y_hat_part <- predict(fit_part, har_set_test)
acc <- confusionMatrix(y_hat_part, reference = har_set_test$class)$overall["Accuracy"]
acc

save(fit_part, file = "rda/fit_part.rda")

# Full tree
#
# cp = 0
fit <- rpart(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4, 
             data = har_set_train, control = rpart.control(cp = 0))
plot(fit)
text(fit, cex = 0.5)

# Predictions
y_hat_part <- predict(fit_part, har_set_test)
acc <- confusionMatrix(y_hat_part, reference = har_set_test$class)$overall["Accuracy"]
acc

# Pruning the tree
#
fit_pruned <- prune(fit, cp = 0.005)
plot(fit_pruned)
text(fit_pruned, cex = 0.5)
# Predictions
y_hat_part_pruned <- predict(fit_pruned, har_set_test, type = "class")
acc_pruned <- confusionMatrix(y_hat_part_pruned, reference = har_set_test$class)$overall["Accuracy"]
acc_pruned

save(fit_part_pruned, file = "rda/fit_part_pruned.rda")

# RANDOM FOREST
#
set.seed(14)
mtry <- seq(1,12,1) # number of variables randomly selected for each tree

fit_forest <- randomForest(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4, 
                           data = har_set_train,
                           ntree = 50,
                           do.trace = 10,
                           tuneGrid = data.frame(mtry = mtry))
plot(fit_forest)
fit_forest$mtry # mtry = 3

start_time <- Sys.time()

mtry <- seq(2,4,1)
control <- trainControl(method="oob", number = 25, p = 0.9)
fit_forest <- train(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4, 
                    method = "rf",
                    data = har_set_train,
                    ntree = 50,
                    do.trace = 10,
                   # trControl = control,
                    tuneGrid = data.frame(mtry = mtry))

ggplot(fit_forest, highlight = TRUE)
beep()
cat("Run time : ", Sys.time() - start_time , " minutes")

y_hat_rforest <- predict(fit_forest, har_set_test)
acc_rforest <- confusionMatrix(y_hat_rforest, reference = har_set_test$class)$overall["Accuracy"]
acc_rforest

save(fit_forest, file = "rda/fit_forest.rda")

# RBORIST
#
train_rf_2 <- train(class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 20)),
                    ntree = 50,
                    do.trace = TRUE,
                   data = har_set_train)

# It takes > 30 min. to run
beep()
confusionMatrix(predict(train_rf_2, har_set_test),har_set_test$class)$overall["Accuracy"]
save(train_rf_2, file = "rda/fit_rborist.rda")



library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)

ggplot(train_rf)
train_rf$bestTune



##################################################################



