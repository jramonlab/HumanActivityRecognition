
wts <- rep(1/7, 7)
#######################################################
#
# Plots the raw signal from ensors Versus time
#
#######################################################
#
library(ggplot2)
library(dplyr)

load("rda/har.rda")

har <- har %>% mutate(id = row_number())

har_filtered <- with(har, ksmooth(id, x1, kernel = "box", bandwidth = 5))
#har_filtered <- with(har, ksmooth(id, x1, kernel = "normal", bandwidth = 5))

har %>% select(x1, class, id) %>% 
  ggplot(aes(x = id, y = x1, color = class)) +
  geom_point(size = 0.2) + 
  geom_line(aes(x = id, y = har_filtered$y), color = "black")


# showing x1 vs Time
har %>% select(x1, class, id) %>% 
  ggplot(aes(x = id, y = x1, color = class)) +
  geom_point(size = 0.2)

ggsave("figs/x1.png", width = 15, height = 5)


# showing Sensor_1 Vs Time
sensor_1_waist <- data.frame(har$id, har$x1, har$y1, har$z1, har$class) %>%
  setNames(c("id", "x1", "y1", "z1", "class"))

sensor_1_waist %>% ggplot(aes(x = id, y = value, color = variable)) + 
  geom_point(aes(y = x1, col = "x1"), size = 0.1) + 
  geom_point(aes(y = y1, col = "y1"), size = 0.1) + 
  geom_point(aes(y = z1, col = "z1"), size = 0.1) + 
  facet_grid(class ~ .) + 
  xlab("Unit Time")+ 
  ggtitle("Sensor 1 (Waist) Vs Time")

ggsave("figs/S1-waist.png", width = 5, height = 3)


# showing Sensor_2 Vs Time
sensor_2 <- data.frame(har$id, har$x2, har$y2, har$z2, har$class) %>%
  setNames(c("id", "x", "y", "z", "class"))

sensor_2 %>% ggplot(aes(x = id, y = value, color = variable)) + 
  geom_point(aes(y = x, col = "x"), size = 0.1) + 
  geom_point(aes(y = y, col = "y"), size = 0.1) + 
  geom_point(aes(y = z, col = "z"), size = 0.1) + 
  facet_grid(class ~ .) + 
  xlab("Unit Time") + 
  ggtitle("Sensor 2 (Left Thigh) Vs Time")

ggsave("figs/S2-left-thigh.png", width = 5, height = 5)

# showing Sensor_3 Vs Time
sensor_3 <- data.frame(har$id, har$x3, har$y3, har$z3, har$class) %>%
  setNames(c("id", "x", "y", "z", "class"))

sensor_3 %>% ggplot(aes(x = id, y = value, color = variable)) + 
  geom_point(aes(y = x, col = "x"), size = 0.1) + 
  geom_point(aes(y = y, col = "y"), size = 0.1) + 
  geom_point(aes(y = z, col = "z"), size = 0.1) + 
  facet_grid(class ~ .) + 
  xlab("Unit Time") + 
  ggtitle("Sensor 3 (Right Ankle) Vs Time")

ggsave("figs/S3-right-ankle.png", width = 5, height = 5)


# showing Sensor_3 Vs Time
sensor_4 <- data.frame(har$id, har$x4, har$y4, har$z4, har$class) %>%
  setNames(c("id", "x", "y", "z", "class"))

sensor_4 %>% ggplot(aes(x = id, y = value, color = variable)) + 
  geom_point(aes(y = x, col = "x"), size = 0.1) + 
  geom_point(aes(y = y, col = "y"), size = 0.1) + 
  geom_point(aes(y = z, col = "z"), size = 0.1) + 
  facet_grid(class ~ .) + 
  xlab("Unit Time") + 
  ggtitle("Sensor 4 (Right Upper Arm) Vs Time")

ggsave("figs/S4-right-upper-arm.png", width = 5, height = 5)

# Revisar :
span <- 10
fit <- with(har, ksmooth(id, x1, kernel = "box", bandwidth = span))


