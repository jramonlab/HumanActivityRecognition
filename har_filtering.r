library(stats)
library(dplyr)

load("rda/har.rda")
har <- har %>% mutate(id = row_number())

# Filtering
s <- har %>% select(id,x1) 
#s <- s %>% filter(id < 100)

n <- 5
wts <- rep(1/n, n)

# avg is calculated at the center of the samples
s.filter1 <- stats::filter(s$x1, filter = wts, sides = 2)
# avg is calculated at the latest sample
s.filter2 <- stats::filter(s$x1, filter = wts, sides = 1)

# Auto-regression
wts <- rep(1/n, nrow(s))
s.filter3 <- stats::filter(s$x1, filter = wts, method = "recursive")


plot(s$x1, type = "p")
lines(s.filter2, col = "red", lwd = 1)
lines(s.filter1, col = "blue", lwd = 1)
lines(s.filter3, col = "green", lwd = 1)



plot(s$x1, type = "p")
lines(s.filter2, col = "red", lwd = 1)

plot(s$x1, type = "p")
lines(s.filter1, col = "blue", lwd = 1)

plot(s$x1, type = "p")
lines(s.filter3, col = "green", lwd = 1)
