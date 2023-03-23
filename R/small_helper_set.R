`%notin%` <- function(x,y) !(x %in% y)
mean.na <- function(x) mean(x, na.rm = T)
median.na <- function(x) stats::median(x, na.rm= T)
min.na <- function(x) min(x, na.rm = T)
max.na <- function(x) max(x, na.rm = T)
sd.na <- function(x) stats::sd(x, na.rm = T)
sum.na <- function(x) sum(x, na.rm = T)

