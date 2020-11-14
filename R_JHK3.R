#----------------------week3----------------------


lapply

x <- list(a = 1:5, b = rnorm(10)) 
lapply(x, mean)


x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5)) 
lapply(x, mean)


x<-1:4
lapply(x, runif)


x<-1:4
lapply(x, runif, min = 0, max = 10)


x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x

lapply(x, function(elt) elt[,1])


x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5)) 
lapply(x, mean)



sapply(x, mean)
mean(x)



str(mapply)
mapply(rep, 1:4, 4:1)


noise <- function(n, mean, sd) { 
  rnorm(n, mean, sd)
}
noise(5, 1, 2)
noise(1:5, 1:5, 2)


mapply(noise, 1:5, 1:5, 2)
list(noise(1, 1, 2), noise(2, 2, 2), noise(3, 3, 2), noise(4, 4, 2), noise(5, 5, 2))

str(tapply)
x <- c(rnorm(10), runif(10), rnorm(10, 1)) 
f<-gl(3,10)
f

tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE)
$‘1‘
$‘2‘
$‘3‘



tapply(x, f, range)
$‘1‘
$‘2‘
$‘3‘



str(split)
x <- c(rnorm(10), runif(10), rnorm(10, 1)) 
f<-gl(3,10)
split(x, f)
$‘1‘
$‘2‘
$‘3‘



lapply(split(x, f), mean)
$‘1‘
$‘2‘
$‘3‘



library(datasets)
head(airquality)
#Ozone Solar.R Wind Temp Month Day



s <- split(airquality, airquality$Month)
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
$‘5‘
$‘6‘
$‘7‘



sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))



x <- rnorm(10) 
f1<-gl(2,5) 
f2<-gl(5,2)
f1
f2
interaction(f1, f2)


str(split(x, list(f1, f2)))
str(split(x, list(f1, f2), drop = TRUE))


printmessage <- function(x) { 
  if(x > 0)
    print("x is greater than zero")
  else
    print("x is less than or equal to zero") 
  invisible(x)
}




printmessage <- function(x) { 
  if(x>0)
    print("x is greater than zero") 
  else 
    print("x is less than or equal to zero") invisible(x)
} 
printmessage(1)


mean(x)
traceback()


















