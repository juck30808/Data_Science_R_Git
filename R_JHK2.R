#----------------------week2----------------------



if(x>3){ 
  y<-10
}else{ 
  y<-0
}

y<-if(x>3){ 
  10
}else{ 
  0
}

for(i in 1:10){
  print(i)
}


x <- c("a", "b", "c", "d") 
for(i in 1:4) {
  print(x[i])
}
for(i in seq_along(x)) { 
  print(x[i])
}
for(letter in x) { 
  print(letter)
}
for(i in 1:4) 
  print(x[i])


x <- matrix(1:6, 2, 3)
for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i,j])
  }
}


count <- 0 
while(count < 10) {
  print(count) 
  count <- count + 1
}


z<-5
while(z>=3&&z<=10){ 
  print(z)
  coin <- rbinom(1, 1, 0.5)
  if(coin == 1) { ## random walk 
    z<-z+1
  }else{ 
    z<-z-1
  } 
}

x0<-1 
tol <- 1e-8
repeat {
  x1 <- computeEstimate()
  if(abs(x1 - x0) < tol) { 
    break
  }else{ 
    x0<-x1
  } 
}

for(i in 1:100) { 
  if(i <= 20) {
  ## Skip the first 20 iterations
  next
  }
  ## Do something here
}


f <- function(<arguments>) {
  ## Do something interesting
}


mydata <- rnorm(100)
sd(mydata)
sd(x = mydata)
sd(x = mydata, na.rm = FALSE)
sd(na.rm = FALSE, x = mydata)
sd(na.rm = FALSE, mydata)

args(lm)
lm(data = mydata, y ~ x, model = FALSE, 1:100) 
lm(y ~ x, mydata, 1:100, model = FALSE)



f<-function(a,b=1,c=2,d=NULL){ 
  }


#Arguments to functions are evaluated lazily, so they are evaluated only as needed.
f <- function(a, b) { 
  a^2
} 
f(2)


args(paste)
paste("a", "b", sep = ":")
paste("a", "b", se = ":")


#Lexical Scoping

make.power <- function(n) { 
  pow <- function(x) {
    x^n 
  }
  pow 
}


> cube <- make.power(3)
square <- make.power(2)
cube(3)
square(3)



ls(environment(cube))
get("n", environment(cube))
ls(environment(square))
get("n", environment(square))



y<-10
f <- function(x) { 
  y<-2
  y^2 + g(x)
}
g <- function(x) { 
  x*y
}
f(3)



> g(2)
Error in g(2) : object "y" not found >y<-3
> g(2)



make.NegLogLik <- function(data, fixed=c(FALSE,FALSE)) { 
  params <- fixed
  function(p) { 
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -0.5*length(data)*log(2*pi*sigma^2) 
    b <- -0.5*sum((data-mu)^2) / (sigma^2) -(a+b)
  }
}


set.seed(1); normals <- rnorm(100, 1, 2)
nLL <- make.NegLogLik(normals)
nLL


# Estimating Parameters

optim(c(mu = 0, sigma = 1), nLL)$par
nLL <- make.NegLogLik(normals, c(FALSE, 2))
optimize(nLL, c(-1, 3))$minimum
nLL <- make.NegLogLik(normals, c(1, FALSE))
optimize(nLL, c(1e-6, 10))$minimum



nLL <- make.NegLogLik(normals, c(1, FALSE)) x <- seq(1.7, 1.9, len = 100)
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type = "l")
nLL <- make.NegLogLik(normals, c(FALSE, 2)) x <- seq(0.5, 1.5, len = 100)
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type = "l")


#Times in R


x <- Sys.time()
x
## [1] "2013-01-24 22:04:14 EST"
p <- as.POSIXlt(x)
names(unclass(p))
## [1] "sec" "min" "hour" "mday" "mon" ## [6] "year" "wday" "yday" "isdst" p$sec
## [1] 14.34


x <- Sys.time()
x ## Already in ‘POSIXct’ format
## [1] "2013-01-24 22:04:14 EST"
unclass(x)
## [1] 1359083054
x$sec
## Error: $ operator is invalid for atomic vectors p <- as.POSIXlt(x)
p$sec
## [1] 14.37


datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10") 
x <- strptime(datestring, "%B %d, %Y %H:%M")
x

class(x)

x <- as.Date("2012-01-01")
y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S") x-y
## Warning: Incompatible methods ("-.Date",
## "-.POSIXt") for "-"
## Error: non-numeric argument to binary operator
x <- as.POSIXlt(x)
x-y
## Time difference of 356.3 days

x <- as.Date("2012-03-01") y <- as.Date("2012-02-28") x-y
## Time difference of 2 days
x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz = "GMT") y-x
## Time difference of 1 hours






