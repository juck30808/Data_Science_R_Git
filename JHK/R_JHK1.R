#----------------------week1----------------------

#R Console Input and Evaluation
x<-1
print(x)
msg<-"hello"


#The c() function can be used to create vectors of obj
x <- 1:20
x <- c(0.5, 0.6)
x <- c(TRUE,FALSE)
x <- c(T,F)
x <- c("a","b","c")
x <- 9:29
x <- c(1+0i, 2+4i)

x <- vector("numeric",length = 10)
x

x<- 0:6
class(x)
as.numeric(x)
as.logical(x)
as.character(x)

x <- c("a","b","c")
as.numeric(x)
as.logical(x)
as.complex(x)


#The List function
x<-list(1,"a",TRUE, 1+4i)
x


m<-matrix(nrow=2,ncol = 3)
m

dim(m)
attributes(m)

m<-matrix(1:6,nrow=2,ncol = 3)
m

m<-1:10
m

dim(m) <- c(2,5)
m

x<-1:3
y<-10:12
cbind(x,y)
rbind(x,y)

#Data Types - Factors

x <- factor(c("yes","yes","no","yes","no"))
x
table(x)
x
unclass(x)


x<- factor(c("yes","yes","no","yes","no"),
           levels = c("yes","no"))
x


x<-c(1,2,NA,10,3)
is.na(x)
is.nan(x)
x<-c(1,2,NaN,NA,4)
is.na(x)
is.na(x)


x <- data.frame(foo = 1:4, bar = c(T,T,F,F))
x

nrow(x)
ncol(x)


x<-1:3
names(x)
names(x) <-c("foo","bar","norf")
x
names(x)

x<-list(a=1,b=2,c=3)
x

m <- matrix(1:4, nrow=2,ncol=2)
dimnames(m) <- list(c("a","b"),c("c","d"))
m


data<-read.table("foo.txt")


read.table
initial <- read.table("datatable.txt", nrows=100)
classes <- sapply(initial, class)
tabAll <- read.table("datatable.txt",colClasses = classes)


#Textual Data Formats
y <- data.frame(a=1,b="a")
dput(y)

dput(y,file="y,R")
new.y <- dget("y,R")
new.y

x<-"foo"
y<-data.frame(a=1,b="a")
dump(c("x","y"),file="data.R")
rm(x,y)
source("data.R")
y
x


str(file)
#open is a code indicating
#- “r”readonly
#- “w”writing(andinitializinganewfile)
#- “a”appending
#- “rb”,“wb”,“ab”reading,writing,orappendinginbinarymode(Windows)

con <- file("foo.txt", "r") 
data <- read.csv(con)
close(con)
#data <- read.csv("foo.txt") same


con <- gzfile("words.gz") 
x <- readLines(con, 10)
x



#Reading Lines of a Text File
con <- url("http://www.jhsph.edu", "r")
x <- readLines(con)
head(x)



x <- c("a", "b", "c", "c", "d", "a") > 
x[1]

x[2]

x[1:4]

x[x>"a"]

u <- x>"a"

x[u]


x <- list(foo = 1:4, bar = 0.6)
x[1]

x$bar
x["bar"]


x <- list(foo = 1:4, bar = 0.6, baz = "hello")
x[c(1, 3)]

x <- list(foo = 1:4, bar = 0.6, baz = "hello")
name <- "foo"
x[[name]] ## computed index for ‘foo’
x$name ## element ‘name’ doesn’t exist!
x$foo



x <- list(aardvark = 1:5)
x$a
x[["a"]]
x[["a", exact = FALSE]]


x<-c(1,2,NA,4,NA,5)
bad <- is.na(x)
x[!bad]


x<-c(1,2,NA,4,NA,5)
y <- c("a", "b", NA, "d", NA, "f")
good <- complete.cases(x, y)
good


x[good]
y[good]


airquality[1:6, ]


good <- complete.cases(airquality)
airquality[good, ][1:6, ]


x<-1:4;y<-6:9
x+y

x>2
x>=2
y==8
x*y
x/y


x <- matrix(1:4, 2, 2); y <- matrix(rep(10, 4), 2, 2)
x * y ## element-wise multiplication
x/y
x %*% y ## true matrix multiplication



#swirl 它的开发者是 Nick Carchedi 他是我们约翰·霍普金斯大学生物统计学院的学生 
#这个系统可以帮助你以互动的方式按自己的节奏来学习 R 它会引导你学习一系列有关 R 语言各个方面的课程 
#同时给你提供练习 与其一如既往地看完视频后去做作业 一步一步地来 不如在 swirl 的引导下 直接在 R 的控制台中进行学习 
#不用什么功能都自己去探索 因此我认为这个 swirl 模块会很有用 我建议大家去试一试

#install.packages("swirl")
#library(swirl)

install_from_swirl("R Programming")
swirl()


