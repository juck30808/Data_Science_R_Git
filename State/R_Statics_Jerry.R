## == ch1 ==

#iv.Input & Output
getwd()
setwd("/Users/juck30808/Documents/Github/Statistics_Git/")
getwd()
dir()

#use femaleMiceWeights.csv
read.csv("data/femaleMiceWeights.csv")
read.table("data/femaleMiceWeights.csv")
tempmydata <- read.csv("data/femaleMiceWeights.csv")
iris
data<-iris
write.table(data,file = "output/test.CSV", sep = ",")
write.csv(data,file = "output/test2.CSV")

##v. Install package() Meet Common Package
install.packages("tidyverse")  # system for declaratively creating graphics / include"ggplot2"
install.packages("dslabs")
install.packages("dplyr")
library(ggplot2)
require(ggplot2)

##vi. other basic operation
sleep
View(sleep)
b <- "Two"
ls()       #list all values

#vii. About help
help(pnorm)
?"pnorm"
args(pnorm)
pnorm(1.96,lower.tail = TRUE) 
pnorm(1.96)
pnorm(1.96,lower.tail = FALSE)


## == charapter 2 ==

#ii.Math function
1:10  
abs(-1) 
sqrt(9) 
sum(1:10) 
summary(1:10) #return max,min,mean
sample(c("a","b","c","d"))
cumsum(1:20)  # summery the summery
cumprod(1:5)  # mutiply the mutiply 
log(8,2)
args(log)
log(1200,10)

#iii. statistics function
n <- sample(1:10) 
plot(n, pch = 17, col = "blue", cex =2) 

mean(n)     #	Mean 平均數
median(n)
as.numeric(names(table(n)))[which.max(table(n))]   # mode 眾數
points(mean(n), pch = 4, col = "blue", cex = 3) 
points(median(n), pch = 3, col = "blue", cex = 3) 

sd(n)       # Standard Deviation 標準差（變異數）
var(n)      # Variance 變異數 = sd(x)^2
cv <- 100 * sd(n) / mean(n);cv  #變異係數 
range(n)[2] - range(n)[1]       #全距(最大值減最小值) 

Q1 <- quantile(n, 1 / 4) ;Q1 # 四分位距
Q2 <- quantile(n, 2 / 4) ;Q2
Q3 <- quantile(n, 3 / 4) ;Q3
b <- Q3 - Q1 == IQR(n)   ;IQR  #IQR = Q3-Q1 

#iv.string function
tolower(c("US","Taiwan"))   #lower
toupper(c("us","taiwan"))   #upper
#casefold(c("US","Taiwan"),upper=FALSE)

#v.seq() ?"seq"
1:10
seq(1,10)
help("seq")
seq(1,10,by=2)             
seq(length=10,from=1,by=2) 
seq(1,10,length.out = 5) 


## == Ch1. Continuous Probability Distributions 連續分配 ==

#(1).Uniform dis

#(2).Normal dis(z)
u = 0.26;sd = 0.05; x = 0.35
z = (x-u)/sd
pnorm(z)       # less than 0.96
1-pnorm(z)     # more than 0.35

#ex
u <- 64;sd <- 2.5 ; x1 <- 60.3;x2 <- 65
z1 <- (x1-u)/sd 
z2 <- (x2-u)/sd
pnorm(z2)-pnorm(z1)   #between 60.3 and 65

# (3).Sampling dis

#ex1 morn than 32
u<-32.2;sd<-0.3;x<-32
z<-(x-u)/sd
1-pnorm(z)

#ex2 have four bottles,and greater than 32
u<-32.2;sd<-0.3;x<-32
z<-(x-u)/(sd/sqrt(4))
1-pnorm(z)

#ex3 take sample 25, and 105 greater
u<-100; sd<-15; x<-105
z<-(x-u)/(sd/sqrt(25))
1-pnorm(z)

#ex4 random sample 100 , assumed = 0.6 , less than 0.56
u<-0.6;x<-0.56
z<-(x-u)/(sqrt( (0.6*(1-0.6)) /100) )
pnorm(z)

#ex5 p=0.4 ,n=100, less than 0.32
u<-0.4;x<-0.32
z<-(x-u)/(sqrt( (0.4*(1-0.4))/100))
pnorm(z)


# == Ch2.Hypothesis test - I (THS) 假設檢定

#Hypothesis test - I
#H0:u<=1.5  / #H1:u>1.5 / #Less than 1.5
pmean <-1.5 ;xbar <-1.1;n <-25;sd <- 0.5;sig <-0.05
z <-(xbar-pmean)/(sd/sqrt(n));z
pvalue <-pnorm(z); pvalue
pvalue<sig            #TRUE Pvalue
cv <- qnorm(sig);cv
z<cv                  #TRUE Z-value  #Reject H0

#Hypothesis test - I Problem 
#H0:u>=3.5  / #H1:u<3.5 / #Less than 3.5
pmean <-3.5;xbar <-2;n <-25;sd <- 0.6;sig <-0.05
z <-(xbar-pmean)/(sd/sqrt(n));z
pvalue <-pnorm(z);pvalue
cv <- qnorm(sig);cv
pvalue<sig            #TRUE Pvalue
z<cv                  #TRUE Z-Value  #Reject H0


#Hypothesis test - II
#H0:u<=1.5   /  #H1:u>1.5 / #Larger(greater) than 1.5
pmean <-1.5;xbar <- 1.9;n <-25;sd <- 0.5;sig <-0.05
z <-(xbar-pmean)/(sd/sqrt(n));z
pvalue <-pnorm(z); pvalue
cv <- qnorm(sig) ; cv
pvalue<sig             #TRUE Pvalue
z<cv                   #TRUE Z-Value  #Non Reject H0

# == Ch3.Hypothesis test - II (THS) 假設檢定
#H0:u<=50  / #H1:u>50 / #Larger(greater) than 50
pmean <-50;xbar <- 60;n <-36;sd <- 10;sig <-0.05
z <-(xbar-pmean)/(sd/sqrt(n)) ;z
cv <- qnorm(1-sig) ;cv
z>cv                   #TRUE Z-Value  #Non Reject H0


#Two-Tailed Test Definition
#H0:u=250  / #H1:u!=250
xbar <-246;pmean <- 250;sd <-5;n <-16
z <-(xbar-pmean)/(sd/sqrt(n)) ;z
CVL <-qnorm(0.05/2);CVL
CVU <-qnorm(1-(0.05/2));CVU
(z<CVL) | (z>CVU)             # |=or
Pvalue <-2*pnorm(z);Pvalue
Pvalue <alpha


#Right tail
#H0:u<=50 H1:u>50
n <-36 ;xbar <-60 ;pmean <-50;sd <-10 ;sig <-0.05
z<-(xbar-pmean)/(sd/sqrt(n));z
CV <-qnorm(1-0.05);CV         #right tail
Pvalue <- pnorm(z,lower.tail = FALSE); Pvalue
Pvalue < sig
z > CV


#Two tail
xbar <-0.495;pmean <- 0.5;sd <-0.05;n <-16
z <-(xbar-pmean)/(sd/sqrt(n));z
CVL <-qnorm(0.05/2); CVL
CVU <-qnorm(1-(0.05/2));CVU
(z<CVL) | (z>CVU) # |=or
Pvalue <-2*pnorm(z,lower.tail = TRUE);Pvalue
Pvalue <alpha


# == Ch4.Hypothesis test - III (THS) 假設檢定

# == select 10 student ==
score <- c(80,75,60,72,55,89,95,78,82,90)
xbar<-mean(score)
n<-length(score)
ssd<-sd(score)
tcv<-qt(0.05/2,df=n-1)
se<-abs(tcv*ssd/sqrt(n))
c(xbar-se,xbar+se)


#ramdom25 aerage0.265 sd0.03 differ0.26 5%
xbar<-0.265;pmean<-0.26;n<-25;sd<-0.03;alpha<-0.05
t <-(xbar-pmean)/(sd/sqrt(n));t
pvalue <-2*pt(t,df=n-1,lower.tail = FALSE)
pvalue <alpha

#now you try to recruit
xbar<-0.29;pmean<-0.26;n<-25;sd<-0.04;alpha<-0.05
t <-(xbar-pmean)/(sd/sqrt(n));t
pvalue <-pt(t,df=n-1,lower.tail = FALSE)
pvalue <alpha

#howerer
xbar<-0.25;pmean<-0.26;n<-25;sd<-0.02;alpha<-0.05
t <-(xbar-pmean)/(sd/sqrt(n));t
pvalue <-pt(t,df=n-1,lower.tail = TRUE)
pvalue <alpha


################################

x<- 32;mu<- 32.2;sigma<- 3
z<-(x-mu)/sigma
1-pnorm((32-32.2)/0.3)

#Test population
sd<-17.386; n<-15; xbar<-24.501;a<-0.05
z <- qnorm(a/2);z
Q<-abs(z*sd/sqrt(n))
ci<- c(xbar-Q,xbar+Q);ci

#Test infer satisfaction
phat <- 0.8655; p <- 0.9; n <- 177
z <-(phat-p)/sqrt((p*(1-p)/n)); z

qnorm(0.05)


#使用summary函數製作次數分配表
summary(mydata)  

#製作被調查者性別的次數分配表，並將次數分配表轉化成百分比表
prop.table(summary(mydata$性別)) 

#計算8名運動員決賽成績的平均數、標準差和變異係數。
me <- colMeans(mydata)  #平均數 #期望值
sd <- apply(mydata, 2, sd, na.rm = TRUE) #標準差
cv <- sd/me #變異係數

#繪製8名運動員10槍決賽成績的盒鬚圖(y軸射擊環數、x軸名字)
boxplot(mydata)

#莖葉圖
stem(mydata$銷售額,scale = 1)

################################