# == CH1. Inferential statistics 敘述統計 ==

# == READ the excel data ==
library(readxl)
x = read_excel("data/Xr12-23.xlsx")
mydata<-data.frame(x);mydata
str(mydata)
t.test(mydata$Times,alternative = "less",mu=6)

x = read_excel("data/Xr12-25.xlsx")
mydata<-data.frame(x);mydata
str(mydata)
t.test(mydata$Overweight,alternative = "greater",mu=20)


#H0:p=0.8 #H1:p=/0.8
phat <-0.73;p <-0.8;n<-100;Alpha <-0.05
z <-(phat-p)/sqrt((p*(1-p)/n));z
Pvalue <- 2*pnorm(z,lower.tail = TRUE)
Pvalue < Alpha


#H0:p<=0.7 #H1:p>0.7 
phat <-0.75 ;p <-0.8;n<-150 ;Alpha <-0.05
z <-(phat-p)/sqrt((p*(1-p)/n));z
Pvalue <- pnorm(z,lower.tail = FALSE)
Pvalue < Alpha


# == import excel and prop.test() == 

#H0:p<=0.5 #H1:p>0.5
mydata <-read_excel("data/Xr12-112.xlsx")
View(mydata)    #use 'V' not 'v'
str(mydata)
table(mydata)
help(prop.test)
prop.test(57,100,p=0.5,alternative = "greater",conf.level = 0.99,correct = FALSE) #100 smaple choice 57


#H0:p>=0.9 #H1:p<0.9
mydata <-read_excel("data/Xr12-108.xlsx")
View(mydata)
str(mydata)
table(mydata)
help(prop.test)
prop.test(153,177,p=0.9,alternative = "less",conf.level = 0.95,correct = FALSE)   #100 smaple choice 57


#H0:a^2 = 100  #H1:a^2 =/100
svar<-(12.5)^2;pvar<-100;n<-20;Alpha<-0.05
chi<-(n-1)*svar/pvar;chi
Pvalue<-2*pchisq(chi,df=n-1,lower.tail = FALSE);Pvalue
Pvalue < Alpha


#H0:o2<=0.0004  #H1:o2>0.0004
svar<-0.0005;pvar<-(0.02)^2;n<-30;Alpha<-0.05
chi<-(n-1)*svar/pvar; chi
Pvalue<-pchisq(chi,df=n-1,lower.tail = FALSE) ; Pvalue
Pvalue < Alpha


# case1 proportion
phat1 <- 0.44;phat2 <- 0.38;n1 <- 200;n2 <- 100
pooledp <- (phat1*n1+phat2*n2)/(n1+n2)
z <- (phat1-phat2)/sqrt((pooledp*(1-pooledp))*((1/n1)+(1/n2)));z
qnorm(0.1/2)


# case2 proportion
phat1 <- 0.51;phat2 <- 0.38;D <- 0.05;n1 <- 200;n2 <- 100
z <- ((phat1-phat2)-D)/sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2));z
qnorm(0.05)


#check 250 found 30 , 400 that 80   0.05sig
#H0 p1-p2 <= 0 #H1 p1-p2  > 0
phat1 <- 0.12;phat2 <- 0.2;n1 <- 250;n2 <- 400
pooledp <- (phat1*n1+phat2*n2)/(n1+n2)
z <- (phat1-phat2)/sqrt((pooledp*(1-pooledp))*((1/n1)+(1/n2)));z
qnorm(0.1/2)


# 652 out of 1158  #412 out of 982 #0.05 sig 0.1point
#H0 p1-p2 <= 0.1 #H1 p1-p2  > 0.1
phat1 <- 652/1158;phat2 <- 412/98;D <- 0.1;n1 <- 1158;n2 <- 982
z <- ((phat1-phat2)-D)/sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2));z
qnorm(0.05)

#H0 pv1 = pv2  #H1 pv1 != pv2
m <- c(8,9,8,9,8.5,9.5,8.4,8.2)
f <- c(5,5,5.2,5.3,5.5,5.2,5.2,5)
sv1 <- var(m)
sv2 <- var(f)
n1 <- sum(complete.cases(m))
n2 <- sum(complete.cases(f))
F <- sv1/sv2; F
qf(0.05,df1=n1-1,df2=n2-1,lower.tail = TRUE)
qf(0.05,df1=n1-1,df2=n2-1,lower.tail = FALSE)


# == CH2. Confidence interval (CV) 信賴區間
n <- 16;sd <- 25;xbar <- 240;sig <- 0.9
se <- abs(qnorm((1-sig)/2)*sd/sqrt(n));se
lcl <- xbar-se
ucl <- xbar+se
ci <- c(lcl,ucl);ci


#Left tail
#H0:u<=50 H1:u>50
n <-36;xbar <-60;pmean <-50;sd <-10 ;sig <-0.05
z<-(xbar-pmean)/(sd/sqrt(n));z
CV <-qnorm(1-sig) ;CV
Pvalue <- pnorm(z,lower.tail = FALSE);Pvalue
Pvalue > sig
z > CV


############mean##################
#?Huse Xr12 25)
#average American is more than 20 pounds overweight. 
#random sample of Was weighed, and the difference 
#between their actual and idea weights was calculated.
#The data is listed at Xr12 25. 
#Do these data allow us to infer at the5 % significance level that the doctor??s claim is true?

#H0:p<=20 , H1:p>20
mydata <-data.frame(Xr12_25)
View(mydata)
t.test(mydata$Overweight,alternative = "greater",mu=20)

##########proportion##########
#Xr12_108
#H0:p>=0.9 H1:p<0.9
mydata <-data.frame(Xr12_108)
View(mydata)
str(mydata)
mydata$Textbook <- as.faactor(mydata$Textbook)
table(mydata$Textbook)
prop.test(57,100,p=0.5,alternative = "greater",conf.level = 0.99,correct = FALSE)

##########variance############
#H0:a^2=250 H1:a^2<250
mydata <-data.frame(Xr12_72)
View(mydata)
str(mydata)
install.packages("EnvStats")
require(EnvStats)
varTest(mydata$Marks,alternative="less",conf.level=0.90,sigma.sqared=250,data.name=NULL)


##############################

#case3 -A test the equal variance between male and female
# Ho: pv1 = pv2  # h1: pv1 =!pv2
sv1 <- (1.5)^2; sv2 <- (0.8)^2; n1 <- 100; n2 <- 64
F <- sv1/sv2;F
qf(0.025,df=n1-1,df2=n2-1,lower.tail = TRUE)
qf(0.025,df=n1-1,df2=n2-1,lower.tail = FALSE)  #Reject Ho

# Ho: pv1 = pv2 # h1: pv1 =!pv2
xbar1 <- 3.5; xbar2 <- 3; sv1 <- (1.5)^2 ; sv2 <- (0.8)^2; n1 <- 100 ;n2 <- 64
v <- ((sv1/n1)+(sv2/n2))^2/(((sv1/n1)^2/(n1-1))+((sv2/n2)^2/(n2-1)))
t <- (xbar1-xbar2-0)/sqrt((sv1/n1)+(sv2/n2))  ;t
qt(0.025,df=v)
qf(0.025,df=n1-1,df2=n2-1,lower.tail = TRUE)
qf(0.025,df=n1-1,df2=n2-1,lower.tail = FALSE) #Reject Ho

#Yellow White ball
mydata <- read_excel("data/Xr13-12.xlsx")
view(mydata)
str(mydata)
help("var.test")
help("t.test")
var.test(mydata$Yellow,mydata$White,ratio=1,alternative = "two.sided")
t.test(mydata$Yellow,mydata$White,mu=0,alternative = "less", var.equal = TRUE)


#Business problem - Ship Business
mydata <- read_excel("data/Xr13-20.xlsx")
View(mydata)
str(mydata)
help("var.test")
help("t.test")
var.test(mydata$'2YearsAgo',mydata$ThisYear,ratio=1,alternative = "two.sided")      
t.test(mydata$'2YearsAgo',mydata$ThisYear,mu=0,alternative = "greater", var.equal = FALSE)   #the excel title first is number use' '


# == CH2. Inferential statistics 敘述統計 ==
# library(readxl)
# read_excel("data/Xr12-23.xlsx")

#H0: pv1 = pv2 #H1: pv1 =/= pv2
m <-c(8,8,7,8,8.5,9.5,8.4,8.2)
f <-c(5,5,5.2,5.3,5.5,5.2,5.2,5)
sv1 <- var(m);sv2 <- var(f)
n1 <-sum(complete.cases(m)); n1 
n2 <-sum(complete.cases(f)); n2
Alpha <- 0.1
F <- sv1/sv2;F
Pvalue <- 2*pf(F,df1=n1-1,df2=n2-1,lower.tail = FALSE);Pvalue
Pvalue < Alpha
var.test(m,f,ratio=1,alternative="two.sided")

#var.test(x,y,rat)
mydata <- read_excel("data/Xr13-122.xlsx")
View(mydata)
str(mydata)
var.test(mydata$'Machine 1',mydata$'Machine 2',ratio=1,alternative="two.sided")
p-value > Alpha  #non_reject

#h0: pv1 = pv2 #h1: pv1 =/pv2
sv1 <- (1.2)^2; sv2 <- (1.1)^2; n1 <- 100; n2 <- 64; Alpha <-0.05
F <-sv1 / sv2; F
Pvalue <- 2*pf(F,df1=n1-1,df2=n2-1,lower.tail = FALSE);Pvalue
Pvalue < Alpha     #FALSE  #Non Reject H0


#h0: pum1 = pum2 #h1: pum1 =/pum2
xbar1 <-3.2; xbar2 <-3; sv1 <- (1.2)^2; sv2 <- (1.1)^2; n1 <- 100; n2 <- 64; Alpha <-0.05
spooled2 <- ((n1-1)*sv1+(n2-1)*sv2)/(n1+n2-2)
t <- (xbar1-xbar2-0)/sqrt(spooled2*((1/n1)+(1/n2)));t
Pvalue <- 2*pt(t,df=n1+n2-2)
Pvalue < Alpha #FALSE #Non Reject H0

#1. H0:uy<=uw H1:uy>uw
mydata<-read_excel("data/Xr13-12.xlsx")
str(mydata)
#H0:pv1/pv2=1,H1:pv1/pv2 /=1
var.test(mydata$Yellow,mydata$White,ratio=1,alternative="two.sided",conf.level=0.95)   #reject H0
t.test(mydata$Yellow,mydata$White,alternative="less",var.equal=FALSE,conf.level=0.95)  #non reject H0
#The baseball's errors are fewer on average when the yellow ball is used.


#2. H0:utwo<=uthis H1:utwo>uthis
mydata<-read_excel("data/Xr13-20.xlsx")
str(mydata)
#H0:pv1/pv2=1,H1:pv1/pv2 /=1
var.test(mydata$'2YearsAgo',mydata$'ThisYear',ratio=1,alternative="two.sided",conf.level=0.95)     #reject H0
t.test(mydata$'2YearsAgo',mydata$'ThisYear',alternative="greater",var.equal=FALSE,conf.level=0.95) #non reject H0
#The cruise ships are not attracting younger customers.

#3. H0:Ud>=0 H1:Ud<0 Ud=Ub-Ua
mydata<-data.frame(b=c(9,8,7,10,6),a=c(10,9,11,12,9))
View(mydata)
t.test(mydata$b,mydata$a,mu=0,alternative="less",paired=TRUE)   #reject H0
#After more than before so we have enough evidence can reject H0.




