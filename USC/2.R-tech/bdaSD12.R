# bdaSD12.R: bda System Design 12 program
# Usage: setwd("/Users/mcsl/Desktop/1050205TPC/BDAcourses/BDA系統設計SD課程/");    
#        source("bdaSD12.R", encoding="UTF8")
# Jia-Sheng Heh, 01/07/2017

###== (12A) 讀取時間序列(time series) ==###
##=== [R语言时间序列初探!, (Avril Coghlan, 2016), A Little Book of R For Time Series, 0.2]
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
  kings  
  kingstimeseries <- ts(kings)
  kingstimeseries  #-- 从威廉一世开始的英国国王的去 世年龄数据。(原始出处:Hipel and Mcleod, 1994)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") 
  birthstimeseries <- ts(births, frequency=12, start=c(1946,1)) 
  birthstimeseries  #-- 从 1946 年 1 月到 1959 年 12 月的纽约每月出生人口数量(由牛顿最初收集)数据集
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
  souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1)) 
  souvenirtimeseries  #-- 包含着一家位于昆士兰海滨度假圣地 的纪念品商店从 1987 年 1 月到 1987 年 12 月的每月销售数据(原始数据源于 Wheelwright and Hyndman, 1998)

###== (12B) 繪製時間序列 [同上] ==###
plot.ts(kingstimeseries, main="kingstimeseries")
plot.ts(birthstimeseries, main="birthstimeseries")
plot.ts(souvenirtimeseries, main="souvenirtimeseries")
logsouvenirtimeseries <- log(souvenirtimeseries);    plot.ts(logsouvenirtimeseries,main="logsouvenirtimeseries")

###== (12C) 時間序列=季節性＋趨勢性＋不規則(隨機性) [同上] ==###
##=== (12C1) 分解 非季节性数据 =====
# install.packages("TTR")  
library(TTR)   #-- Technical Trading Rules
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)  #-- Moving Average of Series (n=跨度number of periods to average over)
plot.ts(kingstimeseriesSMA3)  #--简单移动平均平滑后,时间序列依然呈现出大量的随便波动
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8) 
plot.ts(kingstimeseriesSMA8)  #--简单移动平均平滑数据的趋势看起来更清晰,前 20 为国王去世年龄从最初的55周岁下降到38周岁,然后一直上升到第40届国王的73周岁
##=== (12C2) 分解 季节性数据 =====
birthstimeseriescomponents <- decompose(birthstimeseries)  #--小心: 此函數會被 igraph OVERRIDE 掉
plot(birthstimeseriescomponents)  #--估计出的趋势: 从1947年的24下降到1948年的22,紧随着是一个稳定的增加直到1958年的27
##=== (12C3) 季节性因素 調整 =====
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)  #--經過季节性修正的时间序列

###== (12D) 使用指数平滑法进行预测 [同上] ==###
##=== (12D1A) 简单指数平滑法 (HoltWinters(): interpolation) =====
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries) #--整個曲線處於大致不變的水平(約25英尺)，隨機變動大致不變，可視為相加模型
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts  #-- alpha=0.024 (非常接近0,预测是基于最近的和较远的一些观测值)
rainseriesforecasts$fitted
plot(rainseriesforecasts)  #--预测的时间序列比原始时间序列数据 平滑非常多
rainseriesforecasts$SSE
rainseriesforecasts2356 <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)
plot(rainseriesforecasts2356)
##=== (12D1B) 简单指数平滑法 (forecast(): extrapolation) =====
# install.packages("forecast")
library(forecast)
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8) 
rainseriesforecasts2
plot.forecast(rainseriesforecasts2)  #--預測1913-1920的降雨量: 預測值/80%/95%預測區間
plot.ts(rainseriesforecasts2$residuals)  #--样本内预测误差圖
hist(rainseriesforecasts2$residuals)

###== (12E) ARIMA (Autoregressive Integrated Moving Averaging) [同上]  ==###
###== (12E1) Model Order Determination [同上]  ==###
kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1) 
plot.ts(kingtimeseriesdiff1,main="kingtimeseriesdiff1")
acf(kingtimeseriesdiff1, lag.max=20) # plot a correlogram
acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get the autocorrelation values
#-- 滞后1阶(lag1)的自相关值(-0.360)超出了置信边界,但是其他所有在滞后 1-20 阶(lags 1-20)的自相关值都没有超出置信边界
pacf(kingtimeseriesdiff1, lag.max=20) # plot a partial correlogram
pacf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get the partial autocorrelation values
#-- 偏相关图显示在滞后 1,2 和 3 阶(lags 1,2,3)时的偏自相关系数超出了置信边界,为负值,且在等级上随着滞 后阶数的增加而缓慢减少(lag 1:-0.360,lag 2:-0.335,lag 3:-0.321)。从 lag 3 之后偏自相关系数值缩 小至 0.
#--- ARMA(3,0)模型:即偏自相关值在滞后 3 阶(lag 3)之后缩小至 0 且自相关值缩小至 0(即使此模型中 说自相关值缩小至 0 有些不太合适),则是一个阶层 p=3 自回归模型。
#--- ARMA(0,1)模型:即自相关图在滞后 1 阶(lag 1)之后为 0 且偏自相关图缩小至 0,则是一个阶数 q=1 的移动平均模型。
#--- ARMA(p,q)模型:即自相关图和偏相关图都缩小至 0(即使此模型中说自相关图缩小至 0 有些不太合 适),则是一个具有 p 和 q 大于 0 的混合模型。
###== (12E2) Model Order Automatic Determination [同上]  ==###
library(forecast)
auto.arima(kings)  ##-- 输出说明合适的模型是 ARIMA(0,1,1).
###== (12E3) Model Estimation [同上]  ==###
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model 
kingstimeseriesarima
###== (12E4) Model Prediction [同上]  ==###
kingstimeseriesforecasts <- forecast.Arima(kingstimeseriesarima, h=5) 
kingstimeseriesforecasts
plot.forecast(kingstimeseriesforecasts)

###== (12F) Chaotic Behavior ==###
###== (12F1) Logistic Cobweb [(Nicole Radziwill, 2015). Logistic Growth, S Curves, Bifurcations, and Lyapunov Exponents in R, https://www.r-bloggers.com/logistic-growth-s-curves-bifurcations-and-lyapunov-exponents-in-r/]
logistic.map <- function(r, x, N, M){
  ## from http://www.magesblog.com/2012/03/logistic-map-feigenbaum-diagram.html
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}
logistic.cobweb <- function(r,N) {
  # code adapted from http://bayesianbiologist.com/tag/cobweb-plot/
  x<-seq(0,1,length=100)
  x_next <- lapply(2:N, function(i) r*x[i]*(1-x[i]))
  plot(x[2:N],x_next,type="l",xlim=c(0,1), ylim=c(0,1), main=paste("r=",r),
       xlab=expression(x[t]),ylab=expression(x[t+1]), col="red", lwd=2)
  abline(0,1)
  
  # start at your random spot on the x-axis and start with a vertical line:
  start=runif(1,0,1)
  vert=FALSE
  lines(x=c(start,start),y=c(0,r*start*(1-start)) )
  for(i in 1:(2*N)) {
    if(vert) {
      lines(x=c(start,start),y=c(start,r*start*(1-start)) )
      vert=FALSE
    } else {
      lines(x=c(start, r*start*(1-start)), y=c(r*start*(1-start), r*start*(1-start)) )
      vert=TRUE
      start=r*start*(1-start)
    }
  }
}
plot(logistic.map(2.6,.01,20,20), type="l", main="z[i+1]<-r*z[i]*(1-z[i]), r=2.6")
logistic.cobweb(2.6, 100)
plot(logistic.map(3.3,.01,20,20), type="l", main="z[i+1]<-r*z[i]*(1-z[i]), r=3.3")
logistic.cobweb(3.3, 100)

###== (12F2) Embedded Dimensions [package "tseriesChaos"][(quantsignals, 2012). Nonlinear systems, https://www.r-bloggers.com/nonlinear-systems/]  ==###
# install.packages("tseriesChaos")
library(tseriesChaos)
library(scatterplot3d)
plot(rossler.ts)
acf(rossler.ts, lag.max=20)
pacf(rossler.ts, lag.max=20)
x <- window(rossler.ts, start=90)
C2(rossler.ts, m=1, d=10, t=90, eps=0.1)   #-- Sample correlation integral for the specified length scale BUT no further description
lyap_k(rossler.ts, m=3, d=8, t=90, k=1, ref=100, s=100, eps=0.1)   #-- Lyapunov exponent STILL no further description
xyz <- embedd(x, m=3, d=8)
scatterplot3d(xyz, type="l")
pc<-xyz[1000+which(sapply( 1000:(length(xyz[,1])-1) , function(i) ( (xyz[i+1,1]<xyz[i,1]) * (xyz[i,1]*xyz[i+1,1])  ) )<0),3]
plot(pc[1:(length(pc)-1)],pc[2:length(pc)],pch=16,cex=.4,xlab="current state", ylab="next state", main="Return Map (pc(t)-pt(t+1))")
abline(0,1)
plot(pc[1:(length(pc)-2)],pc[3:length(pc)],pch=16,cex=.4,xlab="previous state", ylab="next state",main="Return Map (pc(t)-pt(t+2))")
abline(0,1)

