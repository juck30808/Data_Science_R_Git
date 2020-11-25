setwd("D:/R語言/INDIA")
#####==== (1) (KDD1) Data Acquisition (file-->X) =====#####
X = read.csv("file_02.csv");  dim(X);  head(X,2)  #-- [1] 4945    9
#   index       Date   Region Thermal.Generation.Actual..in.MU. Thermal.Generation.Estimated..in.MU.
# 1     0 2017-09-01 Northern                            624.23                               484.21
# 2     1 2017-09-01  Western                          1,106.89                             1,024.33
#   Nuclear.Generation.Actual..in.MU. Nuclear.Generation.Estimated..in.MU.
# 1                             30.36                                35.57
# 2                             25.17                                 3.81
#   Hydro.Generation.Actual..in.MU. Hydro.Generation.Estimated..in.MU.
# 1                          273.27                             320.81
# 2                           72.00                              21.53
colnames(X) = c("index","Date","Region","aT","eT","aN","eN","aH","eH");   head(X,2)
#   index       Date   Region       aT       eT    aN    eN     aH     eH
# 1     0 2017-09-01 Northern   624.23   484.21 30.36 35.57 273.27 320.81
# 2     1 2017-09-01  Western 1,106.89 1,024.33 25.17  3.81  72.00  21.53

#####==== (2) (KDD2) Data Exploration (X) =====#####
range(X$Date)  #-- [1] "2017-09-01" "2020-08-01"
table(X$Region)
# Eastern NorthEastern     Northern     Southern      Western 
#     989          989          989          989          989 
X$aT = as.numeric(X$aT)
X$eT = as.numeric(X$eT)
X$aN = as.numeric(X$aN)
X$eN = as.numeric(X$eN)
X$aH = as.numeric(X$aH)
X$eH = as.numeric(X$eH)
##--把內容轉為數值


##--印度每日發電量
head(X,2)
Xa = X[,c("aT","aN","aH")];   head(Xa,2)
#       aT    aN     aH
# 1 624.23 30.36 273.27
# 2     NA 25.17  72.00
#-- (1) NA-->0
which(is.na(Xa), arr.ind=T)[1:2,]
#      row col
# [1,]   2   1
# [2,]   7   1
Xa[which(is.na(Xa), arr.ind=T)] = 0;   head(Xa,2)
#       aT    aN     aH
# 1 624.23 30.36 273.27
# 2   0.00 25.17  72.00

#-- (2) sum with na.rm (NA removed)
X$TE = rowSums(Xa, na.rm=T);   head(X,2)
#   index       Date   Region     aT     eT    aN    eN     aH     eH     TE
# 1     0 2017-09-01 Northern 624.23 484.21 30.36 35.57 273.27 320.81 927.86
# 2     1 2017-09-01  Western     NA     NA 25.17  3.81  72.00  21.53  97.17

##=== modeling
xtabs(TE~Date+Region, data=X)

###############


