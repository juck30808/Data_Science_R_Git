# RtechSTD.R: 大數據實務技術: 學生實作
# Team1  12/04/2020


#####===== (1) (KDD1) 讀取數據(-->X) =====#####

library(readxl)
X <- as.data.frame(read_excel('/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data/googleplaystore2.xlsX'))
dim(X);   
head(X,2)  


#####===== (2) (KDD2-3) 數據轉換(X-->XX) =====#####

range(X$Rating) 
table( cut(X$Rating, breaks=c(0,1,2,3,4,5)) )


range(X$Year)   #2010 - 2018
table(X$Year)
XX = X[which(X$Year>=2018),];   dim(XX)
rownames(XX) = 1:dim(XX)[1]


#####===== (3) (KDD4) 數據模型(XX-->XX.group) =====#####


X.hc = hclust( dist( X[,c("Rating","Reviews","Installs")] ),method="ward.D")
X.group = cutree(X.hc, k=20)

X.group[1:50]
table(X.group)

Ncls = 20  #cause k =20
X[which(X.group==1),c("Rating","Reviews","Installs")]
apply(X[which(X.group==1),c("Rating","Reviews","Installs")], 2, max)
apply(X[which(X.group==1),c("Rating","Reviews","Installs")], 2, min)
round(apply(X[which(X.group==1),c("Rating","Reviews","Installs")], 2, mean),2)
round(apply(X[which(X.group==1),c("Rating","Reviews","Installs")], 2, sd),2)
# Rating  Reviews Installs 
# 5      6477    10000      
# 1      1       1 
# 4.11   145.01  4646.57 
# 0.73   360.82  4272.84 



#####===== (4) (KDD5) 數據解讀(XX.group) =====#####

kk = 1
indKK = which(X.group==kk);   indKK
c(kk,length(indKK), apply(X[indKK,c("Rating","Reviews","Installs")], 2, mean))


##== Group Means (Gmean) ==##
Gmean = NULL
for (kk in 1:Ncls) {
  indKK = which(X.group==kk);   indKK
  c(kk,length(indKK), apply(X[indKK,c("Rating","Reviews","Installs")], 2, mean))
  Gmean = rbind(Gmean, c(kk,length(indKK), apply(X[indKK,c("Rating","Reviews","Installs")], 2, mean)))  
}
round(Gmean,2)[order(Gmean[,2],decreasing=T),][1:2,]


##== Group Features (Gfeature) ==##
colnames(Gmean)
round(Gmean[1,],2)
#                  Rating  Reviews Installs 
# 1.00  2703.00     4.11   145.01  4646.57 

##-- 希望得到: 
##-- 4.11 * Rating 
##-- + 145.01 * Reviews
##-- + 4646.57 * Installs

colnames(Gmean)[3:5]   #表頭
round(Gmean[1,3:5],2)  #加權 

AA = paste0(round(Gmean[1,3:5],1), "*", colnames(Gmean)[3:6]);  AA
BB = paste( AA, collapse="+");   BB
# [1] "10.3*NA_Sales+4.5*EU_Sales+0.1*JP_Sales+1.3*Other_Sales"


#####===== (5) (KDD5) 數據解讀(XX.group) =====#####

##==> 用迴圈包成Gfeature
Gfeature = NULL
for (k in 1:dim(Gmean)[1]) {
  print(k)
  AA = paste0(round(Gmean[k,3:5],2), "*", colnames(Gmean)[3:5]);  AA
  BB = paste( AA, collapse="+")
  print(BB)
  Gfeature[k] = BB
}
Gfeature

##==> 把Gmean,Gfeature合成數據框
Gm = as.data.frame(Gmean);  head(Gm)
colnames(Gm)[1:2] = c("ind","count");   head(Gm)
Gm$feature = Gfeature
head(Gm)

# ind count   Rating     Reviews     Installs                                       feature
# 1   1  2703 4.114058    145.0067     4646.566   4.11*Rating+145.01*Reviews+4646.57*Installs
# 2   2   487 4.166530   9483.6181   500000.000    4.17*Rating+9483.62*Reviews+5e+05*Installs
# 3   3   525 4.248952 106100.6076  5000000.000  4.25*Rating+106100.61*Reviews+5e+06*Installs
# 4   4   115 4.306957 889797.8348 50000000.000  4.31*Rating+889797.83*Reviews+5e+07*Installs
# 5   5  1463 4.087697   2570.0615    85201.640 4.09*Rating+2570.06*Reviews+85201.64*Installs
# 6   6  1294 4.219706  33331.1955  1000000.000    4.22*Rating+33331.2*Reviews+1e+06*Installs

write.csv(Gm,"Team1.csv")


