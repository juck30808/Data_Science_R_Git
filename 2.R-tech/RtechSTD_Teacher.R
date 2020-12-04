# RtechSTD.R: 大數據實務技術: 學生實作
# Jia-Sheng Heh (賀嘉生), 12/03/2020

setwd("/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data/")

########## 第二組的數據例 ##########

#####===== (1) (KDD1) 讀取數據(-->X) =====#####
X = read.csv("vgsales.csv",fileEncoding="Big5");  dim(X);   head(X,2)   #-- [1] 16598    11
#   Rank              Name Platform Year    Genre Publisher NA_Sales EU_Sales
# 1    1        Wii Sports      Wii 2006   Sports  Nintendo    41.49    29.02
# 2    2 Super Mario Bros.      NES 1985 Platform  Nintendo    29.08     3.58
#   JP_Sales Other_Sales Global_Sales
# 1     3.77        8.46        82.74
# 2     6.81        0.77        40.24
### X$Year = as.integer(as.character(X$Year))
range(X$Global_Sales)   #-- [1]  0.01 82.74
table( cut(X$Global_Sales, breaks=c(0,0.2,1,4,9,49,99)) )
# (0,0.2] (0.2,1]   (1,4]   (4,9]  (9,49] (49,99] 
#    9103    5441    1754     225      74       1 

#####===== (2) (KDD2-3) 數據轉換(X-->XX) =====#####
table(X$Year)
# 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 
#    9   46   36   17   14   14   21   16   15   17   16   41   43   60 
# 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 
#  121  219  263  289  379  338  349  482  829  775  763  941 1008 1202 
# 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2020  N/A 
# 1428 1431 1259 1139  657  546  582  614  344    3    1  271 
length(which(X$Year>=2010))   #-- [1] 5416
XX = X[which(X$Year>=2010),];   dim(XX)   #-- [1] 5416   11
rownames(XX) = 1:dim(XX)[1]

#####===== (3) (KDD4) 數據模型(XX-->XX.group) =====#####
XX.hc = hclust( dist( XX[,7:10] ), method="ward.D")
##== 試切成3類
XX.group = cutree(XX.hc, k=3);  
length(XX.group)   #-- [1] 5416
XX.group[1:50]
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
# 1  1  1  2  1  1  2  2  2  1  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 
# 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 
#  3  2  2  3  3  2  2  2  2  3  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 
table(XX.group)
#   1    2    3 
# 242 1346 3828 

##== 試切成4類
XX.group = cutree(XX.hc, k=4);  
table(XX.group)
#  1    2    3    4 
# 20  222 1346 3828 

##== 試切成5類
XX.group = cutree(XX.hc, k=5);  
table(XX.group)
#  1    2    3    4    5 
# 20  222  468  878 3828

##== 試切成10類
XX.group = cutree(XX.hc, k=10);  
table(XX.group)
# 1    2    3    4    5    6    7    8    9   10 
# 1    1    4    6    6   22    4   35 5329    8 
#  1    2    3    4    5    6    7    8    9   10 
# 20   18   42   42  120  468  587  291 1733 2095 

##== 試切成27類
XX.group = cutree(XX.hc, k=27);  
table(XX.group)
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 5   8  10   7  11  42  51   8  31  18  98  51  87  33  73 153 159 
#  18  19  20  21  22  23  24  25  26  27 
# 144 131 369  99 586 528 619 565 815 715
Ncls = 27

#####===== (4) (KDD5) 數據解讀(XX.group) =====#####
XX[which(XX.group==6),7:10]  #-- 2.9NA+2.0EU+0.2JP+0.7OT
apply(XX[which(XX.group==6),7:10], 2, max)
apply(XX[which(XX.group==6),7:10], 2, min)
round(apply(XX[which(XX.group==6),7:10], 2, mean), 2)
round(apply(XX[which(XX.group==6),7:10], 2, sd), 2)
# NA_Sales    EU_Sales    JP_Sales Other_Sales 
#     4.52        3.30        1.27        1.37 --- max
#     1.78        0.26        0.00        0.32 --- min
#     2.91        1.95        0.15        0.67 --- mean
#     0.72        0.66        0.28        0.29 --- sd
XX[which(XX.group==7),7:10]  #-- 1.7NA+1.1EU+0.2JP+0.3OT
round(apply(XX[which(XX.group==7),7:10], 2, mean), 2)
# NA_Sales    EU_Sales    JP_Sales Other_Sales 
#     1.69        1.07        0.20        0.34 


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#####===== (5) (KDD5) 數據解讀(XX.group) =====#####
kk = 6
indKK = which(XX.group==kk);   indKK
c(kk,length(indKK), apply(XX[indKK,7:10], 2, mean))
##== Group Means (Gmean) ==##
Gmean = NULL
for (kk in 1:Ncls) {
  indKK = which(XX.group==kk);   indKK
  c(kk,length(indKK), apply(XX[indKK,7:10], 2, mean))
  Gmean = rbind(Gmean, c(kk,length(indKK), apply(XX[indKK,7:10], 2, mean)))  
}
round(Gmean,2)[order(Gmean[,2],decreasing=T),][1:2,]
#             NA_Sales EU_Sales JP_Sales Other_Sales
# [1,] 26 815     0.01     0.02     0.00           0
# [2,] 27 715     0.00     0.00     0.02           0

##== Group Features (Gfeature) ==##
colnames(Gmean)
# [1] ""   ""    "NA_Sales"    "EU_Sales"    "JP_Sales"   "Other_Sales"
Gmean[1,]
#                      NA_Sales    EU_Sales    JP_Sales Other_Sales 
# 1.000       5.000      10.310       4.512       0.122       1.324

##==> 希望得到: 10.3*NA_Sales + 4.5*EU_Sales + 0.1*JP_Sales + 1.3*Other_Sales
colnames(Gmean)[3:6]  #-- [1] "NA_Sales" "EU_Sales" "JP_Sales" "Other_Sales"
Gmean[1,3:6]          #--     10.310       4.512       0.122       1.324
AA = paste0(round(Gmean[1,3:6],1), "*", colnames(Gmean)[3:6]);  AA
# [1] "10.3*NA_Sales"  "4.5*EU_Sales"  "0.1*JP_Sales"  "1.3*Other_Sales"
BB = paste( AA, collapse="+");   BB
# [1] "10.3*NA_Sales+4.5*EU_Sales+0.1*JP_Sales+1.3*Other_Sales"

##==> 用迴圈包成Gfeature
Gfeature = NULL
for (k in 1:dim(Gmean)[1]) {
  print(k)
  AA = paste0(round(Gmean[k,3:6],2), "*", colnames(Gmean)[3:6]);  AA
  BB = paste( AA, collapse="+")
  print(BB)
  Gfeature[k] = BB
}
Gfeature

##==> 把Gmean,Gfeature合成數據框
Gm = as.data.frame(Gmean);  head(Gm,2)
colnames(Gm)[1:2] = c("ind","count");   head(Gm,2)
Gm$feature = Gfeature
head(Gm,2)
#   ind count NA_Sales EU_Sales JP_Sales Other_Sales
# 1   1     5  10.3100    4.512  0.12200      1.3240
# 2   2     8   5.0175    5.705  0.56125      2.2425
#                                                       feature
# 1 10.31*NA_Sales+4.51*EU_Sales+0.12*JP_Sales+1.32*Other_Sales
# 2   5.02*NA_Sales+5.7*EU_Sales+0.56*JP_Sales+2.24*Other_Sales
write.csv(Gm,"Gm.csv")
