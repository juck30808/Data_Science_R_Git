# RtechSTD.R: 大數據實務技術: 學生實作
# Jia-Sheng Heh (賀嘉生), 12/03/2020

#setwd("c:/Users/jsheh/Desktop/")

#####===== (1) (KDD1) 讀取數據(-->X) =====#####
X = read.csv("vgsales.csv",fileEncoding="Big5");  dim(X);   head(X,2)   #-- [1] 16598    11
#   Rank              Name Platform Year    Genre Publisher NA_Sales EU_Sales
# 1    1        Wii Sports      Wii 2006   Sports  Nintendo    41.49    29.02
# 2    2 Super Mario Bros.      NES 1985 Platform  Nintendo    29.08     3.58
#   JP_Sales Other_Sales Global_Sales
# 1     3.77        8.46        82.74
# 2     6.81        0.77        40.24

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
XX.hc = hclust( dist( XX[,7:10] ) )
##== 試切成3類
XX.group = cutree(XX.hc, k=3);  
length(XX.group)   #-- [1] 5416
XX.group[1:50]
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
# 1  1  1  2  1  1  2  2  2  1  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 
# 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 
#  3  2  2  3  3  2  2  2  2  3  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 
table(XX.group)
# 1    2    3 
# 6   69 5341

##== 試切成4類
XX.group = cutree(XX.hc, k=4);  
XX.group[1:50]
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
# 1  2  1  3  1  1  3  3  3  1  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3 
# 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 
#  4  3  3  4  4  3  3  3  3  4  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3 
table(XX.group)
# 1    2    3    4 
# 5    1   69 5341 
table(cut.h.cluster, XX)    


XX.group = cutree(XX.hc, k=3);  XX.group[1:50]
table(XX.group)
plot(XX.group)
