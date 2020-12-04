# RtechSTD.R: 大數據實務技術: 學生實作
# Jia-Sheng Heh (賀嘉生), 12/03/2020

setwd("/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data/")


#####===== (1) (KDD1) 讀取數據(-->X) =====#####
dt = read.csv("MRT.csv",fileEncoding="UTF-8");  
dim(dt);   head(dt,2)  


#####===== (2) (KDD2-3) 數據轉換(X-->XX) =====#####
table(dt$Year)
length(which(dt$Year>=100)) 
XX = dt[which(dt$Year>=100),];   dim(XX) 
#rownames(XX) = 1:dim(XX)[1]



#####===== (3) (KDD4) 數據模型(XX-->XX.group) =====#####
E.dist = dist(XX[,7:10], method="ward.D")   #聚類方式
XX.hc = hclust(E.dist)
plot(XX.hc, xlab="歐式距離")


XX.hc = hclust( dist( XX[,7:10] ), method="ward.D")


XX.group = cutree(XX.hc, k=3);  ##== 試切成3類
length(XX.group) 
XX.group[1:50]
table(XX.group)

XX.group = cutree(XX.hc, k=4);   ##== 試切成4類
table(XX.group)

XX.group = cutree(XX.hc, k=5);   ##== 試切成5類
table(XX.group)

XX.group = cutree(XX.hc, k=10);  ##== 試切成10類
table(XX.group)


XX.group = cutree(XX.hc, k=27);   ##== 試切成27類
table(XX.group)
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 5   8  10   7  11  42  51   8  31  18  98  51  87  33  73 153 159 
#  18  19  20  21  22  23  24  25  26  27 
# 144 131 369  99 586 528 619 565 815 715

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

