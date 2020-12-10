# RtechSTD.R: 大數據實務技術: 學生實作
# Team1  12/04/2020


#####===== (1) (KDD1) 讀取數據(-->X) =====#####
library(readxl)
X <- as.data.frame(read_excel('/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data/googleplaystore4_(7000).xlsX'))
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

X.group


#####===== (4) (KDD5) 數據解讀(XX.group) =====#####
kk = 1  #X.group(kk)
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

AA = paste0(round(Gmean[1,3:5],2), " * ", colnames(Gmean)[3:5]);  AA
BB = paste( AA, collapse=" + ");   BB
# "4.1 * Rating + 145 * Reviews + 4646.6 * Installs"


#####===== (5) (KDD5) 數據解讀(XX.group) =====#####

##==> 用迴圈包成Gfeature
Gfeature = NULL
for (k in 1:dim(Gmean)[1]) {
  print(k)
  AA = paste0(round(Gmean[k,3:5],2), " * ", colnames(Gmean)[3:5]);  AA
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

#   ind count   Rating     Reviews     Installs                                       feature
# 1   1  2703 4.114058    145.0067     4646.566   4.11*Rating+145.01*Reviews+4646.57*Installs
# 2   2   487 4.166530   9483.6181   500000.000    4.17*Rating+9483.62*Reviews+5e+05*Installs
# 3   3   525 4.248952 106100.6076  5000000.000  4.25*Rating+106100.61*Reviews+5e+06*Installs
# 4   4   115 4.306957 889797.8348 50000000.000  4.31*Rating+889797.83*Reviews+5e+07*Installs
# 5   5  1463 4.087697   2570.0615    85201.640 4.09*Rating+2570.06*Reviews+85201.64*Installs
# 6   6  1294 4.219706  33331.1955  1000000.000    4.22*Rating+33331.2*Reviews+1e+06*Installs

write.csv(Gm,"Team1.csv")




#--- Rtech04 ----

# install.packages( c("arules","arulesViz","igraph","data.table","jiebaR","text2vec") )    

setwd("/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data")
library(arules);library(igraph)
data(Groceries)
X = read.csv("googleplaystore4_(7000).csv");   dim(X);   head(X,2) 
# [1] 7684   16
# App                                            Category Rating   Reviews Size Installs Type Price Content.Rating                    Genres Last.Updated Year Month Day Current.Ver  Android.Ver
# Photo Editor & Candy Camera & Grid & ScrapBook ART_AND_DESIGN    4.1     159  19M    10,000 Free     0       Everyone              Art & Design     7-Jan-18 2018     1   7       1.0.0 4.0.3 and up
#                            Coloring book moana ART_AND_DESIGN    3.9     967  14M   500,000 Free     0       Everyone Art & Design;Pretend Play    15-Jan-18 2018     1  15       2.0.0 4.0.3 and up
summary(X)
table(table(X$Category)) 
#table(table(X$Genres))[1:17]
# 37   38   43   47   51   56   59   61   63   84   89   95  110  116  144  160  166  171  177  179  211  223  235  245  247  266  278  279  324  627  966 1602 
# 1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    2    1    1    1    1    1    1    1    1    1 
#t(X[1:20,7:10])

# measure 評估各個品類(Category) 在有多少有 免費/年份推出 
#PG=table(X$Category,X$Year);   rownames(PG)=NULL;  PG  # 1-33個品項  #PG[1:10,] 
PD=table(X$Category,X$Year);   rownames(PD)=NULL;  PD     # 1-33個品項  #PD[1:10,] 
#以下計算只取PD值

# apriori演算法大概是這樣運作的，我們必須要設定support以及confidence:
# 支持度(support)：「規則」在資料內具有普遍性，也就是這些 A 跟 B 同時出現的機率多少。
# 信賴度(confidence)：「規則」要有一定的信心水準，也就是當購買 A 狀態下，也會購買 B 的條件機率。
txPD = lapply( 1:dim(PD)[1], FUN=function(k) colnames(PD)[which(PD[k,]>0)] )
arPD = apriori( txPD[1:6], parameter=list(support=0.06, confidence=0.8), control=list(verbose=FALSE));  
#catch 1-6 year only

#lhs=>rhs 代表買左邊也會買右邊的意思，而支持度與信賴度，則分別代表了普遍性與信心水準。
inspect(arPD[9:20,])

graph.arPD = graph.edgelist( cbind(inspect(arPD)[1:50,]$lhs, inspect(arPD)[1:50,]$rhs) )
plot(graph.arPD, edge.arrow.size=0.1, edge.curved=0.3)

