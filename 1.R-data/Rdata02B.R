# Rdata02B.R: Part B for data transformation
# Jia-Sheng Heh, 11/10/2020,  revised from Rdata01.R

wkDir = "C:/Users/jsheh/Desktop/working/USC/AIbda/";   setwd(wkDir)

########## (D).隱含迴圈(implicit looping): 部份apply家族 ##########

#####===== (D1).apply:矩陣或高維陣列 =====#####
M = array( seq(32), dim=c(4,4,2) );   M
# , , 1
# [,1] [,2] [,3] [,4]
# [1,]    1    5    9   13
# [2,]    2    6   10   14
# [3,]    3    7   11   15
# [4,]    4    8   12   16
# , , 2
# [,1] [,2] [,3] [,4]
# [1,]   17   21   25   29
# [2,]   18   22   26   30
# [3,]   19   23   27   31
# [4,]   20   24   28   32
apply(M,1,sum)   #-- [1] 120 128 136 144

#####===== (D2).mapply:多個向量或陣列 =====#####
A = 1:5
B = 1:5
C = 1:5
mapply(sum,A,B,C)   #-- [1]  3  6  9 12 15

#####===== (D3).sapply:向量 =====#####
z=list( p=1, q=1:3, r=10:100)
sapply(z, FUN=length)
# p  q  r 
# 1  3 91 

#####===== (D4).lapply:表列 =====#####
z = list( p=1, q=1:3, r=10:100);  z
lapply(z, FUN=length)
# $p
# [1] 1
# $q
# [1] 3
# $r
# [1] 91

#####===== (D5).tapply:向量的子集合 =====#####
x = 1:20
y = factor( rep(letters[1:5], each=4 ) );   y
# [1] a a a a b b b b c c c c d d d d e e e e
# Levels: a b c d e
tapply(x,y,sum)
#  a  b  c  d  e 
# 10 26 42 58 74 


########## (E).從迴圈效益到數據操弄(data wrangling) ##########

#####===== (E0).準備數據 (XXX2.csv-->X) =====#####
X = read.csv("XXX2.csv");   dim(X);   head(X,2)   #-- [1] 84008   9
#   invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2        N2      s1       c2      p2    kind1  1400 2015-01-18 19:56:06        1   1197      sub2 793.36
XX = X[1:30000,]
C0 = unique(XX$customer);   length(C0)   #-- 3390

#####===== (E1) FF by for-loop =====#####
FF.for = rep(0,length(C0))
start.time = Sys.time()
for (k in 1:length(C0)) FF.for[k] = length(unique(XX$invoiceNo[which(XX$customer==C0[k])]))
end.time = Sys.time()
duration.for = end.time - start.time
print("duration.for is");  print(duration.for)         #-- 1.123281 secs

#####===== (E2) FF by sapply =====#####
FF.sapply = rep(0,length(C0))
start.time = Sys.time()
FF.sapply = sapply(1:length(C0), function (k) length(unique(XX$invoiceNo[which(XX$customer==C0[k])])) )
end.time = Sys.time()
duration.sapply = end.time - start.time
print("duration.sapply is");  print(duration.sapply)   #-- 1.040518 secs

#####===== (E3) FF by aggregate =====#####
FF.aggregate = rep(0,length(C0))
start.time = Sys.time()
Xtest <- aggregate( XX[,"invoiceNo"], by=list(XX[,"customer"]), FUN=function(x)length(unique(x)))
end.time = Sys.time()
duration.aggregate = end.time - start.time
print("duration.aggregate is");  print(duration.aggregate)    #-- 0.1583619 secs

#####===== (E4) FF by dplyr (數據操弄dplyr/tidyr) =====#####
library(plyr)
start.time = Sys.time()
Ctemp <- ddply(X, "customer", summarise, FF.ddply=length(unique(XX$invoiceNo[which(XX$customer==C0[k])])) )
end.time = Sys.time()
duration.ddply = end.time - start.time
print("duration.ddply is");  print(duration.ddply)     #-- 3.883581 secs

#####===== (E5) FF by table =====#####
start.time = Sys.time()
FF.table <- table(XX$customer,XX$invoiceNo)
FF.tableSum <- rowSums(FF.table)
end.time = Sys.time()
duration.table = end.time - start.time
print("duration.table is");  print(duration.table)     #-- 0.603364 secs

#####===== (E6) FF by data.table =====#####
library(data.table)
start.time = Sys.time()
setDT(XX, key=c("customer","invoiceNo"))
FF.data.table <- XX[, .(FF=length(unique(invoiceNo))), by=customer]
end.time = Sys.time()
duration.data.table = end.time - start.time
print("duration.data.table is");  print(duration.data.table)     #-- 0.04791284 secs

#####===== (E7) 數據操弄 (data wrangling) =====#####
# [資料科學實驗室, Wu Jerry, 2015/02/18]
##== plyr 
#    -- 可以將vector、list、data.frame的資料做快速的切割、應用、組合
#    -- 是非常好用的套件，像是join功能，可以做inner、left、right、full等join功能
#    -- plyr可以讓工程師以資料庫的概念，有效率的把玩資料。
##== dplyr 
#    -- 跟plyr類似，但是針對data.frame、data.table、以及多種資料庫為基礎的資料
#    -- 將資料做快速的切割、應用、組合，尤其處理大量資料，dplyr是非常好用的工具。 
##== reshape2
#    -- 可以幫助我們將資料進行縱向、橫向轉換，
#    -- 對於該套件處理連續型或時間資料是非常好用的，如空氣品質資料、證券行情資料等
#    -- 通常會再搭配dplyr，讓資料分析事半功倍。



########## (F).數據轉換的核心: 數據框轉換 ##########

#####===== (F1).(KDD1-2) 準備數據 (XXX2.csv-->X) =====#####
X = read.csv("XXX2.csv");   dim(X);   head(X,2)   #-- [1] 84008   9
#   invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2        N2      s1       c2      p2    kind1  1400 2015-01-18 19:56:06        1   1197      sub2 793.36

#####===== (F2).(KDD3) SPC模型的數據轉換(X->Cv) =====#####
library(data.table)
X$date = as.character(as.Date(X$datetime))
X$date = as.Date(X$datetime)
start.time = Sys.time()
setDT(X, key=c("customer"))
Cv = X[, .(D0=min(date), Df=max(date), DD=length(unique(date)),
             FF=length(unique(invoiceNo)), MM=sum(amount), TT=sum(quantity)), by=customer ] 
end.time = Sys.time()
duration.all.data.table = end.time - start.time
print("duration.all.data.table is");  print(duration.all.data.table)     #-- 0.4124379 secs

#####===== (F3).(KDD3) 客戶標籤的設計(Cv$FF0,$MM0,$BB0,$Q0,$Qf) =====#####
range(Cv$FF)   #--     1    11025
range(Cv$MM)   #-- -2400 18251044
Cv$FF0 = cut( Cv$FF, breaks=c(0,1,10,40,200,20000));  table(Cv$FF0)  
# (0,1]      (1,10]     (10,40]    (40,200] (200,2e+04] 
#  4169        3281         275          44           5 
Cv$MM0 = cut( Cv$MM, breaks=c(-5000,0,999,9999,99999,999999,19999999));   table(Cv$MM0)
# (-5e+03,0]       (0,999]   (999,1e+04] (1e+04,1e+05] (1e+05,1e+06] (1e+06,2e+07] 
#         20           937          5587          1193            33             4 
Cv$BB = (Cv$Df-Cv$D0)/Cv$DD
Cv$BB0 = cut( as.numeric(Cv$BB), breaks=c(-1,0,7,30,100,999));  table(Cv$BB0)
# (-1,0]     (0,7]    (7,30]  (30,100] (100,999] 
#   4544       482       712      1146       890 
Cv$Q0 = paste0(substr(Cv$D0,3,4), cut(as.integer(substr(Cv$D0,6,7)),breaks=c(0,3,6,9,12),labels=c("Q1","Q2","Q3","Q4")))
Cv$Qf = paste0(substr(Cv$Df,3,4), cut(as.integer(substr(Cv$Df,6,7)),breaks=c(0,3,6,9,12),labels=c("Q1","Q2","Q3","Q4")))
dim(Cv);   head(Cv,2)       #-- [1] 7774   12
#    customer         D0         Df DD FF   MM TT   FF0         MM0    BB0   Q0   Qf     BB
# 1:       c1 2015-01-07 2015-01-07  1  1 3335  3 (0,1] (999,1e+04] (-1,0] 15Q1 15Q1 0 days
# 2:      c10 2015-01-28 2015-01-28  1  1 2770  1 (0,1] (999,1e+04] (-1,0] 15Q1 15Q1 0 days

#####===== (F4).(KDD4) 幾種有用的模型(table()) =====#####
table(Cv$FF0,Cv$MM0)  #-- 客戶價值模型
table(Cv$Q0,Cv$Qf)    #-- 客戶進出模型
table(Cv$Qf,Cv$BB0)   #-- 客戶回購/流失模型

#####===== (F5).從數據框到數據檔 (write.csv()) =====#####
dim(Cv);   head(Cv,2)   #-- [1] 7774    13
#    customer         D0         Df DD FF   MM TT   FF0         MM0     BB    BB0   Q0   Qf
# 1:       c1 2015-01-07 2015-01-07  1  1 3335  3 (0,1] (999,1e+04] 0 days (-1,0] 15Q1 15Q1
# 2:      c10 2015-01-28 2015-01-28  1  1 2770  1 (0,1] (999,1e+04] 0 days (-1,0] 15Q1 15Q1
write.csv( Cv, paste0(wkDir,"Cv.csv"), fileEncoding="Big5" )
#-- (A)數據框可以直接寫入.csv試算表檔案(.csv,意指Comma-Separated Values逗號分隔值)中
#-- (B).csv試算表檔案，當然也可以用 X = read.csv("Xnew.csv",fileEncoding="Big5") 讀進來
#-- (C)編碼要設定為 Big-5碼，才能由 EXCEL或 Power-BI 讀入
#-- (D)這是一種簡單的標籤檔(tag)
