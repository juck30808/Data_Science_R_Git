

############### (A).數據科學工具平台 ###############
x = 1:10     
x               
plot(x,x*x)     #-- 繪圖

X = 3+2        
Y = X+5
Y

##==> 任何數據分析編程的第一件事：設定工作目錄(Working Directory) 
##--所有R程式的第一行，用來設定工作目錄／數據目錄等
setwd("/Users/juck30808/Google/Course/DataScience_R/USC_R/Rdata01.R")   

#一行中可以有多個指令，以;分隔
X = 3+2;   Y = X+5

############### (B).數據框 到 數據分析程序 ###############

#KDD1: 數據擷取 read.csv(), dim()
#KDD2: 數據探索 Unique(),length(),range(),as.Data()
#KDD3: 數據轉換 cut() table()
#KDD4: 數據模型 Table(a,b)
#KDD5: 數據呈現 Power BI

#####===== (B1).數據擷取 (KDD1, read.csv) =====#####
X = read.csv("XXX2.csv")
X

#####===== (B2).數據框的架構 (KDD2, dim, head): 以下二指令是檢視數據框架構，最基本也最重要的指令 =====#####
rownum = dim(X)[1]     # [1] row / [2]col
colnum = dim(X)[2]
head(X)    # show data

str(X)   #-- data.frame type

#####===== (B3).數據探索 (KDD2, X$欄位, unique, length) =====#####
X$customer     
unique(X$customer)          
length(unique(X$customer))  #有 7774位客戶
table(X$customer)    #-- 客戶分佈品類

sort(table(X$customer))[1:10]   #-- 由小到大
sort(table(X$customer),decreasing=T)[1:10]   #-- 由大到小
sum (sort(table(X$customer),decreasing=T)[1:10] / rownum * 100)  #--- 前 10 大客戶佔了 39.4%
#round(n,2)  #-- round(計算至小數點後)


#####===== (B4).數據框中的元素索引 (X[i,j]) =====#####
X$customer[2]     
X[7,1]            
X[2,]             
X[,2]             
X[2,"customer"]   


#####===== (B5).基本數據轉換與數據探索 (KDD2/KDD3, range, as.--) =====#####
range(X$amount[1:10])   #range() 變量 Min 與 Max
X$datetime[1:5]         
X$date = as.Date(X$datetime,format="%Y-%m-%d")     #as.()轉換函數，字串轉為日期
range(X$date)   #-- [1] "2015-01-01" "2017-12-31"


#####===== (B6).標準數據分析程序  =====#####


#####===== (B7).數據轉換: 連續值切分離散區間 (KDD3, cut) =====##### 
range(X$price)    #-- [1] 0 8280
breaks = c(-0.001,0,99,999,9999) #定義"贈品","數十元品","百元品","千元品" 價值區間
labels = c("贈品","十位","百位","千位") #上述標注標籤
X$price0 = cut( X$price, breaks, labels) 


#####===== (B8).最基本的數據模型 (KDD4, table) =====#####
table(X$price0)  
table(X$channel,X$price0)  

#####===== (B9).數據視覺化 (KDD5, plot--Power BI) =====#####
#建議產生標籤後，就以數據視覺化工具來檢視：如 Tableau, Power BI
par(family="STKaiti");    ##--中文用(?)
barplot(table(X$price0))  ##--長條圖
pie(table(X$channel))     ##--圓餅圖
boxplot(X$price~X$channel, main="price~channel") ##--箱形圖


########## (B).進一步的好用指令 ##########
t = table(X$channel, X$price0)
t
addmargins(t)  #-- 顯示邊際總和 
addmargins( prop.table( t, margin=1 ) )  #-- 邊際百分比(prop.table) 
#margin = Null 直接轉換
#margin = 1 按行求百分比
#margin = 2 按列求百分比


n = addmargins( prop.table( t, margin=1)) * 100  #-- 比例表乘上100比較容易看
round(n,2)  #-- round(計算至小數點後)


range(X$amount)   #-- [1] -22320  37100
which(X$amount>30000)               #-- 第 31728 筆交易紀錄
X$product[ which(X$amount>30000) ]  #-- p3251 產品
X[ which(X$amount>30000), c("customer","quantity","product")]   #--> 第 31728 交易紀錄,c3376客戶/20個/p3251產品


########## (C).數據轉換中的重要訣竅 ##########

install.packages("lubridate")   #安裝package    
library(lubridate) #調用package


#####===== 數據類型轉換(as.-- ) =====#####
head(X$price0,15)
head( as.integer(X$price0), 12 )
head( as.character(X$price0),12 )  

head(X$datetime)
X$date = as.Date( X$datetime, format="%Y-%m-%d" )
head(X$date)

X$weekdays = weekdays(X$date) 
head(X$weekdays)

X$hour = hour(X$datetime);   
head(X$hour)
addmargins( table(X$hour,X$weekdays) )



############### (C).數據轉換 ###############

#####===== (C1/B5).基本數據轉換 (KDD3, as.Date,cut) =====#####
X$date = as.Date(X$datetime,format="%Y-%m-%d")  #-- as.--() 是萬用數據轉換函式，此處是將字串型態 轉為 日期型態
range(X$date)         #-- [1] "2015-01-01" "2017-12-31"
X$price0 = cut( X$price, breaks = c(-0.001,0,99,999,9999), labels=c("贈品","數十元品","百元品","千元品")) 
head(X,2)
#   invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2        N2      s1       c2      p2    kind1  1400 2015-01-18 19:56:06        1   1197      sub2 793.36
#         date      price0
# 1 2015-01-07 (999,1e+04]
# 2 2015-01-18 (999,1e+04]

#####===== (C2).SPC模型的數據轉換(X->Cv) =====#####
library(data.table)
setDT(X, key=c("customer","date"))
Cv = X[, .(D0=min(date), Df=max(date), DD=length(unique(date)),
           FF=length(unique(invoiceNo)), MM=sum(amount), TT=sum(quantity)), by=customer ] 

#####===== (C3).數據框轉換後的數據轉換(Cv$FF0,$MM0,$BB0,$Q0,$Qf) =====#####
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

#####===== (C4).幾種有用的模型 =====#####
table(Cv$FF0,Cv$MM0)  #-- 客戶價值模型
table(Cv$Q0,Cv$Qf)    #-- 客戶進出模型
table(Cv$Qf,Cv$BB0)   #-- 客戶回購/流失模型
