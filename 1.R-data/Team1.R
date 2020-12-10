#================KDD1: 數據擷取 read.csv(), dim()================

X = read.csv("googleplaystore2.csv")
X

#================KDD2: 數據探索 Unique(),length(),range(),as.Data()================

dim(X)
head(X)
length(unique(X$Category))  # Category = 34


#================KDD3: 數據轉換 cut() table()================

#X$Category[1:10]  
range(X$Price) 
X$Price0 = cut(X$Price, breaks = c(-0.001,0,9,49,499), labels=c("免費","低價","中價","高價"))
table(X$Price0)
table(X$Category) 

table(X$Category,X$price0)    #Error
#     贈品 數十元品 百元品 千元品
# s1  2115       71   5615  13040
# s2  4475      114  10432  24861
# s3   468       26   1500   3293
# s4   894       24   3368   6158
# s5   691        7   1741   5115
#-- 這也是最基本的多維度分析(Multi-Dimensional Analysis, MDA)

barplot(table(X$price0))
###=== 建議，產生標籤後，就以數據視覺化工具來檢視：如 Tableau, Power BI



#================Test Range================
colnames(X) = c("index","date","Region","aT","eT")
X$aT = as.numeric(X$aT)
TaT = xtavs (aT-Region, data=x)  #aT做加總 Region 做切分


#================KDD4: 數據模型 Table(a,b)================




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
