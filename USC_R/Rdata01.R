# Rdata01.R: Learning Version (version A) for R platform (RStudio), R dataframe, BDA steps and Customer Clusters
# Jia-Sheng Heh, 11/14/2020,  revised from RDIT01.R

############### (A).數據科學工具平台 ###############

#####===== (A1).整合開發分析平台 (RStudio)--分成四區: =====#####
#-- (A)程式編輯區 (左上區) 
#-- (B)指令執行區(左下區): 請用Shift-Dn指令標記7-9行後，用Ctrl-Enter將這三行送到指令執行區執行
x = 1:10     
x               #-- (C)變量顯示區(右上角): 可以看到 x 變量:  #-- [1]  1  2  3  4  5  6  7  8  9 10
plot(x,x*x)     #-- (D)繪圖與求助區(右下角): 可以看到 x 對 x*x 的繪圖

#####===== (A2).數據分析程式語言 (R)  =====#####
3+2             
X = 3+2        
X   
Y = X+5
Y   

#####===== (A3).RStudio開啟程式檔 =====#####
# 開啟檔案時，如出現亂碼，可由『File-->Reopen with Encoding』選取適當的程式編碼 (通常為UTF8或Big5) 即可觀看
# 可以同時開啟多個程式檔
# 修改後，程式名稱會顯示紅色
# 在其編輯視窗下，按『存檔』後，程式名稱會恢復黑色

#####===== (A4).R程式的工作目錄 =====#####
##==> 任何數據分析編程的第一件事：設定工作目錄(Working Directory) 
setwd("/Users/juck30808/Google/Course/DataScience_R/USC_R/Rdata01.R")   ##--所有R程式的第一行，用來設定工作目錄／數據目錄等
#>> [實作演練] 花點時間，修改上述目錄名稱為，你儲存本程式(RDIT01.R)的目錄
# 注意: Windows的路徑，複製過來時，會是反斜線(backslash,\)，要改成正斜線(slash,/)

#####===== (A5).R 程式 注意事項 =====#####
#== (1)RStudio環境中，可以同時開啟多個程式
# -- 每個程式，隨時保持儲存
# -- 讓每個程式，只要從第一行開始重新執行，即可獲得同樣的結果
# -- 每一計畫 (project) 存於一目錄
#== (2) 井字號# 代表註解(comment): 其後的指令(敘述)不執行，可分為檔首註解，段落註解，行尾註解
# -- 編程檔 (.R) 的註解：有三種--
#   -- (A) 檔首註解：註明整個程式的 (1)程式名稱、(2)程式目的、(*)調用方式、(3)作者，(4)日期
#   -- (B) 分段註解：指示數據分析的步驟(請參考數據分析步驟說明) ，五個井號(#####)可有段落收縮
#   -- (C) 行尾註解：在適當的地方，註解此行說明，或 執行結果 (通常 加 #-- 識別)
#== (3) 在R中，大小寫是不同的，試輸入 x 與 X，觀察他們的值
x   #-- [1]  1  2  3  4  5  6  7  8  9 10
X   #-- [1]  5
#== (4) 一行中可以有多個指令，以分號;分隔，如 第15,17行可以寫成
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
dim(X)     #dimension (維度)84008列11行 (84008筆交易記錄,各有11個欄位)
head(X,2)  #會顯示前2筆記錄


#####===== (B3).數據探索 (KDD2, X$欄位, unique, length) =====#####
X$customer     #-- 數據框的欄位，可以錢號$ 來取用，得到的是一個變量 (向量)
unique(X$customer)          #-- unique(欄位): 用來找出該欄位的種類   
length(unique(X$customer))  #-- length(長度): 顯示向量(此處是變量的相異值-unique)的長度   #-- [1] 7774 --> 有 7774位客戶
length(unique(X$product))   #-- 有 9013件商品


#####===== (B4).數據框中的元素索引 (X[i,j]) =====#####
X$customer[2]     #(1) 取用第k個元素
X[2,3]            #(2) X[2,3] 是取用X數據框中(c2)
X[2,]             #(3) 不指定時，就是所有行
X[,3]             #(4) 不指定時，就是所有列
X[2,"customer"]   #(5) 也可以直接用欄位名稱，當作文字索引，來取用元素 
X[,"customer"]    #(6) 也可配合欄位名稱，所以效果就等於 X$customer


#####===== (B5).基本數據轉換與數據探索 (KDD2/KDD3, range, as.--) =====#####
X$amount[1:10]    #amount 表頭 1:10
range(X$amount)   #range() 變量的範圍(最小值與最大值)
X$datetime[1:5]   #以字串(character)型態儲存，並編碼為 83827層級(levels)
X$date = as.Date(X$datetime,format="%Y-%m-%d")  #-- as.--() 是萬用數據轉換函式，此處是將字串型態 轉為 日期型態
range(X$date)         #-- [1] "2015-01-01" "2017-12-31"


#####===== (B6).標準數據分析程序  =====#####
###=== 知識發現(KDD, Knowledge Discovery in Databases)步驟
##== (KDD1)數據擷取、(KDD2)數據處理、(KDD3)數據轉換、(KDD4)數據模型、(KDD5)數據解讀
##== (KDD2)處理 一般 包括 數據清理、數據整合、數據轉換、數據化約 -- 其中 數據轉換與化約，通常於在 (KDD3)
##== 實務上，將 KDD2 改為 數據探索(Data Exploration)
##== (KDD3)數據轉換，也會和 (KDD2)數據探索 交錯運用，如(B5)


#####===== (B7).數據轉換: 連續值切分離散區間 (KDD3, cut) =====##### 
range(X$price)    #-- [1] 0 8280
breaks = c(-0.001,0,99,999,9999) #定義"贈品","數十元品","百元品","千元品" 價值區間
labels = c("贈品","數十元品","百元品","千元品") #上述標注標籤
X$price0 = cut( X$price, breaks, labels) 
table(X$price0)   #用來看變量的分布(各區間的元素個數)


#####===== (B8).最基本的數據模型 (KDD4, table) =====#####
table(X$channel,X$price0)  #-- (*2) 兩個變量的 table() 可以用來看兩個變量間的關係
#     贈品 數十元品 百元品 千元品
# s1  2115       71   5615  13040
# s2  4475      114  10432  24861
# s3   468       26   1500   3293
# s4   894       24   3368   6158
# s5   691        7   1741   5115
#-- 這也是最基本的多維度分析(Multi-Dimensional Analysis, MDA)

#Jerry
#Linear Scale 線性變化 / Log Scale 指數變化
#三個指數定律
#Moore's Law 摩爾定律 : Processing power double every 18 mon
#Butter's Law 巴特斯定律: Communication speed doubles every 9 mon
#Kryder's Law 凱德爾定律: Storage Capacity doubles every 13 mon

#Solow Computer Paradox 所洛計算機悖論





#####===== (B9).數據視覺化 (KDD5, plot--Power BI) =====#####
barplot(table(X$price0))
###=== 建議，產生標籤後，就以數據視覺化工具來檢視：如 Tableau, Power BI

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
