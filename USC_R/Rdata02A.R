# Rdata02A.R: Part A for data transformation
# Jia-Sheng Heh, 11/10/2020,  revised from Rdata01.R

wkDir = "C:/Users/jsheh/Desktop/working/USC/AIbda/";   setwd(wkDir)

########## (A).複習:數據框 到 數據分析程序 ##########

#####===== (A1).標準數據分析程序  =====#####
###=== 知識發現(KDD, Knowledge Discovery in Databases)步驟
##== (KDD1)數據擷取、(KDD2)數據處理、(KDD3)數據轉換、(KDD4)數據模型、(KDD5)數據解讀
##== (KDD2)處理 一般 包括 數據清理、數據整合、數據轉換、數據化約 -- 其中 數據轉換與化約，通常於在 (KDD3)
##== 實務上，將 KDD2 改為 數據探索(Data Exploration)
##== (KDD3)數據轉換，也會和 (KDD2)數據探索 交錯運用，如(B5)

#####===== (A2).數據擷取 (KDD1, read.csv(), dim(), head()) =====#####
X = read.csv("XXX2.csv")
dim(X)     #-- 84008   11 代表84008列11行 (84008筆交易記錄,各有11個欄位), dim 意指 dimension (維度)
head(X,2)  #-- 會顯示前2筆記錄，不寫筆數的 head(X)預設會顯示6筆
#   invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2        N2      s1       c2      p2    kind1  1400 2015-01-18 19:56:06        1   1197      sub2 793.36

#####===== (A3).數據探索 (KDD2, X$欄位, unique(), length(), range(), as.Date()) =====#####
#-- 數據框的欄位，可以錢號$ 來取用，得到的是一個變量 (向量)
length(unique(X$customer))  #-- length(長度): 顯示向量(此處是變量的相異值-unique)的長度   #-- [1] 7774 --> 有 7774位客戶
length(unique(X$product))   #-- [1] 9013 --> 有 9013件商品
range(X$amount)       #-- [1] -22320  37100     #-- range() 可以算出數據變量的範圍(最小值與最大值)
X$datetime[1:5]    #--> 這看起來是日期，其實是以字串(character)型態儲存，並編碼為 83827層級(levels)
# [1] 2015-01-07 20:07:11 2015-01-18 19:56:06 2015-01-14 21:41:51 2015-01-07 19:12:20 2015-01-31 12:40:09
# 83827 Levels: 2015-01-01 11:18:56 2015-01-01 11:44:07 2015-01-01 12:03:51 ... 2017-12-31 21:59:24
X$date = as.Date(X$datetime,format="%Y/%m/%d")  #-- as.--() 是萬用數據轉換函式，此處是將字串型態 轉為 日期型態
range(X$date)         #-- [1] "2015-01-01" "2017-12-31"

#####===== (A4).數據轉換: 連續值切分離散區間 (KDD3, cut(), table()) =====##### 
range(X$price)    #-- [1] 0 8280
X$price0 = cut( X$price, breaks = c(-0.001,0,99,999,9999))
table(X$price0)            #-- (*1) 一個變量的 table() 可以用來看變量的分布(各區間的元素個數)
# (-0.001,0]      (0,99]    (99,999] (999,1e+04] 
#       8643         242       22656       52467 
table(X$price0)
levels(X$price0) = c("贈品","數十元品","百元品","千元品")
table(X$price0)            #-- (*2) 切分後的變量區間，也可以當作一種標籤(tag)
# 贈品 數十元品   百元品   千元品 
# 8643      242    22656    52467 

#####===== (A5).最基本的數據模型 (KDD4, table) =====#####
table(X$channel,X$price0)  #-- (*2) 兩個變量的 table() 可以用來看兩個變量間的關係
#     贈品 數十元品 百元品 千元品
# s1  2115       71   5615  13040
# s2  4475      114  10432  24861
# s3   468       26   1500   3293
# s4   894       24   3368   6158
# s5   691        7   1741   5115
#-- 這也是最基本的多維度分析(Multi-Dimensional Analysis, MDA)

#####===== (A6).數據視覺化 (KDD5, plot--Power BI) =====#####
par(family="STKaiti");    ##--顯示中文用
pie(table(X$channel))
barplot(table(X$channel))
boxplot(X$price~X$channel, main="price~channel")
###=== 這裡的 y~x, 稱之為公式(formula), 相當於 y=f(x)
#          引申: y~x1+x2+..., 相當於 y=f(x1,x2,...)
###=== 建議，產生標籤後，就以數據視覺化工具來檢視：如 Tableau, Power BI


########## (B).進一步的好用指令 ##########

#####===== (B1).(KDD4)兩種進階表格法 (addmargins(), prop.table()) =====#####
addmargins( table(X$channel, X$price0) )  #-- (1) 列總和(rowSums)與行總和(colSums)
#      贈品 數十元品 百元品 千元品   Sum
# s1   2115       71   5615  13040 20841
# s2   4475      114  10432  24861 39882
# s3    468       26   1500   3293  5287
# s4    894       24   3368   6158 10444
# s5    691        7   1741   5115  7554
# Sum  8643      242  22656  52467 84008
addmargins( prop.table( table(X$channel, X$price0), margin=1 ) )  #-- (2) 比例表(prop.table)
#             贈品     數十元品       百元品       千元品          Sum
# s1  0.1014826544 0.0034067463 0.2694208531 0.6256897462 1.0000000000
# s2  0.1122060077 0.0028584324 0.2615716363 0.6233639236 1.0000000000
# s3  0.0885190089 0.0049177227 0.2837147721 0.6228484963 1.0000000000
# s4  0.0855993872 0.0022979701 0.3224818077 0.5896208349 1.0000000000
# s5  0.0914747154 0.0009266614 0.2304739211 0.6771247021 1.0000000000
# Sum 0.4792817736 0.0144075329 1.3676629904 3.1386477031 5.0000000000
round( 100*addmargins( prop.table( table(X$channel, X$price0), margin=1  ), margin = 2 ), 1 )  #-- 比例表乘上100比較容易看
#     贈品 數十元品 百元品 千元品   Sum
# s1  10.1      0.3   26.9   62.6 100.0
# s2  11.2      0.3   26.2   62.3 100.0
# s3   8.9      0.5   28.4   62.3 100.0
# s4   8.6      0.2   32.2   59.0 100.0
# s5   9.1      0.1   23.0   67.7 100.0

#####===== (B2).複習:數據框中的元素索引 (X[i,j]) =====#####
X$customer[2]     #-- c2  (1) 數據向量的元素，可以用 方括號[k] 來取用第k個元素
X[2,3]            #-- c2  (2) X[2,3] 是取用X數據框中，第2筆記錄(2...)的第3個欄位("customer")的值(c2)
X[2,]             #--     (3) 行(欄位)不指定時，就是所有行(欄位)
#   invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 2        N2      s1       c2      p2    kind1  1400 2015-01-18 19:56:06        1   1197      sub2 793.36
X[,3]             #--     (4) 列(記錄)不指定時，就是所有列(記錄)
# [1] c1    c2    c3    c4    c5    c6    c7    c8    c9    c10   c11   c12   c13   c14   c15   c16   c17  
# [18] c18   c19   c20   c21   c22   c23   c24   c25   c26   c27   c28   c29   c30   c31   c32   c33   c34  
# ...
X[2,"customer"]   #-- c2  (5) 也可以直接用欄位名稱，當作文字索引，來取用元素 
X[,"customer"]    #--     (6) 也可配合欄位名稱，所以效果就等於 X$customer

#####===== (B3).條件式取用元素 (which) =====#####
range(X$amount)   #-- [1] -22320  37100
which(X$amount>30000)               #-- [1] 31728 --> 第 31728 筆交易紀錄, 銷售金額大於 30000 元 --> 買什麼東西啊?
X$product[ which(X$amount>30000) ]  #--  "p3251" --> "p3251"產品
X[ which(X$amount>30000), ]         #--> 調出最高成交價(amount)的交易記錄
#       invoiceNo channel customer product category price       datetime quantity amount category2    cost       date price0
# 31728    N14550      s4    c3376   p3251   kind11  2650 2016/4/5 21:01       20  37100      sub1 1413.78 2016-04-05 千元品
X[ which(X$amount>30000), c("customer","quantity","product")]   #--> 是 c3376客戶 買了 20個 p3251產品
#       customer quantity product
# 31728    c3376       20   p3251
table(X[which(X$amount<0), c("channel","price0")])
#         price0
# channel 贈品 數十元品 百元品 千元品
#      s1    0        1    102    446
#      s2    0        2    231    934
#      s3    0        0     24    117
#      s4    0        1    149    341
#      s5    0        0     40    244
X[which((X$amount<0)&(X$price0=="數十元品")), ]
#       invoiceNo channel customer product category price        datetime quantity amount category2 cost
# 60719    N27797      s1    c5871   p7389   kind12    10 2017/7/23 16:28       -2    -20      sub5    0
# 67567    N30990      s2    c5871   p7144   kind12    10 2017/8/30 20:24       -2    -20      sub5    0
# 67619    N30990      s2    c5871   p7389   kind12    10 2017/8/30 20:12       -2    -20      sub5    0
# 67670    N31030      s4    c5871   p7389   kind12    10 2017/8/31 19:36       -1    -10      sub5    0
#             date   price0                time weekdays hour
# 60719 2017-07-23 數十元品 2017-07-23 16:28:00   星期日   16
# 67567 2017-08-30 數十元品 2017-08-30 20:24:00   星期三   20
# 67619 2017-08-30 數十元品 2017-08-30 20:12:00   星期三   20
# 67670 2017-08-31 數十元品 2017-08-31 19:36:00   星期四   19

#####===== (B4).表格中元素的排序 (table(),sort(),order()) =====#####
table(X$category)    #-- 品類太多了....
# kind1 kind10 kind11 kind12 kind13 kind14 kind15 kind16 kind17 kind18 kind19  kind2 kind20 kind21 kind22 kind23 kind24 
# 10789    323   3107   8407    115     94    110   3424   3274    699    312  31919    222    161      6     38    543 
# kind25 kind26 kind27 kind28 kind29  kind3 kind30 kind31 kind32 kind33 kind34 kind35 kind36 kind37 kind38 kind39  kind4 
#     83     82    476      3    123   9320    256     11     10    364    102      2    417     23     44      3    336 
# kind40 kind41 kind42 kind43 kind44 kind45 kind46 kind47 kind48 kind49  kind5 kind50 kind51 kind52 kind53 kind54 kind55 
#    115    557      5      5    164      1      2      4    267    350    549    269     10      4     85      5    288 
# kind56 kind57 kind58 kind59  kind6 kind60 kind61 kind62 kind63  kind7  kind8  kind9 
#   2580   1649     68    132   1449    131     81      3      2      7     21      7
sort(table(X$category),decreasing=T)[1:10]                  #-- 由數量大到數量小,比較容易抓出重點(前20項)
# kind2  kind1  kind3 kind12 kind16 kind17 kind11 kind56 kind57  kind6 
# 31919  10789   9320   8407   3424   3274   3107   2580   1649   1449
TT = table(X$category);  TT[order(TT,decreasing=T)][1:10]   #-- 另一種寫法
# kind2  kind1  kind3 kind12 kind16 kind17 kind11 kind56 kind57  kind6 
# 31919  10789   9320   8407   3424   3274   3107   2580   1649   1449
sum(round( 100*sort(table(X$category),decreasing=T)[1:20]/dim(X)[1], 2 ))  #-- 前10項就佔90%以上了
# kind2  kind1  kind3 kind12 kind16 kind17 kind11 kind56 kind57  kind6
# 38.00  12.84  11.09  10.01   4.08   3.90   3.70   3.07   1.96   1.72
TT = table(X$channel,X$category);  addmargins(TT[order(rowSums(TT),decreasing=T), order(colSums(TT),decreasing=T)][,1:10])
#     kind2 kind1 kind3 kind12 kind16 kind17 kind11 kind56 kind57 kind6   Sum
# s2  20252     0  6809   4347   2226    459   1545     65      0   741 36444
# s1   5735  6612  1289   2102      0      0    791      0   1649   358 18536
# s4   4425   338  1135    815   1198    196    487     95      0   221  8910
# s5   1464     0     0    657      0   2591    271   2420      0     7  7410
# s3     43  3839    87    486      0     28     13      0      0   122  4618
# Sum 31919 10789  9320   8407   3424   3274   3107   2580   1649  1449 75918


########## (C).數據轉換中的重要訣竅 ##########

#####===== (C1).六種常用數據類型 (尺度 Scale) =====#####
# [簡禎富、許嘉裕, 資料挖礦與大數據分析, 前程, 2014]
# 1.名目尺度(nominal scale) 
# -- 所衡量的數字僅是作為代碼來確認方案，數字的大小不具任何意義，也不能做數學運算。
# -- 如學號和身分證號。
# 2.類別尺度(categorical scale)" = <>
# -- 將欲評估的方案，依其特徵分類，再將每一個類別標示一個數字僅是用來表示其歸屬的類別，
# -- 因此類別尺度的數據可以重複。其轉換方式必須是一對一轉換的函數。
# 3.順序尺度(ordinal scale): > <
# -- 所衡量的數字表示方案間的大小順序關係。其轉換必須保持其數字上的大小順序關係，
# -- 因此必須以嚴格的遞增函數來作轉換。
# 4.間距尺度(interval scale)，又稱 距離尺度(distance scale): + -
# -- 所衡量的數字可有意義地描述並比較數字之間的差距大小。
# -- 其轉換必須保持其數字之間的間距大小關係，因此必須是線性函數。
# 5.比率尺度(ratio scale): * /
# -- 所衡量的數字之間，可以做比率倍數之間的比較。
# -- 其轉換必須是倍數關係。
# 6.絕對尺度(absolute scale)
# -- 所衡量的數字具有絕對的意義，無法再做其他任何轉換，
# -- 如機率值。 

#####===== (C2).數據框中的數據類型 =====#####
head(X,2)
#   invoiceNo channel customer product category price        datetime quantity amount category2   cost       date price0
# 1        N1      s1       c1      p1    kind1  1980  2015/1/7 20:07        1   1692      sub1 931.39 2015-01-07 千元品
# 2        N2      s1       c2      p2    kind1  1400 2015/1/18 19:56        1   1197      sub2 793.36 2015-01-18 千元品
str(X)   #-- structure (str(): 不常用，上課講解用，不列入常用指令) 
# 'data.frame':	84008 obs. of  13 variables:
# $ invoiceNo: chr  "N1" "N2" "N3" "N4" ...
# $ channel  : chr  "s1" "s1" "s1" "s1" ...
# $ customer : chr  "c1" "c2" "c3" "c4" ...
# $ product  : chr  "p1" "p2" "p3" "p3" ...
# $ category : chr  "kind1" "kind1" "kind1" "kind1" ...
# $ price    : int  1980 1400 1600 1600 1600 1400 1600 1600 3250 400 ...
# $ datetime : chr  "2015/1/7 20:07" "2015/1/18 19:56" "2015/1/14 21:41" "2015/1/7 19:12" ...
# $ quantity : int  1 1 1 1 1 1 1 1 1 1 ...
# $ amount   : int  1692 1197 1368 1360 1368 1197 1216 1360 2776 342 ...
# $ category2: chr  "sub1" "sub2" "sub1" "sub1" ...
# $ cost     : num  931 793 847 847 847 ...
# $ date     : Date, format: "2015-01-07" "2015-01-18" "2015-01-14" "2015-01-07" ...
# $ price0   : Factor w/ 4 levels "贈品","數十元品",..: 4 4 4 4 4 4 4 4 4 3 ...
##-- (A) 字元(character)類型: 如invoiceNo, channel, customer, product, category, category2
##-- (B) 數值(numeric,integer整數)類型: 如price, quantity, amount, cost
##-- (C) 日期(Date)類型: 如 datetime (類型chr) --> date (類型為Date, 因為經過as.Date()轉換)
##-- (D) 邏輯(logical)類型: 如(B3)中 which()中的值

#####===== (C3).因子(factor)與數據類型轉換(as.-- ) =====#####
head(X$price0,15)
# [1] 千元品 千元品 千元品 千元品 千元品 千元品 千元品 千元品 千元品 百元品 千元品 千元品 千元品 百元品 千元品
# Levels: 贈品 數十元品 百元品 千元品
head( as.integer(X$price0), 12 )
# [1] 4 4 4 4 4 4 4 4 4 3 4 4 4 3 4 4
head( as.character(X$price0),12 )   #-- 可以將任何數據類型，轉為字元類型
# [1] "千元品" "千元品" "千元品" "千元品" "千元品" "千元品" "千元品" "千元品" "千元品" "百元品" "千元品" "千元品"

#####===== (C4).日期數據類型轉換(as.Date(), strptime()) =====#####
head(X$datetime)
# [1] "2015/1/7 20:07"  "2015/1/18 19:56" "2015/1/14 21:41" "2015/1/7 19:12"  "2015/1/31 12:40" "2015/1/14 21:43"
as.Date( head(X$datetime) )    #-- 直接轉日期,不設格式,很常會判斷出錯(這次是對的)
# [1] "2015-01-07" "2015-01-18" "2015-01-14" "2015-01-07" "2015-01-31" "2015-01-14"
X$date = as.Date( X$datetime, format="%Y-%m-%d" );  head(X$date)           #-- 格式錯誤,也轉換不出來
# [1] NA NA NA NA NA NA
X$date = as.Date( X$datetime, format="%Y/%m/%d" );  head(X$date)           #-- 加上格式的日期轉換
# [1] "2015-01-07" "2015-01-18" "2015-01-14" "2015-01-07" "2015-01-31" "2015-01-14"
X$time = strptime( X$datetime, format="%Y/%m/%d %H:%M" );  head(X$time,5)  #-- 提取時間格式的轉換: CST (中原標準時間,Central Standard Time)
#  "2015-01-07 20:07:00 CST" "2015-01-18 19:56:00 CST" "2015-01-14 21:41:00 CST" "2015-01-07 19:12:00 CST" "2015-01-31 12:40:00 CST"
#-- 有時會有 UTC (Coordinated Universal Time):  CST = UTC + 8:00

#####===== (C5) R的軟件包(package)使用 =====#####
#== (1) R的綜合典藏網(CRAN,Comprehensive R Archive Network)
#       共有6400(2015)/8000(2016)/10000(2017)/13437(2018) 個軟件包(packages)
nrow(available.packages())  #-- [1] 16495/16235  --> 可用來看目前網絡上軟件包的數量
#== (2) 安裝軟件包，只需執行一次，會從網路上下載安裝軟件包, 進入電腦的硬盤
# install.packages("lubridate")     
#== (3) 當安裝好後，就可以隨時在應用前,以 library() 調用此軟件包
library(lubridate)

#####===== (C6).豐富的日期變量(weekdays()與hour()不用記憶，只要在需用時google即可查得 ) =====#####
X$weekdays = weekdays(X$date);   head(X$weekdays)   #-- [1] "星期三" "星期日" "星期三" "星期三" "星期六" "星期三"
X$hour = hour(X$time);   head(X$hour)               #-- [1] 20 19 21 19 12 21
addmargins( table(X$hour,X$weekdays) )
#     星期一 星期二 星期三 星期五 星期六 星期日 星期四   Sum ---> 每週時交易商品分布表
# 0        0      2      0      0      0      0      0     2
# 10       5      2      5      2      1      4      2    21
# 11     175    137    163    135    227    338    148  1323
# 12     634    569    490    649    920    882    443  4587
# 13     712    679    669    760   1113   1358    693  5984
# 14     849    659    704    849   1350   1608    684  6703
# 15     736    726    714    951   1288   1619    763  6797
# 16     869    813    785   1018   1376   1595    863  7319
# 17     937    972    949   1064   1396   1582    870  7770
# 18     991    966    930   1081   1318   1468    852  7606
# 19    1242   1049   1111   1110   1649   1767   1037  8965
# 20    1602   1600   1610   1805   1928   2276   1567 12388
# 21    1660   1592   1393   1699   2087   2078   1380 11889
# 22     321    274    383    489    466    356    348  2637
# 23       7      6      1      0      1      2      0    17
# Sum  10740  10046   9907  11612  15120  16933   9650 84008

