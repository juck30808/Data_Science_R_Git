# Rdata03A.R: Part A for data report
# Jia-Sheng Heh, 11/15/2020,  revised from Rdata02.R

wkDir = "C:/Users/jsheh/Desktop/working/USC/AIbda/"; setwd(wkDir)
library(data.table)

########## (A).複習: 數據框轉換 (數據轉換的核心) ##########

#####===== (A1).(KDD1-2) 準備數據 (XXX2.csv-->X) =====#####
X = read.csv("XXX2.csv");   dim(X);   head(X,2)   #-- [1] 84008   9
#   invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2        N2      s1       c2      p2    kind1  1400 2015-01-18 19:56:06        1   1197      sub2 793.36

#####===== (A2).(KDD3) SPC模型的數據轉換(X->Cv) =====#####
X$date = as.character(as.Date(X$datetime))
setDT(X, key=c("customer"))
Cv = X[, .(D0=min(date), Df=max(date), DD=length(unique(date)),
             FF=length(unique(invoiceNo)), MM=sum(amount), TT=sum(quantity)), by=customer ] 
dim(Cv);   head(Cv,2)   #-- [1] 7774    7
#    customer         D0         Df DD FF   MM TT
# 1:       c1 2015-01-07 2015-01-07  1  1 3335  3
# 2:      c10 2015-01-28 2015-01-28  1  1 2770  1
#-- D0: 首次來店日, Df: 最近來店日, DD: 來店日數
#-- FF交易次數,     MM: 交易金額,   TT: 購買商品數   

#####===== (A3).(KDD3) 客戶價值標籤/客戶價值模型(Cv$FF0,$MM0) =====#####
range(Cv$FF)   #--     1    11025
range(Cv$MM)   #-- -2400 18251044
Cv$FF0 = cut( Cv$FF, breaks=c(0,1,10,40,200,20000));  table(Cv$FF0)  
# (0,1]      (1,10]     (10,40]    (40,200] (200,2e+04] 
#  4169        3281         275          44           5 
Cv$MM0 = cut( Cv$MM, breaks=c(-5000,0,999,9999,99999,999999,19999999));   table(Cv$MM0)
# (-5e+03,0]       (0,999]   (999,1e+04] (1e+04,1e+05] (1e+05,1e+06] (1e+06,2e+07] 
#         20           937          5587          1193            33             4 
dim(Cv);   head(Cv,2)       #-- [1] 7774    9
#    customer         D0         Df DD FF   MM TT   FF0         MM0
# 1:       c1 2015-01-07 2015-01-07  1  1 3335  3 (0,1] (999,1e+04]
# 2:      c10 2015-01-28 2015-01-28  1  1 2770  1 (0,1] (999,1e+04]

#####===== (A4).(KDD4) RFM客戶價值模型: 客戶分層運營的第一步 =====#####
##== 客戶分層: 區隔不同價值的顧客 [Arthur Hughes, 1994] [Stone, 1989]
#-- (1)顧客最近的消費/購買時間 (Recency,R): 
#      -- 即顧客最近一次購買的時間與現在時間的距離天數
#      -- 用來衡量顧客再次購買的可能性。時間距離愈近則再次購買程度愈高
#-- (2)購買頻率(Frequency,F)
#      -- 在某段期間內購買該企業產品的總次數，此期間可定義為一個月、一季或任何時間長度
#      -- 用來衡量顧客在購買行為中與企業的互動程度，頻率愈高表示顧客的熱衷程度愈高。
#-- (3)購買金額(Monetary,M)
#      -- 在某段期間內購買該企業產品的總金額，
#      -- 用來評價顧客對該企業的貢獻度及顧客價值。金額愈高表示價值較高
addmargins( table(Cv$FF0,Cv$MM0) )  
#             (-5e+03,0] (0,999] (999,1e+04] (1e+04,1e+05] (1e+05,1e+06] (1e+06,2e+07]  Sum
# (0,1]               14     901        3219            35             0             0 4169
# (1,10]               6      36        2368           871             0             0 3281
# (10,40]              0       0           0           272             3             0  275
# (40,200]             0       0           0            15            29             0   44
# (200,2e+04]          0       0           0             0             1             4    5
# Sum                 20     937        5587          1193            33             4 7774

#####===== (A5).(KDD5) 客戶分層的經營解讀 =====#####
#-- (1)一次客4169位，約佔2/3，通常是因為行銷活動前來消費，要進一步分析行銷活動的效益
#      而其中的 萬元客35位，可以進行追蹤
#-- (2)逾200次客1+4位，不可能是正常會員，應是特定時間/地點的銷售，或是非會員，可進行非會員分析
#-- (3)逾10次的272+3+15+29位的萬元客或十萬元客，是常客/常貴客，要有VIP方案，並要進行客戶追蹤及流失客分析
#-- (4)36+2368+871位數次客，是發展中的客戶，要進行回購週期分析與連帶消費/再購分析
#      其中的871位萬元客，要進行追蹤經營

##==> 所以，客戶價值模型，是提供進一步深度分析的分水嶺


########## (B).進一步的設計: 客戶進出模型 ##########

#####===== (B1).(KDD3) 客戶時間標籤的設計(Cv$Q0,$Qf) =====#####
Cv$Q0 = paste0(substr(Cv$D0,3,4), cut(as.integer(substr(Cv$D0,6,7)),breaks=c(0,3,6,9,12),labels=c("Q1","Q2","Q3","Q4")))
Cv$Qf = paste0(substr(Cv$Df,3,4), cut(as.integer(substr(Cv$Df,6,7)),breaks=c(0,3,6,9,12),labels=c("Q1","Q2","Q3","Q4")))

#####===== (B2).(KDD4) 客戶進出模型 =====#####
addmargins( table(Cv$Q0,Cv$Qf) )
#      15Q1 15Q2 15Q3 15Q4 16Q1 16Q2 16Q3 16Q4 17Q1 17Q2 17Q3 17Q4  Sum
# 15Q1  389   65   53   61   78   64   72   61   91  118    0    0 1052
# 15Q2    0  342   53   37   49   39   48   34   47   66    0    0  715
# 15Q3    0    0  271   32   37   36   29   48   27   57    0    0  537
# 15Q4    0    0    0  331   36   37   44   34   34   50    0    0  566 首次來店季
# 16Q1    0    0    0    0  344   39   35   34   59   42    0    0  553 --> 新客
# 16Q2    0    0    0    0    0  418   34   44   44   81    0    0  621 
# 16Q3    0    0    0    0    0    0  342   49   49   66    0    0  506
# 16Q4    0    0    0    0    0    0    0  351   51   73    0    0  475
# 17Q1    0    0    0    0    0    0    0    0  392   83    0    0  475
# 17Q2    0    0    0    0    0    0    0    0    0  369    1    0  370
# 17Q3    0    0    0    0    0    0    0    0    0    0 1007  109 1116
# 17Q4    0    0    0    0    0    0    0    0    0    0    0  788  788
# Sum   389  407  377  461  544  633  604  655  794 1005 1008  897 7774
#                      最後來店季 --> 流失客

#####===== (B3).(KDD5) 客戶進出模型的經營解讀 =====#####
#-- (1) 新客每季約500位左右，至2016Q4開始掉至500位以下，值得注意。
#-- (2) 2016Q2開始，流失客大於新進客，要特別注意，而2017Q2-Q4的流失客均高達千位左右，應做回客動作。
#-- (3) 2017Q3後的的回客只有1位，Q2的新客有1116+788位，需檢討。


########## (C).進一步的設計: 客戶回購/流失模型 ##########

#####===== (C1).(KDD3) 客戶標籤的設計(Cv$FF0,$MM0,$BB0,$Q0,$Qf) =====#####
Cv$BB = (as.Date(Cv$Df)-as.Date(Cv$D0))/Cv$DD  #-- 平均回購週期
Cv$BB0 = cut( as.numeric(Cv$BB), breaks=c(-1,0,7,30,100,999));  table(Cv$BB0)
# (-1,0]     (0,7]    (7,30]  (30,100] (100,999] 
#   4544       482       712      1146       890 

#####===== (C2).(KDD4) 客戶回購/流失模型 =====#####
addmargins( table(Cv$Qf,Cv$BB0) )
#      (-1,0] (0,7] (7,30] (30,100] (100,999]  Sum
# 15Q1    339    22     25        3         0  389
# 15Q2    298    25     49       35         0  407
# 15Q3    243    23     43       63         5  377
# 15Q4    290    25     43       73        30  461
# 16Q1    300    30     45       96        73  544
# 16Q2    366    38     43       94        92  633
# 16Q3    308    23     34      118       121  604
# 16Q4    306    27     49      134       139  655
# 17Q1    333    27     65      178       191  794
# 17Q2    310    37    101      318       239 1005
# 17Q3    843   104     60        1         0 1008
# 17Q4    608   101    155       33         0  897
# Sum    4544   482    712     1146       890 7774

#####===== (C3).(KDD5) 客戶回購/流失模型的經營解讀 =====#####
#-- (1) 2017Q3後出現的客戶中，回購週期在0-7天內的有101+104位，應進一步分析。
#-- (2) 回購週期在7-30天者，有65+101位在2017年下半年尚未出現，應做分析，再做回客動作。
#-- (3) 2017年有239位回購週期在100天以上，值得進行追蹤。

