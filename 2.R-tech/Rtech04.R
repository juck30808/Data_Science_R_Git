# Rtech04.R: 大數據實務技術 - 04: 數據關連與數據挖掘
# Jia-Sheng Heh (賀嘉生), 09/24/2020, revised from HUT06.R

setwd("c:/Users/Mike/Desktop/working/USC/AIbda/")

########## (P) 課前準備 ########## 
##== (1) 從微信群組下載 本份講義程式檔 Rtech04.R 放入本門課課程目錄，並以 RStudio開啟
##== (2) 若見到本編程檔為中文亂碼，請以 File-->Reopen with Encoding --> UTF8，則可看到中文碼顯示
##== (3) 修改本程式第4行，設定工作目錄為 本門課工作目錄
##== (4) 下載本門課所需之軟件包至本機備用 -- 下行安裝指令只需執行一次
# install.packages( c("arules","arulesViz","igraph","data.table","jiebaR","text2vec") )    
##== (5) 從微信群組下載 RR1_500r6810.csv, RR501_1000r6217.csv, RR1001_1500r7707.csv, 
#                       RR"1501_2000r8262.csv 數據檔，放入 本門課工作目錄，作為本課程待用


########## (1) 數據挖掘 ##########

#####===== (1A) 數據挖掘(data mining)的魅力 [百度] =====#####
##== 維克多.邁爾-舍恩伯格(Viktor Mayer-Schönberger): 別問為什麼，知道是什麼就夠了
#    -- 牛津大学网络学院互联网治理与监管专业教授, 最早洞见大数据时代发展趋势的数据科学家
#    -- [大数据时代, 2012] 大数据时代最大的转变就是，放弃对因果关系的渴求，而取而代之关注相关关系
#    -- [删除, 2013] 大数据取舍之道，就是把有意义的留下来，把无意义的去掉

#####=====*(1B) 數據模型(Data Model) =====#####
##== 系統/模型(System/Model, M):  輸出y = M( 輸入u )
#    -- (1) 訓練階段(Training/Learning/Modeling/Estimation Phase): (u, y) -> M
#             由輸入/輸出 u與y，求取(估測estimate)模型M#
#    -- (2) 預測階段(Prediction/Estimation/Production/Application Phase): (u_new, M#) -> y_predict
#             以所估測的模型M#與新的輸入 u_new，求取(估測)新的輸出 y_predict
##== 機器學習(Machine Learning)
#    -- (1) 監督式學習 (Supervised learning): 具範例(u,y), y為教師(teacher, desired output) y, 以求得y=M(u)
#           (1A)迴歸(regression):     y 為連續數據   --> 下一單元 Rtech05
#           (1B)分類(classification): y 為離散數據   --> 下二單元 Rtech06 
#    -- (2) 無監督式學習 (Unsupervised learning): 無輸出y, 目標在於發掘輸入(u)的隱含特徵 --> 數據挖掘(Data Mining)
#           (2A)聚類(clustering):           計算數據u的相似度，以產生其分類。 --> 上一單元Rtech03
#           (2B)關聯規則(association rule): 計算多數據(ui-uj)間的關連。       --> 本單元  Rtech04
#           (2C)數據序列(data sequencing):  計算多數據(ui-uj)間的時序關係。   --> 未列入本課程


########## (2) 事務/交易數據 [殷,5.1-5.2] ##########

#####===== (2A) 關聯分析(association analysis) [殷,5.1] =====#####
##== 關聯分析: 發現隱藏在大型數據集中的令人感興趣的聯繫
#              -- 關聯規則(association rule): 所發現的模式 if A1 and A2... then B
#              -- 關聯分析算法: 包括 Apriori算法, FP-growth算法
#              -- 其餘算法: 處理分類屬性, 處理連續屬性, 處理概念分層, 序列模式, 子圖模式, 非頻繁模式等 (均不介紹)
library(arules)

#####===== (2B) 事務/交易數據(transactions) [殷,5.2.1] =====#####
data(Groceries)
##== 購物籃(basket)事務/交易(transaction)
#    -- 事務/交易數據集: 所有的購物記錄, 即所有的事務/交易  D = {Ti} = {T1,T2,...,TN}
#    -- 事務/交易(transaction)Ti: 每一條購物記錄所購買的物品
#    -- 每個事務/交易是項(item)的集合 Ti included in I = { 1,2,...,im }, 每個事務/交易有一個標識符(TID)
inspect(Groceries[1:10]) 
#     items                                                   
# [1] {citrus fruit, semi-finished bread, margarine,ready soups} --- TID = 1
# [2] {tropical fruit, yogurt,coffee}                            --- TID = 2
# [3] {whole milk}                                               --- TID = 3
Groceries   #-- 9835條購物記錄(transaction事務/交易), 共有169個物品
# transactions in sparse format with
# 9835 transactions (rows) and  --- N = 9835 事務/交易數
# 169 items (columns)           --- m = 169  項/物品數

#####=====*(2C) 項集(item sets)及格結構 [殷,5.2.1] =====#####
##== 項集(item set): 包含0個或多個項的集合
#    -- 1-項集: 包括1項的項集, 如: {whole milk}, {rolls/buns}
#    -- 2-項集: 包括2項的項集, 如: {tropical fruit, yogurt,coffee}, {whole milk, cereals}
#    ...
##== 格結構(lattice structure): 列舉所有可能的項集(item sets)
#    -- k個項的數據集，最多產生 2^k - 1 個項集，不含空集合{}
#    -- 如: I = {A,B,C} --> 項集可為 {},{A},{B},{C},{A,B},{A,C},{B,C},{A,B,C} 可形成一個偏序的(partial ordered)的格結構


########## (3) 頻繁項集(frequent item set, FIS) [殷,5.2] ##########

#####=====*(3A) 頻繁項集(frequent item set, FIS) [殷,5.2.2] =====#####
##== 支持度(support): 一個項集的 出現次數 與 數據集中所有事務/交易數的百分比
##== 頻繁項集(frequent item set, FIS): 一個項集的支持度大於或等於某個閾值(minimal support)
##== ECLAT (Equivalence Class Clustering and bottom-up Lattice Traversal) 演算法
#    -- 挖掘關聯規則的頻繁項集
#    -- 深度優先(depth-first)演算法，比 Apriori演算法(寬度優先breadth-first)有效率
fisG = eclat(Groceries, parameter=list(support=0.05,maxlen=5));   length(fisG);   inspect(fisG)   #-- [1] 31
#      items                         support    count
# [1]  {whole milk,yogurt}           0.05602440  551 
# [2]  {whole milk,rolls/buns}       0.05663447  557 
# [3]  {other vegetables,whole milk} 0.07483477  736 ...
# [4]  {whole milk}                  0.25551601 2513 
# [5]  {other vegetables}            0.19349263 1903 
# [6]  {rolls/buns}                  0.18393493 1809 
# [7]  {yogurt}                      0.13950178 1372 
# ...
# [30] {coffee}                      0.05805796  571 = 9835 * 0.05602440 
# [31] {canned beer}                 0.07768175  764 = 9835 * 0.07768175

#####===== (3B) 格結構(lattice structure) [殷,5.2.2] =====#####
##== 格結構(lattice structure)
#      {whole milk,yogurt}: 0.05602440 ----- {whole milk}: 0.25551601
#                                      ----- {yogurt}: 0.13950178
#      {whole milk,rolls/buns}: 0.05663447   ----- {whole milk}: 0.25551601
#                                            ----- {rolls/buns}: 0.18393493
#      {other vegetables,whole milk}: 0.07483477 ----- {whole milk}: 0.25551601
#                                                ----- {other vegetables}: 0.19349263
##== 超集{F1} > 子集{F2}, 則 support(F1) <= support(F2)
#    -- 如: F1 = {whole milk,yogurt} > F2 = {whole milk}: 
#           則 support({whole milk,yogurt}) = 0.05602440 < support({whole milk}) = 0.25551601

#####===== (3C) 一個小的事務/交易數據集例 [殷p.110-111] =====#####
##== 事務/交易數據集: D = { 1={ABC}, 2={ABCD}, 3={BCE}, 4={ACDE}, 5={DE} }
#    -- 事務/交易的數據，是以 list() 的型式儲存，這樣每個事務/交易中的項目數可以不同
D = list( c("A","B","C"), c("A","B","C","D"), c("B","C","E"), c("A","C","D","E"), c("D","E") )
##== 格結構(FIS):  null ----- A (124)  --^-- AB (12)  ----- ABC (12) ----- ABCD (2) ----- ABCDE (x)
#                                                     ----- ABD (2)         
#                                      ----- AC (124) --^-- ACD (24) ----- ACDE (4) 
#                                                     --^-- ACE (4)
#                                      --^-- AD (24)  ----- ADE (4)
#                                      --^-- AE (4)
#                       ----- B (123)  ----- BC (123) --^-- BCD (2)
#                                                     --^-- BCE (3)
#                                      --^-- BD (2)
#                       ----- C (1234) --^-- CD (24)
#                                      --^-- CE (34)
#                       ----- D (245)  --^-- DE (45)
#                       ----- E (345)  
##== 頻繁項集(frequent item set, FIS)
fisD06 = eclat(D, parameter=list(support=0.6,maxlen=3));   inspect(fisD06)
#     items support count
# [1] {B,C} 0.6     3  ---> maxFIS for support=0.6    
# [2] {A,C} 0.6     3  ---> maxFIS for support=0.6   
# [3] {C}   0.8     4  ---> maxFIS for support=0.6 
# [4] {A}   0.6     3    
# [5] {B}   0.6     3    
# [6] {D}   0.6     3  ---> maxFIS for support=0.6   
# [7] {E}   0.6     3  ---> maxFIS for support=0.6  

#####===== (3D) 最大頻繁項集與閉頻繁項集 [殷p.110-111] =====#####
##== 最大頻繁項集(Maximal Frequent Item Sets, maxFIS): 具有最大項目集的頻繁項集(FIS)　
#    -- maxFIS的直接超集 都不是頻繁的
#    -- 如: AC(124), BC(123), C(1234), D(245), E(345) ---> 取 support=0.6 的最大頻繁項集
#    -- 閉項集(closed item set): 它的直接超集都不具有和它相同的支持度計數
##== 閉頻繁項集: 一個頻繁項集滿足閉項集
#    -- 如: AC(124), BC (123), C(1234), D(245), E(345) + CE(34), DE(34), ABC(12), ACD(24) ---> 這是取 support=0.4 的最大頻繁項集
#                                                                                              (有問題?? 應該不是閉頻繁項集)
#    -- 如: AC(124), BC (123), C(1234), D(245), E(345) + CE, DE, CD, ABC, ACD, ACE, BCD, BCE ---> 滿足封閉性的項集
fisD04 = eclat(D, parameter=list(support=0.4,maxlen=3));   inspect(fisD04)
#      items   support count
# [1]  {C,E}   0.4     2  ---> maxFIS for support=0.4  
# [2]  {D,E}   0.4     2  ---> maxFIS for support=0.4  
# [3]  {A,C,D} 0.4     2  ---> maxFIS for support=0.4  
# [4]  {C,D}   0.4     2    
# [5]  {A,D}   0.4     2    
# [6]  {A,B,C} 0.4     2  ---> maxFIS for support=0.4  
# [7]  {B,C}   0.6     3  ---> maxFIS for support=0.6
# [8]  {A,B}   0.4     2    
# [9]  {A,C}   0.6     3  ---> maxFIS for support=0.6  
# [10] {C}     0.8     4  ---> maxFIS for support=0.6  
# [11] {A}     0.6     3    
# [12] {B}     0.6     3    
# [13] {D}     0.6     3  ---> maxFIS for support=0.6  
# [14] {E}     0.6     3  ---> maxFIS for support=0.6  


########## (4) 關聯規則(association rule) [殷,5.2.3-5.2.5] ##########

#####===== (4A) 關聯規則(association rule) [殷,5.2.3] =====#####
##== 關聯規則(association rule), X->Y: 蘊含表達式，表示數據內隱含的關聯性
#    -- X included in I, Y included in I, X intersect Y = {}
#    -- X為前件/先決條件(condition, lhs, left-hand side)，Y為相應的後件/關聯結果(conclusion, rhs, right-hand side)
##== Groceries的關聯規則說明例
arG = apriori( Groceries, parameter=list(support=0.05, confidence=0.2), control=list(verbose=FALSE));  
length(arG);  inspect(arG)    #-- [1] 7
#     lhs                   rhs                support    confidence lift     count
# [1] {}                 => {whole milk}       0.25551601 0.2555160  1.000000 2513 
# [2] {yogurt}           => {whole milk}       0.05602440 0.4016035  1.571735  551 
# [3] {whole milk}       => {yogurt}           0.05602440 0.2192598  1.571735  551 
# [4] {rolls/buns}       => {whole milk}       0.05663447 0.3079049  1.205032  557 
# [5] {whole milk}       => {rolls/buns}       0.05663447 0.2216474  1.205032  557 
# [6] {other vegetables} => {whole milk}       0.07483477 0.3867578  1.513634  736 
# [7] {whole milk}       => {other vegetables} 0.07483477 0.2928770  1.513634  736 

#####=====*(4B) 關聯規則的主要參數(association rule) [殷,5.2.3] =====#####
##== 主要參數定義
#    -- (1) 支持度(support): supp(X->Y) = P(X且Y)。                最小支持度(minsupp)：support(Z)>=minsupp。    
#    -- (2) 置信度(confidence): conf(X->Y) = P(Y|X)=P(X,Y)/P(X),   最小置信度(minconf)。
#    -- (3) 提升度(lift): lift(X->Y) = P(Y|X)/P(Y) = conf(X->Y)/P(Y)。 lift越大(>1): 表示X對Y的提升作用越大
##== 舉例說明參數
#    -- 例如：規則3 {whole milk}=>{yogurt} (support=0.05602440, confidence=0.2192598, lift=1.571735, count=551)
#    -- 支持數count=551  表示 全部 9835個事務/交易中，有 551位客戶符合本規則
#    -- 支持度support(X->Y) = P(X且Y) = 0.0560244 = 551 / 9835 為全部事務/交易中，本規則的支持比例
#    -- 置信度confidence(X->Y) = P(Y|X)=P(X,Y)/P(X) = 0.2192598 = 551/2513 符合本規則前提的2513位客戶中，有551位使得本規則成立
#    -- 提升度lift(X->Y) = P(Y|X)/P(Y) = conf(X->Y)/P(Y) = 1.571735 = 0.2192598/0.1395018: 
#       -- P(B) = 0.1395018 = 1372/9835 = P({yogurt}) 原本符合 B 的個數有1372位，佔全部比0.1395018
#       -- 加上{whole milk}前提後，佔前提比為0.2192598，提升了 1.571735，也就是在前提的數據集合中，結論較明顯(提高為1.57倍)

#####===== (4C) 關聯規則挖掘算法 [殷,5.2.3-5.3] =====#####
##== 關聯規則挖掘算法的基本步驟 [殷,5.2.3]
#    -- (1) 產生頻繁項集: 發現滿足最小支持度閾值的所有項集(即 頻繁項集)
#    -- (2) 產生規則: 從上一步發現的頻繁項集中提取大於置度閾值的規則，即強規則
##== 關聯規則算法的分類 [殷,5.3.1]
#    -- (1) 蠻力法: 確定格結構中每個候選項集的支持度計數，也就是將候選項集與事務/交易集進行比較，若在其中則增加支持度。
#    -- (2) 從一般到特殊/從特殊到一般: 從頻繁(k-1)項集到k項集 / 從頻繁k項集到(k-1)項集
#    -- (3) 等價類: 將格劃分為若干個不相交的結點組(等價類)
#    -- (4) 寬度優先/深度優先: 從頻繁(k-1)/(k+1)項集到k項集即各個處理 / 從頻繁1項集到2項集到...k項集...
##== 先驗(Apriori)原理: 如果一個項集是頻繁的，則它的所有子集一定也是頻繁的 [殷,5.3.2]
#    -- 例: (3C)的格結構
#    -- 支持度度量的反單調性: 一個項集的支持度，絕對不會超過它的子集的支持度
#    -- 支持度的剪枝(prune): 一旦發現一個項集是非頻繁的，則其所有子集的子集可以被立即剪枝
##== 先驗算法(Apriori algorithm): 是基於格結構、從一般到特殊、寬度優先的算法 [殷,5.3.2]
#    -- (1) 設定k=1
#    -- (2) 掃描事務/交易數據集一次，生成頻繁的1項集
#    -- (3) 如果存在兩個或兩個以上的k項集，則重複下面過程:
#           -- (3A) 產生候選(candidate)項集: 由頻繁k項集，生成候選的頻繁(k+1)項集
#           -- (3B) 候選前剪枝: 候選的頻繁(k+1)項集，若有非頻繁的k項子集，則刪除之
#           -- (3C) 計算支持度: 掃描事務/交易數據庫一次，統計每個餘下候選(k+1)項集的支持度
#           -- (3D) 候選後剪枝: 依支持度，刪除非頻繁的候選(k+1)項集
#           -- (3E) 設定k=k+1
#    -- 執行例: 如(4A)
##== 時間複雜度 [略, 殷,pp.123-124]
##== 影響計算複雜度的因素
#    -- 支持度閾值(最小支持度minSupp): 支持度閾值越小，頻繁項集的最大長度將越大
#    -- 項數(維度): 頻繁項集的項數越大，候選項集的數目將越多，計算時間越久
#    -- 事務/交易數: 事務/交易數越多，計算時間越久
#    -- 事務/交易的平均寬度: 事務/交易寬度越大，候選項集的項數將越多，計算時間越久
##== FP-growth算法 [韓家煒,2000] [殷,5.3.2]
#    -- 將頻繁項集的數量庫，壓縮到一棵頻繁模式樹
#    -- 算法與說例 [略, 殷,pp.124-131]

#####===== (4D) 產生關聯規則 [殷,5.4] =====#####
D = list( c("A","B","C"), c("A","B","C","D"), c("B","C","E"), c("A","C","D","E"), c("D","E") )
##== 頻繁項集(FIS, Frequent Item Sets)的項集與支持數
fisD04 = eclat(D, parameter=list(support=0.4,maxlen=3));   dim(inspect(fisD04))[1];   inspect(fisD04)[6:9,]  #-- 14
#       items support count
# [6] {A,B,C}     0.4     2
# [7]   {B,C}     0.6     3
# [8]   {A,B}     0.4     2
# [9]   {A,C}     0.6     3
##== 從置信度到關聯規則的格結構:
#  -- 置信度 confidence(X->Y) = P(Y|X)=P(X,Y)/P(X) --- 關聯規則(AR):lhs => rhs support confidence lift      count    
#    ABC->{} (2/2)----- BC ->A (2/3) ----- B->AC (2/3)     # [19] {B,C} => {A} 0.4     0.6666667  1.1111111 2 
#                                    --^-- C->AB (2/4) 
#                 ----- AC ->B (2/3) ----- A->BC (2/3)     # [20] {A,C} => {B} 0.4     0.6666667  1.1111111 2    
#                 ----- AB ->C (2/2)                       # [18] {A,B} => {C} 0.4     1.0000000  1.2500000 2                                   
##== 本說例的整個關連規則(association rule, AR):
arD04 = apriori( D, parameter=list(support=0.4, confidence=0.6), control=list(verbose=FALSE));  
length(arD04);  inspect(arD04)    #-- [1] 23
#      lhs      rhs support confidence lift      count
# [1]  {}    => {E} 0.6     0.6000000  1.0000000 3    
# [2]  {}    => {B} 0.6     0.6000000  1.0000000 3    
# [3]  {}    => {D} 0.6     0.6000000  1.0000000 3    
# [4]  {}    => {A} 0.6     0.6000000  1.0000000 3    
# [5]  {}    => {C} 0.8     0.8000000  1.0000000 4    
# [6]  {E}   => {D} 0.4     0.6666667  1.1111111 2 
# [7]  {D}   => {E} 0.4     0.6666667  1.1111111 2
# [8]  {E}   => {C} 0.4     0.6666667  0.8333333 2    
# [9]  {B}   => {A} 0.4     0.6666667  1.1111111 2    
# [10] {A}   => {B} 0.4     0.6666667  1.1111111 2    
# [11] {B}   => {C} 0.6     1.0000000  1.2500000 3    
# [12] {C}   => {B} 0.6     0.7500000  1.2500000 3    
# [13] {D}   => {A} 0.4     0.6666667  1.1111111 2    
# [14] {A}   => {D} 0.4     0.6666667  1.1111111 2    
# [15] {D}   => {C} 0.4     0.6666667  0.8333333 2    
# [16] {A}   => {C} 0.6     1.0000000  1.2500000 3    
# [17] {C}   => {A} 0.6     0.7500000  1.2500000 3    
# [18] {A,B} => {C} 0.4     1.0000000  1.2500000 2   
# [19] {B,C} => {A} 0.4     0.6666667  1.1111111 2   
# [20] {A,C} => {B} 0.4     0.6666667  1.1111111 2   
# [21] {A,D} => {C} 0.4     1.0000000  1.2500000 2    
# [22] {C,D} => {A} 0.4     1.0000000  1.6666667 2    
# [23] {A,C} => {D} 0.4     0.6666667  1.1111111 2  
##== 從置信度到關聯規則-->置信度矩陣:
inspect(fisD04)$items
# [1] "{C,E}" "{D,E}" "{A,C,D}" "{C,D}" "{A,D}" "{A,B,C}" "{B,C}" "{A,B}" "{A,C}"  "{C}"  "{A}"  "{B}" "{D}" "{E}" 
inspect(fisD04)$count   #-- [1] 2 2 2 2 2 2 3 2 3 4 3 3 3 3
# 置信度矩陣    2     2     2       2     2     2       3     2     3     4    3    3    3    3
#      X\X,Y   {C,E} {D,E} {A,C,D} {C,D} {A,D} {A,B,C} {B,C} {A,B} {A,C} {C}  {A}  {B}  {D}  {E} 
#    2 {C,E}    -     .     .       .     .     .       .     .     .     .    .    .    .    .
#    2 {D,E}    .     -     .       .     .     .       .     .     .     .    .    .    .    .
#    2 {A,C,D}  .     .     -       .     .     .       .     .     .     .    .    .    .    .
#    2 {C,D}    .     .   [22]1     -     .     .       .     .     .     .    .    .    .    .
#    2 {A,D}    .     .   [21]1     .     -     .       .     .     .     .    .    .    .    .
#    2 {A,B,C}  .     .     .       .     .     -       .     .     .     .    .    .    .    .
#    3 {B,C}    .     .     .       .     .   [19].66   -     .     .     .    .    .    .    .
#    2 {A,B}    .     .     .       .     .   [18]1     .     -     .     .    .    .    .    .    
#    3 {A,C}    .     .   [23].66   .     .   [20].66   .     .     -     .    .    .    .    .
#    4 {C}     0.5@   .    0.5@     .5@   .     0.5@  [12].75 .    [17].75-    .    .    .    .
#    3 {A}      .     .    0.66*    .   [14].66 0.66*   .   [10].66[16]1  .    -    .    .    .    
#    3 {B}      .     .     .       .     .     0.66* [11]1  [9].66 .     .    .    -    .    .    
#    3 {D}      .    [7].66 .66*[15].66 [13].66 .       .     .     .     .    .    .    -    .
#    3 {E}     [8].66[6].66 .       .     .     .       .     .     .     .    .    .    .    -
#    5 {}       .     .     .       .     .     .       .     .     .    [5].8[4].6[2].6[3].6[1].6
#  -- (@)不到最小置信度(minConf=0.6), 
#  -- (*)非最小關聯規則: D->AC, A->CD 包含於 AD->C; C->AB, A->BC 包含於 AC->B (根據下述 先驗性質)
##== 關聯規則的先驗(Apriori)性質: 若 X-S-->S 為關聯規則, 則 X-Ssuper-->Ssuper 必為關聯規則, 其中Ssuper為S的超集 
#    -- 如: AC->B 為關聯規則, C->AB 與 A->BC必為關聯規則


########## (5) 關聯模式的進一步考量 [殷,5.5-5.6] ##########

#####===== (5A) 關聯模式的評估 [殷,5.5] =====#####
##== 興趣度(interestingness)度量: 表示關聯規則模式的有趣性 [殷,5.5.1]
#    -- 客觀興趣度(objective interestingness)度量: 以統計論據建立，可用來排除數據中的獨立項或少數模式的偽聯繫
#    -- 主觀興趣度(subjective interestingness)度量: 以主觀論據建立，可用來無法提供新穎信息或有益行動的主觀無趣模式
##== (客觀興趣度)支持度(support)度量: 反映關聯規則的普遍性 [殷,5.5.2]
#    -- 越高則代表該規則能適用越多的數據
#    -- 缺點: 若支持度閾值過高，許多潛在有意義的模式會被刪除；支持度閾值過低，則計算代價過高，且會產生大量的關聯模式
##== (客觀興趣度)置信度(confidence): 反映關聯規則的可靠性 [殷,5.5.2]
#    -- 越高則代表前件滿足時,後件成立的可能性越大
#    -- 辛普森悖論: 整體數據分析的關聯規則，可能不適用於分組數據 [殷pp.135-136]
#    -- 缺點: 若忽略規則前件和計的統計獨立性，即便置信度高，可能會是誤導的關聯規則
##== (客觀興趣度)其他基於統計的度量 [殷,5.5.3]
#    -- 提升度lift(X->Y) = P(Y|X)/P(Y) = conf(X->Y)/P(Y): 評估項集X出現是否能促進項集Y出現
#       -- lift(X->Y) = 1: 表示 P(X,Y) = P(X)P(Y)，則X和Y是相互獨立的，否則存在某種依賴關係
#       -- (經驗) 所以通常算出關聯規則後，要依提升度排序，小於1 (常用小於 1.1)　的則捨棄
#    -- 相關係數(correlation coefficient) r = (P(X,Y)-P(X)P(Y)) / sqrt(P(X)(1-P(X))P(Y)(1-P(Y)))
#       -- r = (+1)/(-1)/0: 表示 完全正相關/完全負相關/統計獨立
#    -- 餘弦(cosine) IS = cos(X,Y) = P(X,Y)　/　sqrt(P(X)P(Y)): 越大代表越相似
##== (主觀興趣度) 將主觀信息加入到模式發現任務 [https://www.twblogs.net/a/5cab8268bd9eee5b1a07c26a]
#    -- 可視化方法: 以數據可視化方法呈現出數據中蘊涵的信息
#    -- 基於模板的方法：限制發現的模式類型，只有滿足指定模板的模式才被認爲是有趣的
#    -- 主觀興趣度量: 以基於領域信息定義某些主觀度量，來過濾顯而易見和沒有實際價值的模式

#####===== (5B) 關聯規則的圖形化 [殷,5.6.2] =====#####
library(arulesViz)
arG = apriori( Groceries, parameter=list(support=0.05, confidence=0.2), control=list(verbose=FALSE));  
length(arG);  inspect(arG)    #-- [1] 7
#     lhs                   rhs                support    confidence lift     count
# [1] {}                 => {whole milk}       0.25551601 0.2555160  1.000000 2513 
# [2] {yogurt}           => {whole milk}       0.05602440 0.4016035  1.571735  551 
# [3] {whole milk}       => {yogurt}           0.05602440 0.2192598  1.571735  551 
# [4] {rolls/buns}       => {whole milk}       0.05663447 0.3079049  1.205032  557 
# [5] {whole milk}       => {rolls/buns}       0.05663447 0.2216474  1.205032  557 
# [6] {other vegetables} => {whole milk}       0.07483477 0.3867578  1.513634  736 
# [7] {whole milk}       => {other vegetables} 0.07483477 0.2928770  1.513634  736 
##== (1) 分組矩陣圖
plot(arG, method="grouped")   #-- x/y: lhs/rhs, size:support, color:lift
##== (2) 彩色矩陣圖 (課本上的 reorder=TRUE不能使用, measure=c("lift","confidence")用了無用)
plot(arG, method="matrix")    #-- x/y: lhs/rhs, color:lift
##== (3) 有向圖 (課本上加入的 control=list(type="itemsets")用了看不出效果)
plot(arG, method="graph")     #-- x/y: lhs/rhs, size:support, color:lift

#####===== (5C) 關聯規則的圖形設計 =====#####
##== igraph軟件包: 一個非常強大的軟件包，可以進行圖形運算與社群網路分析
library(igraph)
##== 圖形(graph): 一種知識表示法的呈現，具有節點(node,vertex)和邊(edge)
graph.arG = graph.edgelist( cbind(inspect(arG)$lhs, inspect(arG)$rhs) )
graph.arG
# IGRAPH 21e95f9 DN-- 5 7 -- 
#   + attr: name (v/c)
# + edges from 21e95f9 (vertex names):
# [1] {}          ->{whole milk}       {yogurt}    ->{whole milk}  {whole milk}      ->{yogurt}          
# [4] {rolls/buns}->{whole milk}       {whole milk}->{rolls/buns}  {other vegetables}->{whole milk}      
# [7] {whole milk}->{other vegetables}
V(graph.arG)
# + 5/5 vertices, named, from 21e95f9:
#   [1] {}                 {whole milk}       {yogurt}           {rolls/buns}       {other vegetables}
##== 設定節點與邊的屬性
#    -- 節點(node,vertex): 代表 lhs/rhs頻繁項集
#    -- 有向邊(directed edge): 代表 連帶消費之關連規則，其上的數字代表置信度 (confidence)
sizeV0 = length(Groceries)*inspect(arG)$support/inspect(arG)$confidence;   sizeV0   #-- [1] 9835 1372 2513 1809 2513 1903 2513
sizeV  = sizeV0[match(V(graph.arG)$name,inspect(arG)$lhs)];   sizeV   #-- [1] 9835 2513 1372 1809 1903
plot(graph.arG, edge.arrow.size=0.1, edge.curved=0.3,   #-- 邊(edge)的箭頭(arrow)與曲率(curved)設定
     vertex.size=0.01*sizeV, vertex.label=paste0(V(graph.arG)$name,"\n(",sizeV,")"),   
                                                        #-- 支持度/支持數 用來設定 節點(vertex)大小(size)與標籤(label)
     edge.label=round(inspect(arG)$confidence,2))       #-- 置信度 用來 設定邊(edge)的標籤


########## (6) 關聯規則的實作應用 ##########

#####===== (6A) (Rtech03-6) 文本分析的準備 (RRlist-->RR,dtm) =====#####
##== (1:KDD1) 讀取數據 (Rlist/RR...csv-->RR)
library(data.table)
Rlist = c("RR1_500r6810","RR501_1000r6217","RR1001_1500r7707","RR1501_2000r8262")
for (k in 1:length(Rlist)) {
  print(paste0(">> reading file - ",Rlist[k],".csv..."))
  RRk = fread(paste0(Rlist[k],".csv"), encoding="UTF-8" )
  if (k==1) { RR = RRk }   else { RR = rbind(RR,RRk) }
}
dim(RR);   head(RR,2)       #-- 28996 / 464636 
##== (2:KDD3) 數據整併 (RR-->content/Ncontent)
RRtitle = unique(RR$title);   length(RRtitle)   #-- (1) 共有1993個討論主題
content = NULL;   Ncontent = NULL               
for (title in RRtitle) {                                   #-- (2) 同一討論主題的回文內容(RR$text),以"\n\n"串接
  content = c(content, paste(title, paste(RR$text[which(RR$title==title)],collapse="\n\n"),collapse="\n\n"))
  Ncontent = c(Ncontent, length(which(RR$title==title)))   #-- (3) length(which(條件)): 表示滿足條件的個數
}
length(content);   head(content,2)     #-- [1] 1993             #-- (4) content:討論主題的回文內容(RR$text)集合,
##== (3:KDD3) 斷詞 (content-->Dword0-->Dword)
library(jiebaR)
wkr2 = worker(type="tag")     
Dword0 = sapply(content, function(x) segment(tolower(as.character(x)), wkr2));   #-- 要先轉成文字型式(as.character)，並將其中的所有英文字轉為小寫(tolower)
length(Dword0);   head(Dword0[[1]],10)   #-- [1] 1993   #-- 所得到的 Dword0 為由 content各個元素斷詞的表列(list)，共1993個表列元素
Dword = lapply(Dword0, function(x){ nx=sapply(x,nchar); xA=x[nx>=2]; 
return( xA[substr(names(xA),1,1) %in% c("n","v")] ) } );   
length(Dword);   head(Dword[[1]],10)  #-- [1] 1993
##== (4:KDD3) 詞彙集 (Dword-->tokens-->vocab)
library(text2vec)
wkr3 = worker("keywords",topn=5);   wkr3   ##== 在工作引擎中加上"keywords"參數，並可由 topn 指定關鍵詞數目
tokens = lapply(Dword, FUN=function(x) vector_keywords(x,wkr3));   length(tokens);   head(tokens,2)   #-- [1] 1993
it = itoken(tokens, progressbar = FALSE)  
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 3L);   dim(vocab);   head(vocab,3)   #-- 606  3
#    term term_count doc_count
# 1: 一壘          3         3
# 2: 上過          3         3
# 3: 不到          3         3
##== (5:KDD4) 文本詞語矩陣 (Document-Term Matrix, dtm) (vocab-->dtm)
vectorizer = vocab_vectorizer(vocab)
dtm = create_dtm(it, vectorizer);   dim(dtm);    #-- [1]  1993  606
rownames(dtm)=RRtitle;   as.matrix(dtm[1:3,1:45])

#####=====*(6B) 計算數據關聯 (dtm->dtmList->rules0/rules0.df) =====#####
##== dtm格式要轉為list(),作為數據關聯的準備 (dtm->dtmList)
dtmList = lapply(1:dim(dtm)[1], FUN=function(k) return(colnames(dtm)[which(as.matrix(dtm)[k,]>0)]))
##== 數據關聯(dtmList->rules0)
rules0 = apriori( dtmList, parameter=list(support=0.005, confidence=0.5), control=list(verbose=FALSE));  
length(rules0);  inspect(rules0)      #-- [1] 29
##== 關聯規則轉為數據框(rules0->rules0.df)
rules0.df = data.frame( lhs=labels(lhs(rules0)), rhs=labels(rhs(rules0)), rules0@quality )
dim(rules0.df);   head(rules0.df,3)   #-- [1] 29  6
#      lhs    rhs     support confidence       lift count
# 1 {晚餐} {運動} 0.005017561  0.9090909   2.787413    10
# 2 {壓縮} {惡魔} 0.005017561  0.7142857 101.683673    10
# 3 {惡魔} {壓縮} 0.005017561  0.7142857 101.683673    10

#####===== (6C) 數據模型:提取數據關連規則中單項規則 (rules0.df-->rules) =====#####
indX = union( union( grep(",",rules0.df$lhs), which(rules0.df$lift<1.1)), 
              which(rules0.df$count==1)  );   indX   #-- [1] 21 22 23 24 25 26 27 28 29
rules = cbind(rules0.df[,c("lhs","rhs")],round(rules0.df[,c("support","confidence","lift")],2),
              rules0.df[,"count"])[-indX,]
colnames(rules) = c("lhs","rhs","supp","conf","lift","count");  dim(rules)   #-- [1] 20  6

#####===== (6D) 數據呈現:繪製規則關連圖 (rules-->gR-->plot) =====#####
library(igraph)
gR = graph.edgelist(cbind(as.character(rules$lhs),as.character(rules$rhs)))
plot(gR, edge.arrow.size=0.1, edge.curved=0.3 )


########## (R) 复习 ##########

#####===== (R-HW) 演练作业HW =====#####
## 试就你爬取解析的網頁數據，或教材中提供的數據，繼續上一章的作業:
##== (HWA) 確認其中的文本詞語矩陣 
##== (HWB) 將前述文本詞語矩陣轉成list型態，以進行關聯分析
##== (HWC) 用 apriori() 指令求出關聯規則，建議其中最好包括5-20條規則
##== (HWD) 試用 igraph畫出關聯規則，節點應呈現頻繁集的標記
## 请在本周内一起缴交

#####===== (R-RV) 重点复习RV =====#####
##== (RVA) 數據模型(model)的應用，可以分成兩個階段：訓練階段與 "預測階段"。
##== (RVB) 關聯分析的基礎是一個事務/交易集，其中每一筆交易數據是形如{啤酒,尿片}的"項集(item set)"。
##== (RVC) 一個項集的出現次數(如:30次)與數據集中所有事務/交易數(如100筆)的百分比(如0.3)，稱之為"支持度(support)"。
##== (RVD) 關聯規則X->Y，在X出現的情況下，Y會出現的概率，稱之為"置信度(confidence)"。
##== (RVE) 在以igraph進行關連分析繪圖時，有向邊(directed edge)是用來表示頻繁項集間的"關聯規則"。
##== (RVF) 在R中以arules軟件包做關聯分析時，數據集要先轉成"list"型態，才能求出關聯規則。
