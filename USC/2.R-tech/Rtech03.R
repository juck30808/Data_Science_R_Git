# Rtech03.R: 大數據實務技術 - 03: 數據聚類 (4hr)
# Jia-Sheng Heh (賀嘉生), 12/02/2020, revised from HUT05.R

setwd("/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data/")
#install.packages( c("fpc","jiebaR","text2vec","data.table","stringr") )    


#####=====(1A) 聚類(clustering) =====#####
##== 聚類: 把數據對象集,劃分成多個組或簇(cluster)的過程
#    -- 簇內(具有高相似性,distance短) / 不同簇(不相似)
#    -- Input: 樣本間的相似性(距離distance) / Output: 簇集(cluster)

##== 演算法的類型
#    -- 劃分型(partitional): 將數據對象集劃分成不重疊的簇,使每個對象在單子集中(k-means, k-medoids)
#    -- 層次型(hierarchical):嵌套簇的集族,組織成一棵樹 (kNN) 

##== 對象與簇組關係
#    -- 互斥型: 每個對象都指派到單個簇
#    -- 非互斥型: 一個對象同時屬於多個簇
#    -- 模糊型: 在模糊聚類中,每個對象以一個0-1之間的隸屬權值(membership value)屬於每個簇(模糊簇)

##== 簇的類型
#    -- 明顯分離(KNN): 每個對象到同簇旳其他對象的距離,比到不同簇中的任意對象的距離更相似
#    -- 基於原型(k-means, k-medoids): 每個對象到定義該簇的原型的距離,比到其他簇的原型的距離,更相似
#    -- 基於密度(DBSCAN): 簇是對象的稠密區域, 不同簇之間的密度比較低

#####===== (1C)用於練習講解與比較的 iris 數據 =====#####
#ICS 機械學習數據庫http://archive.ics.uci.edu/ml/datasets/Iris 
#-- 花萼(Sepal)長度,寬度 / 花瓣(Petal)長度,寬度
#-- 類別(Class)：三個品種Setosa，Versicolor和Virginica

dim(iris)    # 數據的筆數為150筆
head(iris,2) # 五個欄位(前四個單位為公分)：




#####===== (1D)鳶尾花類別以三種顏色作(Sepal.Length, Petal.Length)分布圖與聚類分布圖 =====#####

##== (1) 設定數據框為iris, 可以精簡以下的變量表示
attach(iris)  
##== (2) 類別分布圖
plot.iris <- function(xx,yy,xxlab,yylab) {
  plot(NULL,xlim=range(xx),ylim=range(yy),main="classified scatter plot of iris data",xlab=xxlab,ylab=yylab)
  points(xx[Species=="setosa"],yy[Species=="setosa"],pch=1,col="blue")
  points(xx[Species=="virginica"],yy[Species== "virginica"],pch=2,col="green")
  points(xx[Species=="versicolor"],yy[Species== "versicolor"],pch=3,col="purple")
  legend("topleft",legend=c("setosa","virginica","versicolor"), bty="n",col=c("blue","green","purple"),x.intersp=0.5, y.intersp=0.5,pch=c(1,2,3))
}
plot.iris(Sepal.Length, Petal.Length, "Sepal.Length", "Petal.Length")
plot.iris(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length")

##== (3)聚類(iriscls)分布圖
plot.iris.cls <- function(xx,yy,xxlab,yylab,iriscls) {
  xx = Petal.Width;   yy=Petal.Length;   xxlab="Petal.Width";  yylab="Petal.Length"
  plot(NULL,xlim=range(xx),ylim=range(yy),main="classified scatter plot of iris data",xlab=xxlab,ylab=yylab)
  text(xx[Species=="setosa"],yy[Species=="setosa"],iriscls[1:50],col="blue")
  text(xx[Species=="virginica"],yy[Species== "virginica"],iriscls[51:100],col="green")
  text(xx[Species=="versicolor"],yy[Species== "versicolor"],iriscls[101:150],col="purple")
  legend("topleft",legend=c("setosa","virginica","versicolor"), bty="n",col=c("blue","green","purple"),x.intersp=0.5, y.intersp=0.5,pch=c(1,2,3))
}
plot.iris.cls(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length", as.integer(iris$Species))



##==  ----------------------k-means聚類 (iris實驗) --------------------------
##== :K-means 基於距離的聚類算法，兩個對象的距離越近，其相似度就越大，將對象分開k個簇。
##== :使在同一個簇的對象是相似的,不同簇的對象是相異的，把得到細分且獨立的簇作為目標。

##== 偽代碼(pseudocode):
#    -- 輸入: 結果簇的個數，包含n個對的數據集合D
#    -- 輸出: k個簇的集合
#    -- (1) 選擇任意k個對象,作為初始的簇中心(初始質心,1B中的原型prototype)
#    -- (2) repeat
#    -- (3) 根據簇中對象的平均值(原型),使每個對象重新賦給最類似的簇(最近的原型)
#    -- (4) 更新簇的平均值(mean), 即重新計算每個簇中對象的平均值(原型)
#    -- (5) until k個平均值不再發生變化
##== 計算的複雜度(complexity): n--樣本個數,K--簇的個數,d--屬性個數, l--迭代次數
#    -- 空間複雜度: O((n+K)d)
#    -- 時間複雜度: O(nxKxlxd)
##== k-means優點 (快速簡單,效率高,具伸縮性,時間複雜度近於線性，而且適合挖掘大規模數據集)
##== k-means缺點 (k值需要事先給定,初始的聚類中心(質心)影響較大)

# 1.選定k個質心的初始猜測值（隨機選取）
set.seed(20)     

# 2.製作聚類模型：每個點到 k 個質心的距離，將該點分配給距離最近的質心所在的簇。
# 簇中心為樣本點的均值。centers(K) = 期望得到數量，使用歐氏距離(略)，
kmeans.m = kmeans( iris[,1:4], centers=3  );   kmeans.m 

# 3.聚類比較與混淆矩陣(confusion matrix)
plot.iris.cls(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length", kmeans.m$cluster) 
table( kmeans.m$cluster, as.integer(iris$Species) ) 

# Cluster means:                      #-- kmeans.m$centers 聚類中心
# Clustering vector:                  #-- kmeans.m$cluster 聚類結果
# Within cluster sum of squares:      #-- 群內的變異數 (kmeans.m$withinss)/每一類之間的誤差
# WSS = kmeans.m$withinss ;WSS        #-- 1.組內距離平方和WSS ( Within,小好)  #--簇內變差, E = sum(sum(dist(p,ci)))
# BSS = kmeans.m$betweenss;BSS        #-- 2.組間距離平方和BSS(Between,)大好
# TSS = BSS + WSS ;TSS                #-- 3.總離均差平方和TSS(Total)
# ratio = WSS /TSS;ratio
# Available components:


##==  ----------------------k-medoids聚類 (iris實驗) --------------------------
##== : 因k-means算法對離群點敏感，每次迭代的質心是選用簇中離平均值最近的代表對象
##== 偽代碼(pseudocode): PAM算法(Partitioning Around Medoids,圍繞中心點劃分)
#    -- 輸入: 結果簇的個數，包含n個對的數據集合D
#    -- 輸出: k個簇的集合
#    -- (1) 選擇任意k個對象,作為初始的簇中心oj
#    -- (2) repeat
#    -- (3) 把剩餘對象分配到距離它最近的代表點所在的簇
#    -- (4) 隨機選擇一個非中心點對象o_random
#    -- (5) 計算用o_random交換oj的總代價s
#    -- (6) 如果s<0, 則用o_random 替換oj, 形成新的k個中心點
#    -- (7) until k 個中心點不再發生變化
##== 計算的複雜度(complexity): n--樣本個數,K--簇的個數,d--屬性個數, l--迭代次數
#    -- 時間複雜度: O(n^2)

library(fpc)
pamk.result = pamk(iris[,1:4]);   pamk.result
plot.iris.cls(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length", pamk.result$pamobject$clustering) #聚類比較
table( pamk.result$pamobject$clustering, as.integer(iris$Species) ) #混淆矩陣(confusion matrix)

# $pamobject           #-- pamk.result$pamobject 聚類模型
# Medoids:             #-- pamk.result$pamobject$medoids 聚類中心
# Clustering vector:   #-- pamk.result$pamobject$clustering 聚類結果
# Objective function:
# Available components:
# $nc                  #-- pamk.result$pamobject$nc 聚類數
# $crit


##==  ----------------------Hierarchical 層次聚類 (iris實驗) --------------------------
##== : 將數據對象組成層次結構或簇的"樹"，分為以下兩種方法
##== : 凝聚方法(agglomerative：bottom-up)
#      形成自己的簇開始，迭代地把簇合併成越來越大，直到所有的對象都在一個簇
##== : 分裂方法(partitional：top-down)
#      把所有對象置於一個簇中開始，把根上的簇劃分成多個較小的子簇，直到底層的簇都足夠相似

##== 偽代碼(pseudocode): 凝聚層次聚類流程
#    -- 輸入: 結果簇的個數，包含n個對的數據集合D
#    -- 輸出: k個簇的集合
#    -- (1) 計算鄰近度矩陣(linkage)
#    -- (2) 每一個點作為一個簇
#    -- (3) repeat
#    -- (4)    合併最接近的兩個簇
#    -- (5)    更新鄰近度矩陣
#    -- (6) until 僅剩下一個簇

##== 層次聚類優點(不需確定簇數量,可切割樹圖得,不需要隨機的初始值計算的結果是一致)
##== 層次聚類缺點(時間複雜度高 O(n^2)，因此若數萬筆以上的數據，建議先做預先處理)


#####===== (3B) 距離(distance) [殷,8.3] =====##### 
head(iris[,1:4])                          #== (1) 向量化數據(vectorization)
iris[1,1:4] - iris[2,1:4]                 #-- 距離的計算,誤差 (Error) = 兩個點之間的差距
(iris[1,1:4] - iris[2,1:4])^2             #-- 平方誤差 (Squared Error)
sum((iris[1,1:4] - iris[2,1:4])^2)        #-- 平方誤差和 (MSE)
sqrt(sum((iris[1,1:4] - iris[2,1:4])^2))  #-- 根號平方誤差和 (RMSE)
dist(iris[,1:4])[1:10]                    #-- 距離函數(dist())與距離矩陣
x=as.matrix(dist(iris[,1:4]))[1:4,1:8]    #-- 距離矩陣
round(x,2)

# 在階層式分群中，主要是以資料之間的「距離」遠近，來決定兩筆資料是否接近。
# R 我們可以使用dist()，來建立資料之間的「距離矩陣」(Distance Matrix)
dist(iris[,1:4],method="euclidean" )[1:5]    #-- 歐幾里德式距離 (預設)
dist(iris[,1:4],method="manhattan" )[1:5]    #-- 曼哈頓距離
dist(iris[,1:4],method="maximum" )[1:5]      #-- 最大距離法

hc_cmp = hclust(dist(iris[,1:4]));   hc_cmp
plot(hc_cmp)   ##== 繪出樹圖/蟹爪圖(dendrogram)
hc_cmp.cls = cutree(hc_cmp,3);   hc_cmp.cls

# Call:  hclust(d = dist(iris[, 1:4]))
# Cluster method   : complete    #---> 預設算法為complete linkage (見下子節)
# Distance         : euclidean   #---> 預設距離為euclidean
# Number of objects: 150 

##== 不同類別數的比較1
plot.iris.cls(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length", hc_cmp.cls) ##== 聚類比較
table( hc_cmp.cls, as.integer(iris$Species) ) ##== 混淆矩陣(confusion matrix)

##== 不同類別數的比較2
plot.iris.cls(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length", cutree(hc_cmp,6))
table( cutree(hc_cmp,6), as.integer(iris$Species) )


#####=====*(3D) 鄰近度(linkage) [殷,8.3] =====##### 
##== 鄰近度矩陣(linkage): 度量兩個簇(Ci,Cj)之間的距離 
##== (1) 最遠鄰(farthest neighbor)聚類算法 (全鏈算法, complete linkage)
#        -- dist_max(Ci,Cj) = max_{pi in Ci, pj in Cj}(p,p')
hc_cmp = hclust(dist(iris[,1:4]), method="complete" );   
plot(hc_cmp,main="iris dedrogram with complete linkage")
##== (2) 最近鄰(Nearest Neighbor, kNN)聚類算法 (單鏈算法, single linkage)
#        -- dist_min(Ci,Cj) = min_{pi in Ci, pj in Cj}(p,p')
par(mfrow=c(2,2))
hc_sng = hclust(dist(iris[,1:4]), method="single" );   
plot(hc_sng,main="iris dedrogram with single linkage")
##== (3) 組平均聚類算法 (平均鏈算法, average linkage)
#        -- dist_mean(Ci,Cj) = | mean(Ci) - mean(Cj) |
hc_avg = hclust(dist(iris[,1:4]), method="average" );   
plot(hc_avg,main="iris dedrogram with average linkage")
##== (4) 質心聚類算法 (質心鏈算法, centroid linkage)
#        -- dist_mean(Ci,Cj) = sum_{pi in Ci, pj in Cj}|pi-pj| / n(Ci)n(Cj)
hc_cntrd = hclust(dist(iris[,1:4]), method="centroid" );   
plot(hc_cntrd,main="iris dedrogram with centroid linkage")
##== (5) ward聚類算法 (ward鏈算法, ward.D linkage): dist_ward = 兩個簇合併時,導致的平方誤差的增量 (和average很相似)
hc_ward = hclust(dist(iris[,1:4]), method="ward.D" );   
plot(hc_ward,main="iris dedrogram with ward.D linkage")
par(mfrow=c(1,1))
##== 幾種層次聚類的混淆矩陣計算
table( cutree(hc_sng,3), as.integer(iris$Species) )
table( cutree(hc_avg,3), as.integer(iris$Species) )
table( cutree(hc_cntrd,3), as.integer(iris$Species) )
table( cutree(hc_ward,3), as.integer(iris$Species) )



##==----------DBSCAN 密度聚類 (iris實驗) --------------------------
##== : 過濾噪音數據,發現任意形狀的簇，計算高密度連通的基於密度的聚類算法(OPTICS, DENCLUE等)
##== 兩個參數
#    -- 特定點的密度: 通過該點eps半徑之內的點計數(包括點本身) ---- eps越大,密度越大
#    -- MinPts: 稠密區域的密度閾值
##== 兩個點(點p對點q)的關係
#    -- 關於eps 直接密度可達(q#p): p 在 q的eps鄰域 內
#    -- 關於eps,MinPts 密度可達(q-p): q = p1, p1#p2#p3#..#pn, pn=p, pi#p(i+1) 為直接密度可達
#    -- 關於eps,MinPts 密度相連: (q-o), (p-o) 均為密度可達
##== 所有的點可分類為
#    -- 核心點(core points): 該點的eps鄰域內的點的個數, 超過密度閾值MinPts
#    -- 邊界點(border points): 不是核心點，但落在某個核心點的鄰域內
#    -- 噪聲點(noise): 不是核心點，也不是邊界點
##== DBSCAN的優點(對噪音不敏感,可以處理不同形狀和大小的數據)
##== DBSCAN的缺點(密度變化、高維數據計算量不能太大，eps,MinPts不易選擇)

library(fpc)
ds = dbscan(iris[,1:4], eps=0.42, MinPts=5);ds
ds$cluster
table( ds$cluster, as.integer(iris$Species) )
plot.iris.cls(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length", ds$cluster) ##== 聚類比較

##== 換eps的聚類比較
par(mfrow=c(2,1))
plot.iris.cls(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length", dbscan(iris[,1:4],eps=0.5,MinPts=5)$cluster)
plot.iris.cls(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length", dbscan(iris[,1:4],eps=0.4,MinPts=5)$cluster)
par(mfrow=c(1,1))


########## (5) 聚類方法的評估 [殷,8.5] ##########

#####===== (5A) 聚類方法的考量 [殷,8.5] =====##### 
##== 1.可伸縮性: 
#    -- 許多聚類算法在數百個對象的小數據集合運行良好，但在數百萬以上對象的數據便會有偏頗的結果
#    -- (經驗) 層次方法在上萬個對象便可能會太慢
#    -- (然而) 不可能未做預分析，直接拿數百萬對象直接分析，數據分析上是不對的...

##== 2.處理不同屬性類型的能力: 
#    -- 許多算法是針對數值數據設計,但越來越多的需要用到序列,文本,圖像等數據
#    -- (經驗) 即便是大量的文字或圖像要做聚類，也必須要先做特徵擷取。參見本段第(6)節

##== 3.發現任意形狀的簇: 
#    -- 基於距離度量的算法是用來發現球狀簇，但要開伋能發現任意形狀的簇
#    -- (評論) 這不是實務數據分析的課題，而是研究的方向。          

##== 4.對於確定輸入參數的領域知識的要求: 
#    -- 如簇數，需要用戶以輸入參數的形式提供領域知識
#    -- (經驗) 如簇數，也不是由領域知識可以提供的 (如交易數據分析，怎能確定客戶對象該分幾個簇?)
我們做的事ＡＰＰ擁有許多的劇類資料，我們可用參數其實很多，我們可以選擇我們的領域分析
像是假設在Kmeans,KNN使用上 

##== x5.處理噪音數據的能力: 
#    -- 包括離群值和缺失值等，需要對噪音魯棒性(robust)的聚類方法
#    -- (經驗) 噪音或離群值，未必全為干擾，應以領域知識解讀 (如高消費金額的超貴客,看起來是離群值,但很有價值)

##== 6.增量聚類和對輸入次序不敏感: 
#    -- 因有新數據之增量更新可能隨時發生 ,一些方法也可能對輸入次序敏感
#    -- (評論) 這是兩個課題: (1) 增量聚類: 這在實務上很需要考慮
#                            (2) 對輸入次序不敏感: 這包括對初值的敏感, 這是劃分算法的最大缺點
##== 7.聚類高維數據的能力: 
#    -- 處理高維空間的數據對象是一個挑戰，尤其是可能非常稀疏傾斜
#    -- (經驗) 數據聚類屬於數據建模(data modeling)，
#    -- 數據建模前需要先進行關連性分析(correlation analysis)，挑出較高關連的屬性，再進行建模

##== 8.基於約束的建模: 
#    -- 實際應用可能要在各種約束條件下進行聚類，這是有挑戰性的任務
#    -- Yes, 但需 by case 進行

##== 9.可解釋性和可用性: 
#    -- 聚類可能需要與特定的語義解釋和應用相聯繫
#    -- (經驗) 與領域知識結合，聚類的解釋能創造數據分析很大的價值

#####=====*(5B) 聚類方法的評估度量或指標 [殷,8.5] =====##### 
set.seed(20)   #-- 隨機選取初始質心
kmeans.m = kmeans( iris[,1:4], centers=3  )

##== (1) 非監督性指標 (內部指標)
#        -- 簇內的凝聚性(緊湊性,緊緻性): 確定簇中對象如何密切相關
#        -- 簇間的分離性(孤立性): 確定某個簇不同於其他簇的地方
#        ---> 如: 簇內/簇間 變差計算 -- 
kmeans.m$withinss
# Within cluster sum of squares by cluster:    
#   [1] 23.87947 15.15100 39.82097   #-- kmeans.m$withinss 簇內變差 E = sum(sum(dist(p,ci)))
kmeans.m$betweenss
# (between_SS / total_SS =  88.4 %)  #-- kmeans.m$betweenss=602.5192; kmeans.m$totss=681.3706

##== (2) 監督性指標 (外部指標): 聚類結構與外部結構的匹配程度
#        -- 如: 雙分類混淆矩陣 (Two-class confusion matrix)
#                                    ACTUAL CLASS
#                               positive          negative
# PREDICTED    positive   True Positives   False Positives <-- type-I error (alpha)
# CLASS                             (TP)              (FP)
#              negative  False Negatives    True Negatives
#                                   (FN)              (TN)
#               type-II error (beta) --^
#                 power of test = 1-beta
table( kmeans.m$cluster, as.integer(iris$Species) )
# 1  2  3
# 1  0  2 36
# 2 50  0  0
# 3  0 48 14

##== (3) 相對指標: 比較不同的聚類或簇，用監督或非監督的不同評估指標


########## (6) 文本與詞語的聚類 ##########

#####===== (6A) (HUT04-3) 文本分析的準備 (RRlist-->RR,dtm) =====#####
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

#####===== (6B) (KDD4) 層次聚類 (dtm-->group) =====#####
##== (1) 在數據分析中，很多理論會濃縮成一條指令 (dtm-->fit_hc)
fit_hc = hclust(dist(as.matrix(dtm)),method="ward.D")
##== (2) 聚類數的決定 (fit_hc-->Ncls)
#        -- 不建議繪出樹圖，而要以演算方式來求數聚類數：
#        -- 經驗法則: 聚類數 ~ sqrt(數據對象數) ---> 在本例中,約為 sqrt(1993)=44.63
#        -- 利用一個迴圈，來找最適合聚類數:
for (kk in 20:95) { group = cutree(fit_hc, k=kk);   print(table(group)) };  
# ...
# group
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32 
# 36  13  12  21  17  14  24  16  26  16  10  13 281  16  41  21  29  18  34  99  72  21  14  12  42  24  16  22  19   7   9  30 
# 33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64 
# 10  30  36  13  14  16  26  22  16   8  10  23  10  51  15  17  19  17  10  27  13  16  15   9  14  14  13  11  17  18  18  19 
# 65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88 
# 18  18  20  10  12  15  15  41  18  15  21  28   9  20  10  13  10   9  23  14  13  10   9  10 
# group
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32 
# 36  13  12  21  17  14  24  16  26  16  10  13 254  16  41  21  29  18  34  99  72  21  14  12  42  24  16  22  19   7   9  30 
# 33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64 
# 10  30  36  13  14  16  26  22  16   8  10  23  10  51  15  17  19  17  10  27  13  16  15   9  14  14  13  11  17  18  27  18 
# 65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89 
# 19  18  18  20  10  12  15  15  41  18  15  21  28   9  20  10  13  10   9  23  14  13  10   9  10 
# ...
#             --> 當聚類數從88-->89時，可以切開最大簇(從最大281個數據對象-->最大254個數據對象), 
#                 爾後再增加聚類數，此281個數據對象的最大簇，不會被分開 (到聚類數=95個簇，才會被分開)
Ncls=89   
##== (3) 各簇的數據對象個數 (fit_hc,Ncls-->group)
group = cutree(fit_hc, k=Ncls);   print(table(group))  
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32 
# 36  13  12  21  17  14  24  16  26  16  10  13 254  16  41  21  29  18  34  99  72  21  14  12  42  24  16  22  19   7   9  30 
# 33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64 
# 10  30  36  13  14  16  26  22  16   8  10  23  10  51  15  17  19  17  10  27  13  16  15   9  14  14  13  11  17  18  27  18 
# 65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89 
# 19  18  18  20  10  12  15  15  41  18  15  21  28   9  20  10  13  10   9  23  14  13  10   9  10 

#####===== (6C) (KDD5) 聚類結果的觀察 (RRtitle,group) =====#####
##== (1) 某些簇的數據對象觀察 (RRtitle,group)
RRtitle[group==27]    
# [1] "請問一下運動喝的乳清蛋白杯子....."        "關於吃乳清蛋白.."                         "這樣的體質還能喝高熱量乳清嗎?"           
# [4] "強度大概要多少，才適合喝乳清??"           "關於正確減肥的一些疑問? 希望有人可以解答" "請問complete protein跟whey protein..."   
# [7] "飲食控制~只吃蛋白會出問題嗎？"            "我想脫離瘦皮猴啦"                         "蛋白質補充的問題？"                      
# [10] "買哪種肌酸好呢?"                          "請問減肥可以喝高蛋白嗎？"                 "請教各位大大"                            
# [13] "關於飲食的問題想請教一下"                 "保健食品檢驗~"                            "如何吃到天然食物適量的蛋白質"            
# [16] "有人有喝過阿諾的乳青嗎?"        
#---> 都是討論 乳清蛋白的文章主題...
RRtitle[group==36]  
# [1] "Bauerfeind保爾範護膝~~運動狂熱份子"                            "關節退化有救了？不只是膝關節鏡手術，重點是…！（轉載健康2.0）" 
# [3] "深蹲 跑步 久坐 騎車…誰最傷膝蓋？ 膝關節保健必知(轉載中時電子)" "膝蓋不適的建議來看看"                                         
# [5] "關於受傷後護膝選擇"                                            "Adidas 訪鞋網!!"                                              
# [7] "半月板破裂…有經驗的人分享一下"                                 "通常運動傷害 多久才會好?"                                     
# [9] "請問做完膝關節鏡手術後，恢復期間關節會酸痛很久嗎？"            "運動時出汗問題"                                               
# [11] "請問有沒有不傷膝蓋又能鍛鍊股四頭肌的訓練方法？"                "推薦及選擇護膝"                                               
# [13] "請問台北哪裡可以試用 慕樂 Mueller 的護膝!?"          
#---> 都是討論 護膝 的文章主題...

#####=====*(6D) (KDD5) 從文本數據到知識發掘 (RRtitle,group-->KWgroup) =====#####
DTM = as.matrix(dtm)
KWgroup = NULL
for (k in 1:Ncls) {   
  ind = as.vector(which(group==k))   
  nContent = sum(Ncontent[ind])
  if (length(ind)==1) { colDTM0 = DTM[ind,] } else { colDTM0 = colSums(DTM[ind,]) };  colDTM0
  colDTM = colDTM0[which(colDTM0>0)];   colDTM
  sortDTM = colDTM[order(colDTM,decreasing=TRUE)];   sortDTM[1:20]
  max2 = as.integer(sortDTM[2]);  selDTM = sortDTM[which(sortDTM>=max2)];   selDTM
  KWgroup = rbind( KWgroup, c(k, paste(names(selDTM),collapse="/"), length(ind), nContent, paste(ind,collapse="/")) )
}
KWgroup.df = as.data.frame(KWgroup);   colnames(KWgroup.df) = c("k","KW","cntTitle","nContent","indTitle")
dim(KWgroup.df);   KWgroup.df[c(27,36),]   #-- 89  4
#     k          KW cntTitle nContent                                                                    indTitle
# 27 27 乳清/蛋白質       16      191 33/456/540/1081/1082/1194/1226/1276/1346/1484/1527/1640/1682/1776/1854/1924
# 36 36   膝蓋/運動       13      102                     52/260/273/279/423/502/866/997/1026/1335/1353/1732/1733

##== KWgroup 是從文本 RR中挖掘出來的一個知識結構


########## (R) 单元的复习 ##########

#####===== (R-HW) 演练作业HW =====#####
## 试就你爬取解析的網頁數據，或教材中提供的數據，繼續上一章的作業:
##== (HWA) 確認其中的文本詞語矩陣 (作業HW3B)
##== (HWB) 將前述文本詞語矩陣進行聚類分析，找出其中的關鍵字聚類
##== (HWC) 將關鍵字聚類納入前節中的標籤檔(作業HW4A)
##== (HWD) 能的話，加入前節所製作的儀表板中(作業HW4)

#####===== (R-RV) 重点复习RV =====#####
##== (RVA) 聚類(clustering)是根據數據對象間的相似性或距離，把數據對象集 分成很多個 "簇(cluster)"。
##== (RVB) k-means聚類法，需事先給定 "聚類數k"。
##== (RVC) 層次方法聚類時，兩個簇(cluster)之間的距離，稱作 "鄰近度(linkage)"。
##== (RVD) 基於密度的聚類法DBSCAN的密度，是指在eps半徑內的 "點計數"。
##== (RVE) 聚類分析的外部指標，最常用的是比較聚類結構與外部結構匹配的 "混淆矩陣(confusion matrix)"。
##== (RVF) 在文本分析後對文本詞語矩陣DTM進行聚類分析，可以得到 "關鍵字聚類"的知識結構。

