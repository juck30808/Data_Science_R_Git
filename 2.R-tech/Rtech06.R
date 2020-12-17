# Rtech06.R: 大數據實務技術 - 06: 決策樹分類、與神經網絡學習與其他模型
# Jia-Sheng Heh (賀嘉生), 12/16/2020, revised from HUT08.R

setwd("c:/Users/jsheh/Desktop/working/USC/AIbda/")

########## (P) 課前準備 ########## 
##== (1) 從微信群組下載 本份講義程式檔 Rtech06.R 放入本門課課程目錄，並以 RStudio開啟
##== (2) 若見到本編程檔為中文亂碼，請以 File-->Reopen with Encoding --> UTF8，則可看到中文碼顯示
##== (3) 修改本程式第4行，設定工作目錄為 本門課工作目錄
##== (4) 下載本門課所需之軟件包至本機備用 -- 下行安裝指令只需執行一次
# install.packages( c("tree","rpart","rpart.plot","adabag","randomForest","nnet","devtools","neuralnet","e1071","klaR","mle2") )    
##== (5) 從微信群組下載 RR1_500r6810.csv, RR501_1000r6217.csv, RR1001_1500r7707.csv, 
#                       RR"1501_2000r8262.csv 數據檔，放入 本門課工作目錄，作為本課程待用


########## (1) 分類的基本概念 [殷,7.1] ##########

#####=====*(1A) (HUT06-1B,HUB07-1B) 數據模型(Data Model)符號 =====#####
##== 系統/模型/函數(System/Model/Function, M):  因變量/輸出數據y = M( 自變量/輸入數據u )  
#    -- (1) 訓練階段(Training/Learning/Modeling/Estimation Phase): (u, y) -> M
#             由輸入/輸出 u與y，求取(估測estimate)模型M#
#    -- (2) 應用階段(Prediction/Estimation/Production/Application Phase): (u_new, M#) -> y_predict
#             以所估測的模型M#與新的輸入 u_new，求取(估測)新的輸出 y_predict
##== 機器學習(Machine Learning)
#    -- (1) 監督式學習 (Supervised learning): 具範例(u,y), y為教師(teacher, desired output) y, 以求得y=M(u)
#           (1A)迴歸(regression):     y 為連續數據   --> 上一單元HUT07
#           (1B)分類(classification): y 為離散數據   --> 本單元  HUT08   
#    -- (2) 無監督式學習 (Unsupervised learning): 無輸出y, 目標在於發掘輸入(u)的隱含特徵 --> 數據挖掘(Data Mining)
#           (2A)聚類(clustering):           計算數據u的相似度，以產生其分類。 --> HUT05單元
#           (2B)關聯規則(association rule): 計算多數據(ui-uj)間的關連。       --> HUT06單元
#           (2C)數據序列(data sequencing):  計算多數據(ui-uj)間的時序關係。   --> 未列入本課程

#####===== (1B) 數據分割的方法 =====#####
# python 機器學習劃分訓練集/測試集/驗證集, 2019/01/14, https://www.itread01.com/content/1547463484.html
##== 常用的數據分割有不同的比例，均應代表原來的數據
#    -- (1) 留出法(hold-out): 抽取80%的數據用以建構模式, 剩下的20%用於模式的效度檢驗 train_test_split
#    -- (2) k-fold交互驗證(k-fold cross-validation): KFold, GroupKFild, StratifiedKFold 
#           -- 將數據分為 k 個等分，每次選取 k-1 份進行模式訓練，剩下的一份數據則用以測試模式。
#           -- 如此重複 k 次，使每筆數據都能成為訓練數據集與測試數據集，最後的平均結果代表模式的效度。
#    -- (3) leave-one-out cross-validation: 當k個區間等於總樣本數時: LeaveOneGroupOut，LeavePGroupsOut，LeaveOneOut，LeavePOut
#    -- (4) 自助法(bootstraping): 使用重複取樣的方式進行數據取樣: 自定函式
##== 留出法函式: 數據直接取樣函式(X/y_train)及測試數據(X/y_test)
#    -- (1)留出法（hold-out）-- 隨機抽樣
#          -- 將數據集 X 劃分為 訓練集X_train 及 測試集 X_test
#          -- 通常以 2/3~4/5 的數據用於訓練，test_size 預設值為 0.25
#          -- 亦可設定 test_size 為測試數量，另亦可設定訓練數據 train_size
#    -- (2) 留出法（hold-out）-- 分層抽樣

#####===== (1C) (HUT05:1C-1D,HUT07-3A) iris 數據 =====#####
dim(iris);   head(iris,2)
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
##== 數據的筆數為150筆，共有五個欄位(前四個單位為公分)：
#--     花萼長度(Sepal Length),花萼寬度(Sepal Width),花瓣長度(Petal Length),花瓣寬度(Petal Width),
#--     類別(Class)：三個品種Setosa，Versicolor和Virginica
attach(iris)   ##== (1) 設定數據框為iris, 可以精簡以下的變量表示
##== 鳶尾花類別分布圖
plot.iris <- function(xx,yy,xxlab,yylab) {
  plot(NULL,xlim=range(xx),ylim=range(yy),main="classified scatter plot of iris data",xlab=xxlab,ylab=yylab)
  points(xx[Species=="setosa"],yy[Species=="setosa"],pch=1,col="blue")
  points(xx[Species=="virginica"],yy[Species== "virginica"],pch=2,col="green")
  points(xx[Species=="versicolor"],yy[Species== "versicolor"],pch=3,col="purple")
  legend("topleft",legend=c("setosa","virginica","versicolor"), bty="n",col=c("blue","green","purple"),x.intersp=0.5, y.intersp=0.5,pch=c(1,2,3))
}
plot.iris(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length")

#####===== (1D) 主要的分類模型 [殷,7.1] =====#####
##== 決策樹分類: ID3, C4.5, Cart算法 [殷,7.2, 7.9.1-7.9.2]          --> (2)-(3)
##== kNN (k最近鄰)分類 [殷,7.3, 7.9.3]: 是[HUT05]聚類的衍生,訓練/預測時調用 knn3()/predict(), 本單元略過
##== 貝葉斯分類: 最樸素貝葉斯和貝葉斯信念網絡分類法 [殷,7.4, 7.9.4] --> (5A)
##== 人工神經網絡: 無監督學習網絡和有監督學習網絡 [殷,7.5, 7.9.5]   --> (4A-4C)
##== 支持向量機: 線性SVM分類 [殷,7.6, 7.9.6]                        --> (4D)
##== 組合方法 [殷,7.7, 7.9.7]                                       --> (3B)
##== 其他方法


########## (2) 決策樹分類的基本操作 [殷,7.2, 7.9.2] ##########

#####===== (2A) 決策樹(decision tree)基本概念 =====#####
##== 決策樹: 由結節和有向邊組成的層次結構:
#    -- 樹狀結構, 包括(1)(待分類的)決策結點, (2)(可能分類結困的)葉結點或終結點, (3)(不同決策取值的)分支
#    -- 三種數據屬性: (1)標稱屬性(single,married,divorced), (2)序列屬性(e.g.Hot,Mild,Cool), (3)連續屬性(e.g.Petal.Width)
#    -- 兩種數據分裂(split): (1)二元劃分, (2)多路劃分
##== 三種數據不純度(impurity)度量: (1)Gini, (2)熵/亂度(Entropy), (3)分類誤差(Error) [請參見(3A)]
##== 三種常見決策樹算法
#    -- ID3: 利用增益率, 採用二叉樹 -- 演法偽代碼 [殷,7.2, p.176]
#    -- C4.5: ID3改進版, 採用多重分支和剪枝技術 -- 演法偽代碼 [殷,7.2, pp.181-182]
#    -- CART: 利用Gini係數，採用二元遞歸劃分方法 -- 演法偽代碼 [殷,7.2, p.188]
#       [分類迴歸樹(CART, Classification And Regression Tree), Breiman (1984)]
##== R中常見的決策樹軟件包 (不同的演算法，預測的正確率亦不同) 
#    -- party: 利用C4.5, 但因要安裝 rJava與RWeka，本教材中不講義，請參考課本 [殷,7.9.1]
#    -- tree: 使用CART，在下一子節(2B)講解 [殷,7.9.2]
#    -- rpart: 可自定Gini和Entropy，最普遍，網絡上案例最多

#####=====*(2B) iris例說明決策樹tree操作程序及其中觀念 [殷,7.2, 7.9.2] =====#####
library(tree)
##== 模型的訓練階段: 由輸入數據(u)輸出數據(y) 求取模型 M = tree(y~u) 
iris.tree = tree(Species ~ .,data=iris);    iris.tree   #-- * denotes terminal node(葉結點或終結點)
# 1) root 150 329.600 setosa ( 0.33333 0.33333 0.33333 )  
#   2) Petal.Length < 2.45 50   0.000 setosa ( 1.00000 0.00000 0.00000 ) * 葉結點 R2 
#   3) Petal.Length > 2.45 100 138.600 versicolor ( 0.00000 0.50000 0.50000 )        
#     6) Petal.Width < 1.75 54  33.320 versicolor ( 0.00000 0.90741 0.09259 )  
#      12) Petal.Length < 4.95 48   9.721 versicolor ( 0.00000 0.97917 0.02083 )  
#        24) Sepal.Length < 5.15 5   5.004 versicolor ( 0.00000 0.80000 0.20000 ) * 葉結點 FR24
#        25) Sepal.Length > 5.15 43   0.000 versicolor ( 0.00000 1.00000 0.00000 ) * 葉結點 R25
#      13) Petal.Length > 4.95 6   7.638 virginica ( 0.00000 0.33333 0.66667 ) * 葉結點 FR13
#     7) Petal.Width > 1.75 46   9.635 virginica ( 0.00000 0.02174 0.97826 )  
#      14) Petal.Length < 4.95 6   5.407 virginica ( 0.00000 0.16667 0.83333 ) * 葉結點 FR14
#      15) Petal.Length > 4.95 40   0.000 virginica ( 0.00000 0.00000 1.00000 ) * 葉結點 R15
##== 繪製決策樹
plot(iris.tree);     text(iris.tree)  #-- 6個葉結點, 其餘均為決策結點, 數據分裂為二元劃分
##== 從決策樹到AI的規則庫(rulebase)
#    -- node 2 ---> R2:  IF (Petal.Length<2.45) THEN Species=setosa
#    -- node 25---> R25: IF (Petal.Length>2.45)&(Petal.Width<1.75)&(Petal.Length<4.95)&(Sepal.Length>5.15) 
#                                               THEN Species=versicolor
#    -- node 15---> R15: IF (Petal.Length>4.95)&(Petal.Width<1.75) THEN Species=virginica
#    -- nodes FR24,FR13,FR14 為未確定規則(uncertain rules,為粗糙集rough set中的定義)，如:
#                  FR13: IF (Petal.Length>4.95)&(Petal.Width>1.75) THEN Species=virginica (prob=0.66667)
##== 模型的預測階段: 由所求取的模型(M),對新的數據(u_new)進行輸出結果預測 y_predict = predict(M,u_new)
Species.new.tree3 = predict(iris.tree, newdata=iris,level=0.95,interval="confidence")
##== iris圖上的決策屬性分割: iris--iris.tree
plot.iris(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length")
abline(h=2.45,col="purple");  segments(1.75,2.45,1.75,7,col="pink");  segments(0,4.95,1.75,4.95,col="red");
#    -- 可以看出，每一個決策結點，為一個直角分割(rectangular partition), 如:
#       -- 決策結點 2與3 的分支是由圖上的 紫色橫線(Petal.Length=2.45) 來進行數據分裂
#       -- 決策結點 6與7 的分支是由圖上的 粉色直線(Petal.Width=1.75) 來進行數據分裂
#       -- 決策結點14與15的分支是由圖上的 紅色橫線(Petal.Length=4.95) 來進行數據分裂
##== 混淆矩陣(confusion matrix): 比較 原輸出結果 與 預測輸出結果 的差別
Species.new.tree = apply(Species.new.tree3, 1, which.max); Species.new.tree[41:60] #-- 將150*3的陣列 轉成 150*1 的向量
# 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 
#  1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2 
table( Species.new.tree, Species)
#                  Species
# Species.new.tree setosa versicolor virginica
#                1     50          0         0
#                2      0         47         1
#                3      0          3        49

#####===== (2C) iris例說明決策樹rpart操作程序 =====#####
library(rpart);   library(rpart.plot)
##== 訓練模型
iris.rpart = rpart(Species ~. , data=iris);   print(iris.rpart)
# 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
#   2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#   3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
#     6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
#     7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *
##== 繪出決策樹
rpart.plot(iris.rpart)
##== 預測值
Species.new.rpart3 = predict(iris.rpartA, newdata=iris);   Species.new.rpart3
Species.new.rpart = apply(Species.new.rpart3, 1, which.max)
##== 混淆矩陣
table( Species.new.rpart, Species)
#                   Species
# Species.new.rpart setosa versicolor virginica
#                 1     50          0         0
#                 2      0         47         1
#                 3      0          3        49
##== rpart決策樹參數
#    -- minsplit：每一個節點的最少數據(data)數
#    -- minbucket：終端節點(terminal node)的最少數據(data)數
#    -- maxdepth：決策樹的深度
##== 加上參數求得的決策樹
iris.rpartA = rpart(Species ~. , data=iris, control=rpart.control(minsplit=6, maxdepth=4))
rpart.plot(iris.rpartA)
Species.new.rpartA3 = predict(iris.rpartA, newdata=iris);   Species.new.rpartA3
Species.new.rpartA = apply(Species.new.rpartA3, 1, which.max)
table( Species.new.rpartA, Species)  #---> 分類誤差完全相同(沒有改善)
#                   Species
# Species.new.rpart setosa versicolor virginica
#                 1     50          0         0
#                 2      0         47         1
#                 3      0          3        49

#####===== (2D) 分類模型的評估 [殷,7.8] =====#####
##== 測試與預測數據
y_actual  = as.integer(Species);            y_actual[91:110]   #-- [1] 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3
y_predict = as.integer(Species.new.rpart);  y_predict[91:110]  #-- [1] 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 2 3 3 3
##== 數據間的距離
dist( rbind(y_actual,y_predict) )   #-- = 2: euclidean distance (歐式距離,預設)
dist( rbind(y_actual,y_predict), method="manhattan" )   #-- = 4: manhattan distance (曼哈頓/直角距離)
  #-- method 可以是 "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
##== 混淆矩陣(confusion matrix)
#--  -- 多分類混淆矩陣
table(y_actual, y_predict)
#           y_predict
# y_actual  1  2  3
#        1 50  0  0
#        2  0 47  3
#        3  0  1 49
y_actual1 = y_actual[51:150];    y_predict1 = y_predict[51:150]
TAP = table(y_actual1, y_predict1);   TAP
#            y_predict1
#--  -- 雙分類混淆矩陣 (Two-class confusion matrix)
#                                    ACTUAL CLASS
#                               positive          negative
# PREDICTED    positive   True Positives   False Positives <-- type-I error (alpha)
# CLASS                             (TP)              (FP)
#              negative  False Negatives    True Negatives
#                                   (FN)              (TN)
#               type-II error (beta) --^
#                 power of test = 1-beta
# y_actual1  2  3
#         2 47  3  --->  TP  FP
#         3  1 49  --->  FN  TN
TP = TAP[1,1];   FP =TAP[1,2];   FN = TAP[2,1];   TN = TAP[2,2]
##== 準確率等
accuracy = (TP+TN) / (TP+TN+FP+FN);   accuracy   #-- = 0.96 準確率(accuracy)
P = TP / (TP+FP);   P   #-- = 0.94          精确率(precision) -- Positive 中的 True(2)
R = TP / (TP+FN);   R   #-- = 0.9791667     召回率(recall)    -- 有多少 (2) 被召回(recall)
F1 = 2*P*R/(P+R);   F1  #-- = [1] 0.9591837 F1-value


########## (3) 決策樹的進階 [殷,7.2, 7.7] ##########

#####=====*(3A) 不純度(impurity)計算 [殷,7.2] =====#####
##== 三種數據不純度(impurity)度量: (1)Gini, (2)熵/亂度(Entropy), (3)分類誤差(Error)
#    -- 當分類完成時,不純度達到最小值0
##== 增益(gain) = 數據分裂前的不純度 - 數據分裂後的不純度
##== (1) Gini, 第一種不純度(impurity): Gini(x) = 1-sum(p(xi|x)^2) [殷,7.2]
#--      -- (1a) 練習數據 [殷,p.169]
bank = data.frame(house=c("yes","no","no","yes","no","no","yes","no","no","no"),
                  marriage=c("single","married","single","married","divorced","married",
                             "divorced","single","married","single"),
                  income=c(125,100,70,120,95,60,220,85,75,90),
                  debt=c("no","no","no","no","yes","no","no","yes","no","yes"))
bank$house = as.factor(bank$house);      bank$marriage = as.factor(bank$marriage)
bank$debt = as.factor(bank$debt);        bank   #-- 必須轉為 factor, 決策樹函式才能處理
#    house marriage income debt
# 1    yes   single    125   no
# 2     no  married    100   no
# 3     no   single     70   no
# 4    yes  married    120   no
# 5     no divorced     95  yes
# 6     no  married     60   no
# 7    yes divorced    220   no
# 8     no   single     85  yes
# 9     no  married     75   no
# 10    no   single     90  yes
#--      -- (1b) Gini係數的函式定義 [殷,7.2]
Gini <- function(tblX) { g1=1-sum((tblX^2)/sum(tblX)^2); return(g1)};  Gini(table(bank$debt))  #-- [1] 0.42 [殷7-55]
Gini2 <- function(X,A,a1,a2) {  AX = table( (A %in% a1), X );   AX
  #       no yes
  # FALSE  2   2
  # TRUE   5   1
  g2 = sum(apply(AX,1,Gini)*rowSums(AX)/sum(AX));   g2   #-- [1] 0.3666667
  return(g2)  
}
#--      -- (1c) Gini係數的計算 [殷,pp.187-190]
Gini(table(bank$debt)) - Gini2(bank$debt, bank$house, c("yes"), c("no"))   #-- [1] 0.07714286 [殷7-57]
Gini(table(bank$debt)) - Gini2(bank$debt, bank$marriage, c("married","divorce"), c("single"))   #-- [1] 0.12 [殷7-58] <-- 增益最大
Gini(table(bank$debt)) - Gini2(bank$debt, bank$marriage, c("single","divorce"), c("married"))   #-- [1] 0.05333333 [殷7-59]
Gini(table(bank$debt)) - Gini2(bank$debt, bank$marriage, c("single","married"), c("divorce"))   #-- [1] 0.02 [殷7-60]
Gini(table(bank$debt)) - Gini2(bank$debt, cut(bank$income,breaks=c(0,65,300)), factor("(0,65]"), factor("(65,300]"))   #-- [1] 0.02 [殷7-61]
Gini(table(bank$debt)) - Gini2(bank$debt, cut(bank$income,breaks=c(0,92.5,300)), factor("(0,92.5]"), factor("(92.5,300]"))   #-- [1] 0.02 [殷7-61]
##== (2) Entropy, 第二種不純度(impurity): Entropy(x) = -sum(p(xi|x)*log2(p(xi|x)) [殷,7.2]
#--      -- (2a) 練習數據 [殷,p.176]
weather = read.csv("weather.csv");   dim(weather);   weather   #-- [1] 14  6
#    day  outlook temperature humidity   wind playball
# 1   D1    Sunny         Hot     High   Weak       No
# 2   D2    Sunny         Hot     High Strong       No
# 3   D3 Overcast         Hot     High   Weak      Yes
# 4   D4     Rain        Mild     High   Weak      Yes
# 5   D5     Rain        Cool   Normal   Weak      Yes
# 6   D6     Rain        Cool   Normal Strong       No
# 7   D7 Overcast        Cool   Normal Strong      Yes
# 8   D8    Sunny        Mild     High   Weak       No
# 9   D9    Sunny        Cool   Normal   Weak      Yes
# 10 D10     Rain        Mild   Normal   Weak      Yes
# 11 D11    Sunny        Mild   Normal Strong      Yes
# 12 D12 Overcast        Mild     High Strong      Yes
# 13 D13 Overcast         Hot   Normal   Weak      Yes
# 14 D14     Rain        Mild     High Strong       No
#--      -- (2b) 熵/亂度(entropy)的函式定義 [殷,7.2]
Entropy <- function(tblX) return(sum(-log(tblX/sum(tblX),base=2)*(tblX/sum(tblX)),na.rm=T));   Entropy(table(weather$playball))   #-- [1] 0.940286 [殷7-12]
Entropy2 <- function(X,A) {  AX = table( A, X );   AX
  #        No Yes
  # Strong  3   3
  # Weak    2   6
  g2 = sum(apply(AX,1,Entropy)*rowSums(AX)/sum(AX));   g2   #-- [1] 0.3666667
  return(g2)  
}
#--      -- (2c) 熵/亂度(entropy)的計算 [殷,p.172]
Entropy(table(weather$playball)) - Entropy2(weather$playball, weather$wind)        #-- [1] 0.04812703 [殷7-16,7-20]
Entropy(table(weather$playball)) - Entropy2(weather$playball, weather$outlook)     #-- [1] 0.2467498  [殷7-17]
Entropy(table(weather$playball)) - Entropy2(weather$playball, weather$temperature) #-- [1] 0.02922257 [殷7-18]
Entropy(table(weather$playball)) - Entropy2(weather$playball, weather$humidity)    #-- [1] 0.1518355  [殷7-19]
#   ---> 第一層選擇 outlook屬性進行劃分
weatherS = weather[weather$outlook=="Sunny",];   dim(weatherS)   #-- [1] 5 6
Entropy(table(weatherS$playball)) - Entropy2(weatherS$playball, weatherS$wind)        #-- [1] 0.01997309 <-*-> 0.371[殷7-23]
Entropy(table(weatherS$playball)) - Entropy2(weatherS$playball, weatherS$temperature) #-- [1] 0.5709506  [殷7-22]
Entropy(table(weatherS$playball)) - Entropy2(weatherS$playball, weatherS$humidity)    #-- [1] 0.9709506  [殷7-21]
#   ---> Sunny的第二層選擇humidity屬性進行劃分
weatherR = weather[weather$outlook=="Rain",];   dim(weatherR)   #-- [1] 5 6
Entropy(table(weatherR$playball)) - Entropy2(weatherR$playball, weatherR$wind)        #-- [1] 0.9709506  [殷 圖7-9]
Entropy(table(weatherR$playball)) - Entropy2(weatherR$playball, weatherR$temperature) #-- [1] 0.01997309  
Entropy(table(weatherR$playball)) - Entropy2(weatherR$playball, weatherR$humidity)    #-- [1] 0.01997309  
#   ---> Rain的第二層選擇wind屬性進行劃分
weatherRW = weather[(weather$outlook=="Rain")&(weather$wind=="Weak"),];   dim(weatherRW)   #-- [1] 3 6
Entropy(table(weatherRW$playball))   #-- 0 ----> 亂度為0, 表示分配完畢
weatherRS = weather[(weather$outlook=="Rain")&(weather$wind=="Strong"),]; dim(weatherRS)   #-- [1] 2 6
Entropy(table(weatherRS$playball))   #-- 0 ----> 亂度為0, 表示分配完畢
##== (3) 分類誤差 Error(x) = 1 - max(p(xi|x) [殷,7.2]
#--      -- 得到的最佳值，和Entropy所得者相同

#####===== (3B) 組合方法分類 [殷,7.7] =====#####
##== 組合方法/整體學習(Ensemble Learning): 通過多個分類器的預測，以各基分類器的預測進行投票，來提高分類準確度的技術
##== (1) 裝袋Bagging (Bootstrap Aggregating) 自助聚集: 匯總學習 [Breiman,1994][黃文, Chap.10]
#        -- 根據均勻概率分布，從數據中重覆抽樣，各訓練一個基分類器，再依其預測結果投票
library(adabag)
iris.bag = bagging(Species ~ ., data=iris, mfinal=5)    #-- mfinal:基本分類器的個數
names(iris.bag)   #-- [1] "formula" "trees" "votes"  "prob"  "class" "samples"  "importance" "terms" "call"      
length(iris.bag$trees)   #-- [1] 5
iris.bag$importance
# Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
#      89.5833      10.4167       0.0000       0.0000 
iris.bag$trees[[1]]
# 1) root 150 92 virginica (0.3200000 0.2933333 0.3866667)  
# 2) Petal.Length< 2.45 48  0 setosa (1.0000000 0.0000000 0.0000000) *
#   3) Petal.Length>=2.45 102 44 virginica (0.0000000 0.4313725 0.5686275)  
# 6) Petal.Length< 4.95 50  6 versicolor (0.0000000 0.8800000 0.1200000)  
# 12) Petal.Width< 1.65 42  0 versicolor (0.0000000 1.0000000 0.0000000) *
#   13) Petal.Width>=1.65 8  2 virginica (0.0000000 0.2500000 0.7500000) *
#   7) Petal.Length>=4.95 52  0 virginica (0.0000000 0.0000000 1.0000000) *
Species.new.bag = predict(iris.bag, newdata=iris)   #-- prediction
Species.new.bag$confusion
# Predicted Class setosa versicolor virginica
#      setosa         50          0         0
#      versicolor      0         48         3
#      virginica       0          2        47
##== (2) 提升Adaboost (Adaptive Boosting) 自適應增加模型 [Breiman,1996]
#        -- 每一個訓練樣本一個權值，依是否正確分類調整權重，越難分類權重越高，使某些分類器更對其分類
iris.ada = boosting(Species ~ ., data=iris, mfinal=5)    #-- mfinal:基本分類器的個數
iris.ada$trees[[1]]
# 1) root 150 86 virginica (0.28666667 0.28666667 0.42666667)  
# 2) Petal.Length< 4.75 80 37 setosa (0.53750000 0.46250000 0.00000000)  
# 4) Petal.Length< 2.45 43  0 setosa (1.00000000 0.00000000 0.00000000) *
#   5) Petal.Length>=2.45 37  0 versicolor (0.00000000 1.00000000 0.00000000) *
#   3) Petal.Length>=4.75 70  6 virginica (0.00000000 0.08571429 0.91428571) *
plot.iris(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length")
abline(h=4.75,col="purple");  abline(h=2.45,col="red");  
iris.ada$trees[[2]]
# ...
# 1) root 150 94 versicolor (0.29333333 0.37333333 0.33333333)  
# 2) Petal.Length< 2.45 44  0 setosa (1.00000000 0.00000000 0.00000000) *
#   3) Petal.Length>=2.45 106 50 versicolor (0.00000000 0.52830189 0.47169811)  
# 6) Petal.Width< 1.65 53  2 versicolor (0.00000000 0.96226415 0.03773585) *
#   7) Petal.Width>=1.65 53  5 virginica (0.00000000 0.09433962 0.90566038) *
plot.iris(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length")
abline(h=2.45,col="purple");  segments(1.65,2.45,1.65,7,col="red");
Species.new.ada = predict(iris.ada, newdata=iris)   #-- prediction
Species.new.ada$confusion
#           Observed Class
# Predicted Class setosa versicolor virginica
# setosa         50          0         0
# versicolor      0         50         0
# virginica       0          0        50
##== (3) random forest 隨機森林 [Breiman,2001]
#        -- 每一棵決策樹依賴於獨立抽樣，所有的樹具有相同的分布的隨機向量的值，依各樹的預測結果進行投票
# install.packages("randomForest")
library(randomForest)
set.seed(777)
iris.rf = randomForest(Species ~ ., data=iris, importane=T)  
names(iris.rf)
# [1] "call"            "type"            "predicted"       "err.rate"        "confusion"       "votes"           "oob.times"      
# [8] "classes"         "importance"      "importanceSD"    "localImportance" "proximity"       "ntree"           "mtry"           
# [15] "forest"          "y"               "test"            "inbag"           "terms"          
print(iris.rf)
# Call:
#   randomForest(formula = Species ~ ., data = iris, importane = T,      proximity = T) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# OOB estimate of  error rate: 4%
# Confusion matrix:
#   setosa versicolor virginica class.error
# setosa         50          0         0        0.00
# versicolor      0         47         3        0.06
# virginica       0          3        47        0.06
treesize(iris.rf)
# [1]  8  6  6 10 10  7  8  9  9  8  8  7  8 10  9  7 12  8  8  7  9  9  7  6  5  9 10  8 11  8  9  6 11 10  7  8  7  8  6 14  9  9  7  8  8  8
# [47]  8 11  9  8  8 10  7 14  8 10  8  9  8  8 11  9  5  6 10  7 13  8 10 11  7 10 10 10 10  8 11  7  7 12  9 11 10  8 11  8  9  9 10 11  6 10
# [93]  8 14  8  8  5  9  8  6  9  6  9  8  9  7 11  7 11  7  8 12  6 11  7  8  6  7  8 11  6  9 10  6  7  6 10  8 12 12  6  9  9  8  6  6  9  5
# [139] 13 13  8  9  7 10  9  6  7 10 12 10 10  8 11  9  6  9  8  8 11 10 14  6  8 11  4 11 10 11  9  7 12 11  8  8  5 10  5 12  9 10  6 10 10 11
# [185]  9 11  6 11 10  7  8  9 12  3  6  8  8 10  6 11 10  6  8 11  9 11 10 10  8  7  6 12 13  5  7  5  9  7  9  8 12  6 12 13  8  8 12 10 10 10
# [231]  4 10 13  6 11 13 10  9  8  7  8 11  8  6  4  8  7  7 15  4 11 10  9  9  9 10  5  6 10  7 13 10  8 14 12 13 10 10 12  9 10 14  6  7 10  8
# [277]  9 11  6 13 11 13 10 13  9 11 13  6  9 10  6 10  7  8  8  8  9  6  9 10  5  9  6 11 11  7 10  5  8 12 11  6  8  6 14  8  5 10 11 13  4  9
# [323]  6 10  9  8  7 10  8  6 12  6  8  8  9  7 13 10 14  8  9  6 12 12 10 11  9 10  7 10  9 11  6 10  9 10  9 10 11 17 10 13  9  7  5 11 13 12
# [369]  7 11  7  8 11  8 11 10 14 10 10 12  5  7  7  7  6 10  9  9 10 11 10  8  4 14  4  8  8  8 14 11 12  9 17  6  8  9  6 12  5  9 10  8  9  8
# [415]  6 12  8  9 10  9  7 12  6  8 10  7  5  9 10 10  6  8 11  9 10  6  8  9  7  6  9  8 11 12  8 12 13  8  8  7 10  8  9  8  9 14  6 11  6  9
# [461] 10  6  6 10  5  8  7 11  7  7  8 13  8  9 10  4 10  9  6  8  6  9  6 10 10  9  6  9 13  9  7  7  7  5  8 11 10  6  6  9
getTree(iris.rf, 1, labelVar=TRUE)
#      left daughter right daughter    split var split point status prediction
#   1              2              3 Sepal.Length        5.45      1       <NA>
#   2              4              5  Petal.Width        0.75      1       <NA>
#   3              6              7 Petal.Length        4.75      1       <NA>
#   4              0              0         <NA>        0.00     -1     setosa
#   5              8              9 Sepal.Length        4.95      1       <NA>
#   6             10             11  Petal.Width        0.65      1       <NA>
#   7             12             13  Petal.Width        1.70      1       <NA>
#   8              0              0         <NA>        0.00     -1  virginica
#   9              0              0         <NA>        0.00     -1 versicolor
#   10             0              0         <NA>        0.00     -1     setosa
#   11             0              0         <NA>        0.00     -1 versicolor
#   12            14             15 Sepal.Length        6.05      1       <NA>
#   13             0              0         <NA>        0.00     -1  virginica
#   14             0              0         <NA>        0.00     -1 versicolor
#   15             0              0         <NA>        0.00     -1  virginica
Species.new.rf = predict(iris.rf, newdata=iris)   #-- prediction


########## (4) 人工神經網絡(Artificial Neural Networks, ANN)分類 [殷,7.5, 7.6, 7.9.5, 7.9.6] ##########

#####=====*(4A) 神經網絡原理 [殷,7.5] =====#####
##== 處理單元/神經元(neuron)原理
#    -- 匯集來自其他神經元(i)的信息(xi)，與本神經元的相互作用強度/權重為wi --> sum(xi*wi) = a (activation激發)
#    -- 大於某閾值 w0 即為產生輸出 y --> y = f(a-w0) = f( sum(xi*wi) - w0 )
#       -- 一個神經元相當於一個感知機(perceptron)，
#                          是一個超平面(hyperplane)，將輸入數據 切成 正/負兩類(positive/negative halfspace)
#    .. 神經元的輸入/輸出 xi/y 可為 0/1 (二元binary), (-1)/1 (bipolar雙極), 任意實數 (linear,線性)
##== 神經網絡(neural network)架構
#    -- 前向網絡(feedforward network, multi-layer perceptron, MLP): 包括 輸入層/隱含層(hidden layer) / 輸出層
#    -- 反饋網絡(feedback/recurrent network): 輸出層到輸入層中存在反饋(feedback)

#####===== (4B) 單隱藏層神經網路: 以nnet建模與預測 =====#####
library(nnet)
set.seed(101);   
iris.nnet = nnet(Species~., data=iris, size=2, rang=1/max(abs(iris[,1:4])), decay=0.05, maxit=50000)
Species.new.nnet = predict(iris.nnet, iris[,1:4], type="raw")     
table(apply(Species.new.nnet,1, which.max), as.integer(iris$Species))
#    1  2  3
# 1 50  0  0
# 2  0 48  1
# 3  0  2 49
##== iris.nnet的神經鍵與繪圖
summary(iris.nnet)
# a 4-2-3 network with 19 weights
# options were - softmax modelling  decay=0.05
# b->h1 i1->h1 i2->h1 i3->h1 i4->h1 
# 0.28   0.46   1.47  -2.38  -1.08 
# b->h2 i1->h2 i2->h2 i3->h2 i4->h2 
# 4.14   1.53   1.82  -2.66  -3.62 
# b->o1 h1->o1 h2->o1 
# -2.50   5.45   0.91 
# b->o2 h1->o2 h2->o2 
# -1.07  -4.70   4.78 
# b->o3 h1->o3 h2->o3 
# 3.57  -0.75  -5.69 
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
par(mar=numeric(4),family='serif')
plot.nnet(iris.nnet, pos.col='darkgreen',neg.col='red', alpha.val=0.7, rel.rsc=15,
          circle.cex=3, cex=1, circle.col='yellow')

#####===== (4C) 單隱藏層神經網路: 以neuralnet建模與預測 =====#####
library(neuralnet)
head(class.ind(iris$Species))
iris3 <- cbind(iris, class.ind(iris$Species));   head(iris3)
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris.bpn <- neuralnet(formula=formula.bpn, data=iris3, hidden=c(2), learningrate=0.01, threshold=0.01, stepmax=50000 )
Species.new.bpn = compute(iris.bpn, iris[,1:4])     
table(apply(Species.new.bpn$net.result,1, which.max), as.integer(iris$Species))
#    1  2  3
# 1 50  0  0
# 2  0 49  1
# 3  0  1 49
plot(iris.bpn)
names(iris.bpn);     iris.bpn$weights

#####===== (4D) 支持向量機(Support Vector Machine, SVM) [殷,7.6]  =====#####
##== 支持向量機(SVM): 以一組事先選擇的非線性映射(kernel核函數)，將輸入向量映射到高維特徵空間，進行最佳分類
#    -- 支持向量(Support Vector): 區隔各(正/負)分類的最大間隔超平面
library(e1071)
##== 線性SVM(Support Vector Machine)支持向量機: kernel=線性函數
iris.svm.linear <- svm(Species ~ Petal.Length + Petal.Width, data = iris, kernel = "linear")
plot(iris.svm.linear, iris, Petal.Width~Petal.Length, slice=list(petal.width=1,pepal.length=2))
##== RBF SVM(Support Vector Machine)支持向量機: kernel-RBF函數
iris.svm.radial <- svm(Species ~ Petal.Length + Petal.Width, data = iris, kernel = "radial")
plot(iris.svm.radial, iris, Petal.Width~Petal.Length, slice=list(petal.width=1,pepal.length=2))


########## (5) 貝葉斯分類 [殷,7.4, 7.9.4] ##########

#####===== (5A) 貝葉斯分類(Bayes classifier) [殷,7.4]  =====#####
##== 貝葉斯分類法的特點
#    -- 利用領域知識和其他先驗信息，計算假設概率，分類結果是領域知識和數據樣本信息的綜合體現
#    -- 利用有向圖，來表現各變量之間的依賴關係，以概率分布表示依賴關係的強弱 (Bayesian Network)
#    -- 可進行增量學習，數據樣本可以增量地提高或降低某種假設的估計
##== 貝葉斯定理: P(Ci|X) = P(Ci)*P(X|Ci)/P(X)
#    -- P(Ci): Ci分類的先驗概率(a priori probability)
#    -- P(X): 樣本X出現的概率，   P(X|Ci): 在Ci分類下會出現X的機率
#    -- P(Ci|X): 在條件X下(數據為X的條件下)，為Ci分類的後驗概率(a posteriori probability)
##== 素樸貝葉斯分類法(Naive Bayes): 指派為 Ci分類, 若對於所有j<>i, P(Ci)*P(X|Ci) > P(Cj)*P(X|Cj)
#    -- 偽代碼: [殷,p.197]
##== 計算例: (3A)的bank例:
#    -- P(debt=yes) = 3/10, P(debt=no) = 7/10
#       P(house=yes|debt=no) = 3/7, P(house=no|debt=no) = 4/7  
#       P(house=yes|debt=yes) = 0,  P(house=no|debt=yes) = 1  
#       P(marriage=single|debt=no) = 2/7, P(marriage=divorced|debt=no) = 1/7, P(marriage=married|debt=no) = 4/7  
#       P(marriage=single|debt=yes) = 2/3, P(marriage=divorced|debt=yes) = 1/3, P(marriage=married|debt=yes) = 0
#       debt=no:  income均值=110, income方差=2975
#       debt=yes: income均值=90, income方差=25
#    -- X = (house=no, marriage=married, income=120K)
#    --> P(X|debt=no) = P(house=no|no)*P(marriage=married|no)*P(income=120K|no) = (4/7)*(4/7)*0.0072 = 0.0024
#        P(X|debt=yes) = P(house=no|yes)*P(marriage=married|yes)*P(income=120K|yes) = (1)*(0)*1.2*10^(-9) = 0
#    --> P(X|debt=no)*P(debt=no) = 0.0024*(7/10) = 0.00168
#        P(X|debt=yes)*P(debt=yes) = 0*(3/10) = 0
#    --> X 分類為 debt=no

#####===== (5B) 素樸貝葉斯分類器(Naive Bayes classifier) [殷,7.9.4]  =====#####
library(klaR)
iris.naiveB = NaiveBayes(Species~., iris)
Species.new.naiveB = predict(iris.naiveB, iris)
Species.new.naiveB$class[46:55]
# [1] setosa     setosa     setosa     setosa     setosa     versicolor versicolor virginica  versicolor versicolor
# Levels: setosa versicolor virginica
Species.new.naiveB$posterior[50:55,]
#             setosa   versicolor    virginica
# [1,]  1.000000e+00 7.713456e-18 3.349997e-25 --> setosa
# [2,] 4.893048e-107 8.018653e-01 1.981347e-01 --> versicolor
# [3,] 7.920550e-100 9.429283e-01 5.707168e-02 --> versicolor
# [4,] 5.494369e-121 4.606254e-01 5.393746e-01 --> virginica
# [5,]  1.129435e-69 9.999621e-01 3.789964e-05 --> versicolor
# [6,] 1.473329e-105 9.503408e-01 4.965916e-02 --> versicolor
table( Species.new.naiveB$class, Species)
#            Species
#            setosa versicolor virginica
# setosa         50          0         0
# versicolor      0         47         3
# virginica       0          3        47

#####===== (5C) 最大似然函式(Maximum Likelihood Estimate, MLE) =====#####
library(bbmle)
##== 似然函式(Likekihood Function): L(theta) = prod f(xi|theta), theta in THETA
##-- 最大似然函式(Maximum Likelihood Estimate, MLE) theta^: theta的點估測
#    -- 最大值性質: 一次微分 L'(theta^) = 0, 且二次微分 L"(theta^) < 0
##-- Log似然函式(Log-Likelihood): l(theta) = ln L(theta) can get the same solutions
X = iris;   X$Species = as.integer(X$Species);   colnames(X) <- c('x1','x2','x3','x4','y');   head(X,2) 
#    x1  x2  x3  x4 y
# 1 5.1 3.5 1.4 0.2 1
# 2 4.9 3.0 1.4 0.2 1
##== 似然函式的函式定義例
LL <- function(w0,w1,w2,w3,w4,mu,sigma) {
  R = y - (w0+w1*x1+w2*x2+w3*x3+w4*x4)    #-- 線性回歸式 y~x1+x2+x3+x4, R 為 回歸式的殘差(residual)
  score = suppressWarnings(dnorm(R,mu,sigma, log=TRUE))  #-- log似然函式 為 R的常態分布
  return(-sum(score))                                    #-- 加總log-Likelihood, 相當於Likelihood乘積
}
##== MLE的訓練 (X-->iris.mle2)
iris.mle2 <- mle2(LL, start=list(w0=0,w1=0,w2=0,w3=0,w4=0,mu=0,sigma=1), fixed=list(mu=0), data=X) # MLE estimation
summary(iris.mle2)
# Coefficients:
#          Estimate Std. Error z value     Pr(z)    
#   w0     1.186492   0.201402  5.8912 3.835e-09 ***
#   w1    -0.111906   0.056679 -1.9744   0.04834 *  
#   w2    -0.040080   0.058687 -0.6829   0.49464    
#   w3     0.228645   0.055896  4.0906 4.303e-05 ***
#   w4     0.609253   0.092872  6.5602 5.375e-11 ***
#   sigma  0.215420   0.012438 17.3200 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# -2 log L: -34.87371 
##== 抽取iris.mle2模型中的線性回歸參數 (iris.mle2-->w/w0)
w = slot(iris.mle2,"coef")[2:5];   w   
#         w0          w1          w2          w3          w4  
# 1.18649224 -0.11190554 -0.04007962  0.22864503  0.60925277   
w0 = slot(iris.mle2,"coef")[1];    w0   #-- [1] 1.186492
##== MLE的預測 (iris.mle2-->w/w0+X-->y_predict)
y_predict = sapply(1:dim(X)[1], FUN=function(k) as.vector(sum(w*X[k,1:4])+w0));   y_predict[46:55]
# [1] 1.0319857 0.9511541 0.9854256 0.9099164 0.9366554 2.2024842 2.2848238 2.3243370 2.1854374 2.3125296
##== 混淆矩陣評估
table(round(y_predict,0), iris$Species)
#   setosa versicolor virginica
# 1     50          0         0
# 2      0         48         2
# 3      0          2        48

#####===== (5D) 各種分類器之間的關係 =====#####
# [Pieter Eykhoff. (1974). System Identification: Parameter and State Estimation. NY:Wiley]
##== 事件/量測概率(event probability) P(Xt): 在某時間t發生量測值(measurement)Xt的概率
##-- 先驗概率(a priori probability) P(Ci): 在量測(measure)前某類別Ci會出現的概率
##-- 後驗概率(a posteriori probability) P(Ci|Xt): 在量測值為Xt後,類別Ci出現的概率
##== 貝葉斯定理(Bayesian Rule): P(Ci|Xt) = P(Ci)*P(Xt|Ct) / P(Xt)
###=== 各種分類器(classifiers)的條件與計算式 [Ekyhoff, Table 5-1, p.152] ===###
#
#        分類器 ----- 事先條件(a priori knowledge) ----- 分類器計算式(objective function)
##== (1) Bayes分類器 ----- P(Ci),P(X|Ci),loss(ai|Cj) ----- ai = arg min_ai risk(ai|X) = sum(loss(ai|Cj)*P(Cj|X))
#     |  -- ai: 決策(decision)或動作(action)
#     |
#     |== 當 損失函式loss(ai|Cj) 均相等, 或 沒有損失函式時，Bayes分類器 化簡為 MAP分類器  
#     V
##== (2) MAP(Maximum A Posteriori)分類器 --- P(Ci), P(X|Ci) ----- max(P(Ci|X)) = max(P(Ci)*P(Xt|Ct)/P(Xt))
#     |  -- 上述(5A-5B)的素樸貝葉斯分類器，其實是 MAP分類器
#     |
#     |== 當 沒有 先驗概率 P(Ci)時，MAP分類器 化簡為 ML分類器 
#     V
##== (3) ML(Maximum Likelihood)分類器 --- P(X,Ci) ----- max(P(X|Ci)) = max(log(P(X,Ci)))
#     |  -- 上述(5C)最大似然函式，不僅可以用在常態分布，也可用在二項分布等
#     |
#     |== 當 P(X,Ci)為 常態分布時，ML分類器 化簡為 Markov分類器
#     V
##== (4) Markov(Minimum Variance)分類器 --- P(X,Ci)=n(0,R) ----- min( t(y-W*X)*inv(R)*(y-W*X) )
#     |  -- 可以求得 W = inv(t(X)*inv(R)*X) * t(X)*inv(R)*y, 即上述(4)中若取無隱含層的神經網絡公式
#     |
#     |== 當 常態分布中的變異數/標準差均相等時，則可化為LS(最小平方)/回歸分類器，即(HUT07)中的回歸
#     V
##== (5) Least Square (最小平方)/回歸分類器 --- R = (sigma^2)*I ----- W = inv(t(X)*X) * t(X)*y


########## (6) 其他模型與大數據的全貎 [請參考投影片] ##########

#####===== (6A) 監督式學習模型比較 =====#####

#####===== (6B) 網絡模型 =====#####

#####===== (6C) 時序模型 =====#####

#####===== (6D) 地圖模型 =====#####

#####===== (6E) 大數據的全面觀 =====#####


########## (R) 单元的复习 ##########

#####===== (R-HW) 演练作业HW =====#####
## 试就你爬取解析的網頁數據，或教材中提供的數據，繼續上一章的作業:
##== (HWA) 就上一章(HWA)找出來的變量，而且具高相關性者(HWB)，將(HWC)的回歸模型改為本章所教授的各種模型
##== (HWB) 比較一下這些訓練後的模型(含回歸模型)
##== (HWC) 如果可能的話，將這些模型想辦法整合進第四/五章的儀表板中(HW4/HW5D)

#####===== (R-RV) 重点复习RV =====#####
##== (RVA) 決策樹模型，是一種 "監督式學習模型"。
##== (RVB) 決策樹模型中的 "葉結點(終結點)"，代表一條規則(rule)。
##== (RVC) 在決策樹的不純度計算中，不論是Gini或Entropy，都和數據對象的 "概率"有關。
##== (RVD) 一個神經元相當於一個 "超平面(hyperplane)"，將數據空間分割為正/負兩個半平面。
##== (RVE) 在素樸貝葉斯分類法中，會計算數據在每個類中的P(Ci)*P(X|Ci)，這是一種 "後驗概率(a posteriori probability)"。
##== (RVF) 不論是監督式或無監督式模型，都是大數據分析中的第四步驟 "數據模型(data modeling)"。
