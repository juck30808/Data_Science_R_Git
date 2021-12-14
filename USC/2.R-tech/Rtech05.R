# Rtech05.R: 大數據實務技術 - 05: 監督式學習與數據回歸
# Jia-Sheng Heh (賀嘉生), 12/11/2020, revised from HUT07.R
setwd("/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data")


########## (P) 課前準備 ########## 
##== (1) 從微信群組下載 本份講義程式檔 Rtech05.R 放入本門課課程目錄，並以 RStudio開啟
##== (2) 若見到本編程檔為中文亂碼，請以 File-->Reopen with Encoding --> UTF8，則可看到中文碼顯示
##== (3) 修改本程式第4行，設定工作目錄為 本門課工作目錄
##== (4) 下載本門課所需之軟件包至本機備用 -- 下行安裝指令只需執行一次
# install.packages( c("data.table") )    
##== (5) 從微信群組下載 RR1_500r6810.csv, RR501_1000r6217.csv, RR1001_1500r7707.csv, 
#                       RR"1501_2000r8262.csv 數據檔，放入 本門課工作目錄，作為本課程待用


########## (1) 機器學習與監督式學習 ##########

#####=====*(1A) 回歸、分類和聚類的關係 [殷,6.1] =====#####
##== 課本上的名詞
#    -- 回歸(regression): 通過函數表達連續數據映射的關係，來發現屬性值之間的依賴關係
#       -- 預測回歸: 建立連續值函式的預測模型，可預測缺失的/難以獲得的數值數據值
#    -- 分類(classification): 找出一組離散數據對象的共同特點，按照分類模式將其劃分為不同的類
#       -- 預測分類: 找出描述和區分數據類/概念的模型，以預測未知類標號對象的類標號
#    -- 聚類(clustering): 把數據對象集，劃分成多個組/簇的過程，使得簇內的對具有很高的相似性
#       -- 與分類類似，但每個客戶(對象)的類標號是未知的，需要發現這些分組(簇)
##== 課本上的另一組定義
#    -- 有監督學習(supervised learning)
#       -- 監督: 訓練數據(觀察,測量等)都帶有標籤
#       -- 監督學習: 學習已經創建好的分類系統，學習後可以根據訓練集分類新數據
#    -- 無監督學習(unsupervised learning)
#       -- 無監督: 訓練集的類別(標籤)未知
#       -- 無監督學習: 給定一個觀察、測量等的數據集，以建立數據中存在的數據類或簇

#####===== (1B) (HUT06-1B) 數據模型(Data Model)符號 =====#####
##== 系統/模型/函數(System/Model/Function, M):  因變量/輸出數據y = M( 自變量/輸入數據u )  
#    -- (1) 訓練階段(Training/Learning/Modeling/Estimation Phase): (u, y) -> M
#             由輸入/輸出 u與y，求取(估測estimate)模型M#
#    -- (2) 應用階段(Prediction/Estimation/Production/Application Phase): (u_new, M#) -> y_predict
#             以所估測的模型M#與新的輸入 u_new，求取(估測)新的輸出 y_predict
##== 機器學習(Machine Learning)
#    -- (1) 監督式學習 (Supervised learning): 具範例(u,y), y為教師(teacher, desired output) y, 以求得y=M(u)
#           (1A)迴歸(regression):     y 為連續數據   --> 本單元 HUT07
#           (1B)分類(classification): y 為離散數據   --> 下一單元 HUT08   
#    -- (2) 無監督式學習 (Unsupervised learning): 無輸出y, 目標在於發掘輸入(u)的隱含特徵 --> 數據挖掘(Data Mining)
#           (2A)聚類(clustering):           計算數據u的相似度，以產生其分類。 --> 上兩單元HUT05
#           (2B)關聯規則(association rule): 計算多數據(ui-uj)間的關連。       --> 前一單元HUT06
#           (2C)數據序列(data sequencing):  計算多數據(ui-uj)間的時序關係。   --> 未列入本課程

#####===== (1C) 以數據模型符號重讀[殷,6.1]:模型回歸、分類和聚類的關係 =====#####
##== 課本上的名詞
#    -- 回歸(regression): 通過函數(M)表達連續數據映射(u->y)的關係，來發現屬性值之間的依賴關係
#                         : M(u)=y, y為連續值
#       -- 預測回歸: 建立連續值函數的預測模型(y=M(u))，可預測缺失的/難以獲得的數值數據值(u_new)
#                    : M(u_new)=y_predict
#    -- 分類(classification): 找出一組離散數據對象(y)的共同特點，按照分類模式(y=M(u))將其劃分為不同的類y
#                             : M(u)=y, y為離散值
#       -- 預測分類: 找出描述和區分數據類/概念的模型(y=M(u))，以預測未知類標號對象(u_new)的類標號(y_predict)
#                    : M(u_new)=y_predict
#    -- 聚類(clustering): 把數據對象集(u, 無y)，劃分成多個組/簇(求取M(u))的過程，使得簇內的對具有很高的相似性
#       -- 與分類類似，但每個客戶(對象)的類標號是未知(無y)的，需要發現這些分組(簇)(求取M(u))
##== 課本上的另一組定義
#    -- 有監督學習(supervised learning): M(u)=y
#       -- 監督: 訓練數據(觀察,測量等)都帶有標籤(y,可稱之為desired output, teacher)
#       -- 監督學習: 學習已經創建好的分類系統(y=M(u))，學習後可以根據訓練集分類新數據(y_predict=M(u_new))
#    -- 無監督學習(unsupervised learning): M(u) ... 無y
#       -- 無監督: 訓練集的類別(標籤,y)未知(無y,teacher)
#       -- 無監督學習: 給定一個觀察、測量等的數據集(u)，以建立數據中存在的數據類或簇(求取M(u))


########## (2) 回歸的基本概念 (以cars數據為例) ##########

#####===== (2A) 一個簡單的回歸數據例子: cars =====#####
##== 車速(speed)與停(刹)車距離(dist)的關係
dim(cars);   head(cars,3)   #-- [1] 50  2
#   speed dist
# 1     4    2
# 2     4   10
# 3     7    4
plot(dist ~ speed, data = cars)
##== 模型(model)/公式(formula) M 的表示法: y ~ u   其中 數據u是自變量, 數據y是因變量

#####=====*(2B) 訓練 線性回歸模型(linear model, lm()) =====#####
##== 訓練函式(lm)
cars.lm = lm(dist ~ speed, data = cars)  #--> linear model lm() 即為線性回歸的模型 M()
cars.lm
# Call:  lm(formula = dist ~ speed, data = cars)
# Coefficients:
#   (Intercept)        speed  
#       -17.579        3.932  
##== 線性回歸式
w = coef(cars.lm);  w
# (Intercept)       speed  --> 表示線性回歸式為   y =     w0      +       w1 * u  
#  -17.579095    3.932409                      dist = (-17.579095) + (3.932409) * speed
plot(dist ~ speed, data = cars)
abline(coef(cars.lm))          #---> 這條線 即為 線性回歸式     
##== 線性回歸的原理: LSE (最小平方誤差Least Square Error)
#    -- 將誤差平方(Squared Error) E = sum( ( y - (w0+w1*u) )^2 ) 最小化
#    -- 也就是將誤差 E 針對 w1, w0 微分, 使 dE/dw1 = 0 及 dE/dw0 = 0
#    -- 可以求得下列回歸係數 w1 和 w0 的公式
##== 線性回歸係數的公式
u = cars$speed;   mean(u)   #-- [1] 15.4
y = cars$dist;    mean(y)   #-- [1] 42.98
w1 = sum((u-mean(u))*(y-mean(y)))/sum((u-mean(u))^2) ;   w1   #-- = 5387.4/1370 = 3.932409
w0 = mean(y) - w1 * mean(u);   w0   #-- = -17.57909

#####===== (2C) 以線性回歸模型(cars.lm)進行預測(predict()) =====#####
##== 預測數據的準備
cars1 = data.frame(speed=c(6,8,21,30))
##== 預測函式(predict())
cars1$dist = predict(cars.lm, newdata=cars1)  
cars1
#   speed       dist
# 1     6   6.015358
# 2     8  13.880175
# 3    21  65.001489
# 4    30 100.393168
##== 預測的能力
range(cars$speed)   #-- [1]  4 25
#    -- 內插(interpolation): 預測在原數據範圍內的數據對象,有一定的參考性
#    -- 外插(extrapolation): 預測超過原數據範圍的數據對象,除非必要,儘量避免 (但比較有價值)


########## (3) 多元線性回歸(Multiple Linear Regression, MLR) (以iris數據為例) ##########

#####===== (3A) (HUT05:1C-1D) iris 數據 =====#####
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

#####=====*(3B) 輸出數據數值化與模型訓練 =====#####
##== 輸出數據數值化
iris$y = as.integer(iris$Species)
head(iris,2)
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species y 
# 1          5.1         3.5          1.4         0.2  setosa 1 
# 2          4.9         3.0          1.4         0.2  setosa 1 
##== 模型訓練
iris.lm = lm(y ~ Petal.Length + Petal.Width ,data=iris)
##== MLR模型
summary(iris.lm)
# Call:  lm(formula = y ~ Petal.Length + Petal.Width, data = iris)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.56449 -0.13898  0.01482  0.10172  0.58898 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   0.57378    0.05411  10.603  < 2e-16 ***
#   Petal.Length  0.17832    0.03863   4.616 8.47e-06 ***
#   Petal.Width   0.63042    0.08947   7.046 6.61e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.2248 on 147 degrees of freedom
# Multiple R-squared:  0.9257,	Adjusted R-squared:  0.9247 
# F-statistic: 916.3 on 2 and 147 DF,  p-value: < 2.2e-16
##==>多元線性回歸式： y = 0.57378 +  0.17832 * Petal.Length + 0.63042 * Petal.Width

#####===== (3C) 多元線性回歸模型的預測 =====#####
##== 原有數據的匹配(fit)
iris$y_predict = predict(iris.lm, newdata=iris[,c("Petal.Length","Petal.Width")]);   head(iris,2)
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species y y_predict
# 1          5.1         3.5          1.4         0.2  setosa 1 0.9495138
# 2          4.9         3.0          1.4         0.2  setosa 1 0.9495138
tail(iris,2)
#     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species y y_predict
# 149          6.2         3.4          5.4         2.3 virginica 3  2.986690
# 150          5.9         3.0          5.1         1.8 virginica 3  2.617982
##== iris類別分布圖上的多元線性回歸線
u1 = seq(0,2.5,length.out=20);   u2 = seq(1,7,length.out=20);   
y_predict = predict(iris.lm, newdata=data.frame(Petal.Length=u2, Petal.Width=u1));   y_predict
plot.iris(Petal.Width, Petal.Length, "Petal.Width", "Petal.Length")
for (k in 1:length(u1)) text(u1[k],u2[k],round(y_predict[k],2))


########## (4) 非線性回歸(NonLinear Regression, NLR) [殷,6.4] ##########

#####===== (4A) 非線性回歸模型的關係 =====#####
##== 非線性回歸的可能關係
#    -- y = b0 + b1*exp(x)
#    -- y =  + b1*ln(x)
#    -- y = b0 + b1*x + b2*x^2 + ... + bn*x^n
##== 非線性回歸 轉為 線性回歸: y = y = b0 + b1*u1 + b2*u2 + ... + bm*um

#####===== (4B) 彩色顯影的非線性回歸例 =====#####
##== 銀的光學密度zeta, 形成燃料eta的光學密度 ==> 求eta關於zeta的回歸方程
##== 試驗數據
zeta = c(0.05,0.06,0.07,0.1, 0.14,0.2, 0.25,0.31,0.38,0.43,0.47)
eta  = c(0.1, 0.14,0.23,0.37,0.59,0.79,1,   1.12,1.19,1.25,1.29)
##== 圖形關係
plot(zeta,eta,type="b")
##== 設定之回歸方程 (通常需要領域知識來建構): zeta = y = A*exp(b/x) = A*exp(b/eta),  b<0

#####=====*(4C) 非線性回歸式轉換為線性回歸式來求解 =====#####
##== 非線性回歸方程，等號兩方共取對數，可以得到 線性回歸方程
# ln(y) = ln(A) + b/x = ln(eta) = ln(A) + b/zeta
# Y     = a     + b*U, 其中...
##== 線性回歸方程 的 (轉換後)數據
Y = log(eta, base=exp(1) );   Y
# [1] -2.3025851 -1.9661129 -1.4696760 -0.9942523 -0.5276327 -0.2357223  0.0000000  0.1133287  0.1739533  0.2231436  0.2546422
U = 1/zeta;   U 
# [1] 20.000000  16.666667  14.285714  10.000000   7.142857   5.000000   4.000000   3.225806   2.631579   2.325581   2.127660
##== 檢視(U,Y)的相關係數(correlation coefficient):
r = cor(U,Y);   r   #-- = Ruy/sqrt(Ruu*Ryy) = (sum(U*Y)-k*mean(U)*mean(Y)) / ( sum((U-mean(U))^2) * sum((Y-mean(Y))^2) ) 
                    #-- = [1] -0.9982764    
                    #-- [殷,p.154] n=2 之 (n-2)=(11-2)=9 自由度 的相關性係數顯著性為0.602/0.735 (對顯著性水平=0.05/0.01)
##== 代入 線性回歸模型 的數據準備(形成數據框)
UY = data.frame(U=U,Y=Y);   head(UY,2)
#          U         Y
# 1 20.00000 -2.302585
# 2 16.66667 -1.966113
UY.lm = lm(Y~U, data=UY);   UY.lm  #-- 線性回歸模型 = Y = a + b*U = 0.5476 - 0.1459 * U
# Coefficients:
#   (Intercept)            U       
#        0.5476      -0.1459      
b = UY.lm$coefficients[2];  b      #-- [1] -0.1459 = b = Ruy/Ruu
a = UY.lm$coefficients[1];  a      #-- [2] 0.5476 = a = mean(Y) - b * mean(U)

#####===== (4D) 線性回歸式 轉換回 非線性回歸式 並進行預測 =====#####
A = exp(a);   A   #-- 1.729184
##==> 非線性回歸式: y = A*exp(b/x) = 0.729184*exp(-0.1459/zeta)
##== 圖形關係
plot(zeta,eta,type="b")
lines(zeta,A*exp(b/zeta),pch=19,type="b",col="red")


########## (5) 連續型監督式學習的評估 [殷,6.5] ##########

#####====== (5A) 回歸評估準則 [殷,6.5] =====#####
##== 準確率: 正地預新的或先前未見過的數據的屬性值
# -- 魯棒性: 給定噪聲數據或缺失數據時的正確預測能力
##== 速度: 使用的計算花費
# -- 可伸縮性: 給定大量數據時的有效建構能力
##== 可解釋性: 提供的理解和洞察能力，因為主觀很難評估

#####====== (5B) 數據的準備:以cars為例 (cars-->actual/predicted) =====#####
##== 真實的輸入數據(u,y-->actual)
u = cars$speed;   y = cars$dist;    actual = y;   actual
# [1]   2  10   4  22  16  10  18  26  34  17  28  14  20  24  28  26  34  34  46  26  36  60  80  20  26
# [26]  54  32  40  32  40  50  42  56  76  84  36  46  68  32  48  52  56  64  66  54  70  92  93 120  85
##== 訓練模型(u,y-->cars.lm)
cars.lm = lm(dist ~ speed, data = cars)  #--> linear model lm() 即為線性回歸的模型 M()
##== 輸入數據u的預測值(predicted)
predicted = as.vector( predict(cars.lm, newdata=data.frame(speed=u)) );   predicted
# [1] -1.849460 -1.849460  9.947766  9.947766 13.880175 17.812584 21.744993 21.744993 21.744993 25.677401
# [11] 25.677401 29.609810 29.609810 29.609810 29.609810 33.542219 33.542219 33.542219 33.542219 37.474628
# [21] 37.474628 37.474628 37.474628 41.407036 41.407036 41.407036 45.339445 45.339445 49.271854 49.271854
# [31] 49.271854 53.204263 53.204263 53.204263 53.204263 57.136672 57.136672 57.136672 61.069080 61.069080
# [41] 61.069080 61.069080 61.069080 68.933898 72.866307 76.798715 76.798715 76.798715 76.798715 80.731124

#####====== (5C) 單預測量的評估 =====#####
#--- 誤差 E: 真實樣本值與估計值的誤差也稱為“殘差(Residual)”
E = actual-predicted;   E[1:6]
# [1]  3.849460 11.849460 -5.947766 12.052234  2.119825 -7.812584 
##== 絕對誤差 aE: 此值是越小越好
aE = abs(actual-predicted);   aE[1:6]
# [1]  3.849460 11.849460  5.947766 12.052234  2.119825  7.812584  
##== 平方誤差 SE
SE = (actual-predicted)^2;   SE[1:6]
# [1]  14.818341 140.409699  35.375925 145.256334   4.493657  61.036468
##== 相對誤差 e: 一般此值應控制在1與-1之間
e = (actual-predicted)/actual;   e[1:6]
# [1]  1.9247299  1.1849460 -1.4869416  0.5478288  0.1324891 -0.7812584

#####======*(5D) 多預測量的評估--絕對誤差: 檢驗誤差/泛化誤差 =====#####
##== 均值絕對誤差(Mean Absolute Error/Deviation) MAE/MAD
MAE = 1/length(actual)*sum(abs(actual-predicted));   MAE   #-- [1] 11.58012
##== 平均絕對百分誤差(Mean Absolute Percentage Error) MAPE: 一般小於10%的可被接受
MAPE = 100/length(actual)*sum(abs((actual-predicted)/actual));   MAPE   #-- [1] 38.36881
##== 均方誤差(Mean Squared Error) MSE: 可以放大誤差的作用
MSE = 1/length(actual)*sum((actual-predicted)^2);    MSE   #-- [1] 227.0704
##== 均方根誤差(Root Mean Squared Error) RMSE
RMSE = sqrt(1/length(actual)*sum((actual-predicted)^2));   RMSE   #-- [1] 15.06886
##== 平均均方對數誤差(Mean Squared Logarithmic Error) MSLE
MSLE = 1/length(actual)*sum((log(1+actual,base=exp(1))-log(1+predicted,base=exp(1)))^2,na.rm=T);   
       MSLE  #-- [1] 0.1250257  因為log()中會有負值,相加時必須加上 na.rm=T (NA removed)

#####====== (5E) 多預測量的評估--相對誤差: 檢驗誤差/泛化誤差 =====#####
##== 相對絕對誤差(Relative Absolute Error)
RAE = sum(abs(actual-predicted))/sum(abs(actual-mean(actual)));   RAE   #-- [1] 0.5595125
##== 相對平方誤差(Relative Squared Error) / 正規化均方誤差(Normalized Mean Squared Error) NMSE = SSe/SSt: 越小越好
NMSE = sum((actual-predicted)^2)/sum((actual-mean(actual))^2);   NMSE   #-- [1] 0.3489206

#####====== (5F) 判定係數(Coefficient of Determination, R squared) =====#####
##== R Squared, 決定係數, 擬合度, 誤差百分比(percentage of reduced error, PRE) 
#    -- R^2 = 1 - SSe/SSt = 1 - 殘差變異量/總變異量 = SSregression/SSt = 回歸變異量/總變異量
#    -- 表示是預測值對實際值的解釋程度，越接定1, 預測值越接近真實值 
R_squared = 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2);   R_squared   #-- [1] 0.6510794
#    -- R_squared > 0.75:    回歸模型擬合度好, 可解釋程度較高
#    -- R_squared: 0.5-0.75: 回歸模可擬合度可接受, 但需再修正模型
#    -- R_squared < 0.5:     回歸模型擬合有問題, 需調整自變量再進行回歸


########## (6) 回歸模型的預測實務 ##########

#####===== (6A) (KDD1) (HUT04-3) 文本分析的準備 (RRlist-->RR) =====#####
library(data.table)
Rlist = c("RR1_500r6810","RR501_1000r6217","RR1001_1500r7707","RR1501_2000r8262")
for (k in 1:length(Rlist)) {
  print(paste0(">> reading file - ",Rlist[k],".csv..."))
  RRk = fread(paste0(Rlist[k],".csv"), encoding="UTF-8" )
  if (k==1) { RR = RRk }   else { RR = rbind(RR,RRk) }
}
dim(RR);   head(RR,2)       #-- 28996 / 464636 
#    V1 ktitle                title replyNo            Tdate      Tauthor       author authorclass
# 1:  1      1 自己的腰痠背痛自己救       0 2017-11-16 16:32 vivavida7749 vivavida7749    進階會員
# 2:  2      1 自己的腰痠背痛自己救       0 2017-11-16 16:32 vivavida7749          ujm    資深會員
#                date rPostNo authorcredit
# 1: 2017-11-16 16:32      #1           29
# 2: 2017-11-16 19:34      #2           32
# text
# 1: 板上偶爾有人在討論背痛或是腰痛問題\r\n然而上班族會腰痠背痛的原因不外乎：坐姿不正確 (我就是)、搬重物\r\n姊爬文看了一下，腰酸背痛的主因是肌肉緊繃，可透過『肌肉伸展』改善下背疼痛症狀。\r\n肌肉伸展最重要的部分是：核心肌群 和 大腿後肌群。\r\n講這麼多，還是來看教學影片最實在，感謝姊夫和翻譯者呀!只能幫分享回報了
# 2:                                                \n\nvivavida7749 wrote:\r\n板上偶爾有人在討論背...(恕刪)\n\r\n感覺是貼錯了<U+22EF><U+22EF>應該是下背痛那幾篇...Jeff和譯者都佛心來著\r\nhttps://youtu.be/Z3-mYrVjBEo\r\nhttps://youtu.be/gYsXOKAfxTs\r\nhttps://youtu.be/vw-LIovZ_Os\r\nhttps://youtu.be/oXj3O0quR0E

#####===== (6B) (KDD3) 數據轉換:整理每年月的貼文數/作者數 (RR-->RRym) =====#####
RR$ym = substr(RR$date,1,7)
library(data.table)   ##== 數據操弄(data wrangling)是數據分析中很重要的技巧
setDT(RR, key="ym")   #--- (1)以"ym"為數據表RR的索引關鍵(key)
RRym = RR[, .(nPost=length(V1), nTitle=length(unique(ktitle)), nAuthor=length(unique(author))), 
          by="ym"];   #--- (2)以ym為key, 計算post(V1)/ktitle/author的相異計數,放在RRym數據表中
dim(RRym);   head(RRym,2)  #-- [1] 125   4
#         ym nPost nTitle nAuthor
# 1: 2006-03    16      1      14
# 2: 2007-07    52      4      41

#####===== (6C) (KDD2) 數據探索:探索RRym各變量間的相關性 (RR-->RRym) =====#####
cor(RRym[,2:4])
#             nPost    nTitle   nAuthor
# nPost   1.0000000 0.8671691 0.9434940  #--> nAuthor和nPost/nTitle的相關性都超過九成
# nTitle  0.8671691 1.0000000 0.9130782
# nAuthor 0.9434940 0.9130782 1.0000000

#####=====*(6D) (KDD4) 數據模型:建立各變量間可能的回歸模型 (RRym-->RRym.lm/RRym.lm1/RRym.lm2) =====#####
RRym.lm = lm(nPost~nTitle+nAuthor, data=RRym[,2:4]);   RRym.lm
# Coefficients:
#   (Intercept)       nTitle      nAuthor  
#      -37.4457       0.3292       2.1734  
RRym.lm1 = lm(nPost~nAuthor, data=RRym[,2:4]);   RRym.lm1
# Coefficients:
#   (Intercept)      nAuthor  
#       -37.885        2.248 
RRym.lm2 = lm(nAuthor~nTitle, data=RRym[,2:4]);   RRym.lm2
# Coefficients:
#   (Intercept)       nTitle  
#        24.892        3.691  

#####===== (6E) (KDD5) 數據評估:評估各模型的擬合度(RRym.lm/RRym.lm1/RRym.lm2) =====#####
##== RRym.lm(nPost~nTitle+nAuthor)的擬合度
actual    = RRym$nPost
predicted = as.vector( predict(RRym.lm, newdata=RRym[,3:4]) )
R_squared = 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2);   R_squared    #-- [1] 0.8903753
##== RRym.lm1(nPost~nAuthor)的擬合度
actual    = RRym$nPost
predicted = as.vector( predict(RRym.lm1, newdata=RRym[,4]) )
R_squared1 = 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2);   R_squared1  #-- [1] 0.8901809
##== RRym.lm2(nAuthor~nTitle)的擬合度
actual    = RRym$nAuthor
predicted = as.vector( predict(RRym.lm2, newdata=RRym[,3]) )
R_squared2 = 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2);   R_squared2  #-- [1] 0.8337119


########## (R) 单元的复习 ##########

#####===== (R-HW) 演练作业HW =====#####
## 试就你爬取解析的網頁數據，或教材中提供的數據，繼續上一章的作業:
##== (HWA) 尋找哪些變量之間會有關係? (維度與量值)
##== (HWB) 試著找這些變量之間的相關性
##== (HWC) 試著找出其間的回歸模型
##== (HWD) 試著解讀所求得的回歸模型

#####===== (R-RV) 重点复习RV =====#####
##== (RVA) 建立連續屬性值(u,y)之間的函式關係y=M(u)，以預測未知值的函式值M(u_new)，稱之為"回歸(regression)"。
##== (RVB) 線性回歸式中，求取回歸係數的原理是 "LSE (最小平方誤差Least Square Error)"。
##== (RVC) 多元線性回歸中，公式(formula) y~u1+u2的意義是 "模型 y=M(u1,u2)"。
##== (RVD) 指數型非線性回歸中，一般常用的方法是將回歸式取 "對數ln" 轉成線性回歸式，來求取回歸係數。
##== (RVE) 連續型監督式學習的評估常用 均方根誤差(Root Mean Squared Error, RMSE)，表示指真實值和"預測值"間的誤差。
##== (RVF) 在回歸模型的預測實務中，當選擇適當的變量後，可以 "lm()" 來訓練變量間的線性回歸模型。

