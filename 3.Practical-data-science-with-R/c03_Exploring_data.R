# (example 3.1 of section 3.1)  : Exploring data : Using summary statistics to spot problems 
# Title: The summary() command 
setwd("/Users/juck30808/Documents/Github/USC_R_Git/3.Manning/data")
custdata <- read.table('custdata.tsv',header=TRUE,sep='\t')
summary(custdata)
# custid            sex            is.employed         income       marital.stat       health.ins      housing.type       recent.move      num.vehicles        age        state.of.res      
# Min.   :   2068   Length:1000        Mode :logical   Min.   : -8700   Length:1000        Mode :logical   Length:1000        Mode :logical   Min.   :0.000   Min.   :  0.0   Length:1000       
# 1st Qu.: 345667   Class :character   FALSE:73        1st Qu.: 14600   Class :character   FALSE:159       Class :character   FALSE:820       1st Qu.:1.000   1st Qu.: 38.0   Class :character  
# Median : 693403   Mode  :character   TRUE :599       Median : 35000   Mode  :character   TRUE :841       Mode  :character   TRUE :124       Median :2.000   Median : 50.0   Mode  :character  
# Mean   : 698500                      NA's :328       Mean   : 53505                                                         NA's :56        Mean   :1.916   Mean   : 51.7                     
# 3rd Qu.:1044606                                      3rd Qu.: 67000                                                                         3rd Qu.:2.000   3rd Qu.: 64.0                     
# Max.   :1414286                                      Max.   :615000                                                                         Max.   :6.000   Max.   :146.7                     
# NA's   :56                  
#Note1. is.employed: 大約三分之一的數據缺少is.employed變量。可變收入具有負值，這可能是無效的。
#Note2. health.ins: 約84％的客戶擁有健康保險。
#Note3. recent.move: 變量housing.type，recent.move和num.vehicles分別缺少56個值。
#Note4. age: 可變年齡的平均值似乎合理，但最小值和最大值似乎不太可能。變量state.of.res是類別變量； summary（）報告每個州（前幾個州）有多少客戶。



# (example 3.3 of section 3.1.1)  : Exploring data : Using summary statistics to spot problems : Typical problems revealed by data summaries 
# Title: Examples of invalid values and outliers 
summary(custdata$income)      #收入的負值可能表示不良數據。您是否丟棄收入為負的數據？您是否將負值轉換為零？
summary(custdata$age)         #零年齡的客戶或年齡大於110的客戶是異常值。
summary(custdata$income)      #收入從 0 到 615000 美元以上；範圍很廣。
summary(custdata$income/1000) #合理地解釋為“小時工資”或“以1000美元為單位的年收入



# (example 3.6 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Plotting a histogram 
library(ggplot2)
ggplot(custdata) + geom_histogram(aes(x=age),
                                  binwidth=5,    #binwidth 告訴 geom_histogram 如何製作五年間隔的bin
                                  fill="gray") 	 #fill參數指定直方圖條的顏色



# (example 3.7 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Producing a density plot 
library(scales) 	# Note: 1 比例軟件包引入了 $ 符號。
ggplot(custdata) + geom_density(aes(x=income)) + scale_x_continuous(labels=dollar) 	# Note: 2 將x軸標籤設置為美元。
ggplot(custdata) + geom_density(aes(x=income)) + scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) +   # Note: 1  將x軸設置為log10刻度
  annotation_logticks(sides="bt")  	# Note: 2在圖表的頂部和底部添加對數刻度刻度線。



# (informalexample 3.2 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
ggplot(custdata) + geom_bar(aes(x=marital.stat))
ggplot(custdata) + geom_bar(aes(x=state.of.res)) + coord_flip() + theme(axis.text.y=element_text(size=rel(0.8)))
# Note: 翻轉x和y軸 + 為了清晰起見，請將y軸刻度標籤的大小減小為默認大小的80％。



# (example 3.10 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Producing a bar chart with sorted categories 
statesums <- table(custdata$state.of.res)    	# Note: 1 table（）命令按居住狀態匯總數據-正是條形圖繪製的信息。
statef <- as.data.frame(statesums) 	          # Note: 2 將表轉換為數據框。默認列名稱為Var1和Freq。
colnames(statef)<-c("state.of.res", "count") 	# Note: 3 重命名列以提高可讀性。
statef <- transform(statef,state.of.res=reorder(state.of.res, count)) 	 # 讓 state.of.res變量是按計數順序排列
summary(statef)  	                            # Note: 4 state.of.res變量的默認順序為字母順序。
ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
                         stat="identity",     # Note: 7 由於數據已預先聚合傳遞到geom_bar，因此請同時指定x和y變量，並使用stat =“ identity”完全按照給定的方式繪製數據。
                         fill="gray") +
  coord_flip() + theme(axis.text.y=element_text(size=rel(0.8)))

## state.of.res     count
## Alabama   : 1    Min.   :  1.00
## Alaska    : 1    1st Qu.:  5.00
## Arizona   : 1    Median : 12.00
## Arkansas  : 1    Mean   : 20.00
## California: 1    3rd Qu.: 26.25
## Colorado  : 1    Max.   :100.00
## (Other)   :44



# (example 3.11 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Producing a line plot 
x <- runif(100)   	# Note: 1 均勻地隨機分佈在0和1之間。
y <- x^2 + 0.2*x   	# Note: 2 y 是二次函數
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()  	# Note: 3 line plot



# (example 3.12 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Examining the correlation between age and income 
custdata2 <- subset(custdata,
                    (custdata$age > 0 & custdata$age < 100 & custdata$income > 0))  # Note: 僅考慮合理年齡和收入值的數據
cor(custdata2$age, custdata2$income) 	# Note: 相關性 correlation. 
	


# (informalexample 3.3 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + ylim(0, 200000)
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + ylim(0, 200000) + stat_smooth(method="lm")
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + ylim(0, 200000) + geom_smooth()  #添加平滑曲線
  


# (example 3.13 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting the distribution of health.ins as a function of age 
ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) + geom_point(position=position_jitter(w=0.05, h=0.05)) + geom_smooth()
  # Note: 1 必須使用as.numeric將health.ins轉換為0/1變量。
  # Note: 2 由於y值只能為0或1，請添加一個小的抖動以獲得數據密度感。
  


# (example 3.14 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Producing a hexbin plot 
library(hexbin) 	# Note: 1 
ggplot(custdata2, aes(x=age, y=income)) +
  geom_hex(binwidth=c(5, 10000)) +      	# Note: 2 建立六進位制，將年齡分成5年的增量，收入以10,000美元為增量。
  geom_smooth(color="white", se=F) +    	# Note: 3 添加白色平滑曲線；禁止顯示標準錯誤功能區（se = F）。
  ylim(0,200000)



# (example 3.15 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Specifying different styles of bar chart 
ggplot(custdata) + geom_bar(aes(x=marital.stat,fill=health.ins))                  	# Note: 1 堆積條形圖
ggplot(custdata) + geom_bar(aes(x=marital.stat,fill=health.ins), position="dodge")  # Note: 2 並排條形圖
ggplot(custdata) + geom_bar(aes(x=marital.stat,fill=health.ins), position="fill")   # Note: 3 實心條形圖
                                               	


# (example 3.16 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting data with a rug
ggplot(custdata, aes(x=marital.stat)) +
  geom_bar(aes(fill=health.ins), position="fill") + 
  geom_point(aes(y=-0.05), size=0.75, alpha=0.3, 	position=position_jitter(h=0.01))
# Note 1 將點設置在y軸正下方，並使用alpha參數使其稍微透明，為了使內容清晰，請稍稍抖動點。



# (example 3.17 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting a bar chart with and without facets 
ggplot(custdata2) + 
  geom_bar(aes(x=housing.type, fill=marital.stat ),	position="dodge") +   #並排條形圖
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   	            #coord_flip命令傾斜x軸標籤，使它們不重疊。 也可以使用coord_flip（）

ggplot(custdata2) + 
  geom_bar(aes(x=marital.stat), position="dodge", fill="darkgray") +      #多面條形圖
  facet_wrap(~housing.type, scales="free_y") +               	            #按housing.type劃分圖形。
  theme(axis.text.x = element_text(angle = 45, hjust = 1))                #因此我們必須傾斜x軸標籤。
