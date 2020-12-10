# example 3.1 of section 3.1 
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

#Note1. 大約三分之一的數據缺少is.employed變量。可變收入具有負值，這可能是無效的。
#Note2. 約84％的客戶擁有健康保險。
#Note3. 變量housing.type，recent.move和num.vehicles分別缺少56個值。
#Note4. 可變年齡的平均值似乎合理，但最小值和最大值似乎不太可能。變量state.of.res是類別變量； summary（）報告每個州（前幾個州）有多少客戶。



# example 3.3 of section 3.1.1 
# (example 3.3 of section 3.1.1)  : Exploring data : Using summary statistics to spot problems : Typical problems revealed by data summaries 
# Title: Examples of invalid values and outliers 
summary(custdata$income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -8700   14600   35000   53505   67000  615000 	
# Note: 1 收入的負值可能表示不良數據。它們也可能有特殊含義，例如“債務額”。無論哪種方式，
# 您都應該檢查問題的普遍性，然後決定要怎麼做：您是否丟棄收入為負的數據？您是否將負值轉換為零？
summary(custdata$age)
##    Min. 1st Qu.  Median    Mean 3rd Qu.   Max.
##     0.0    38.0    50.0    51.7    64.0   146.7
# Note: 2 零年齡的客戶或年齡大於110的客戶是異常值。他們不在預期的客戶價值範圍內。離群值可能是數據輸入錯誤。
#它們可能是特殊的哨兵值：零可能表示“年齡未知”或“拒絕陳述”。而且您的某些客戶可能壽命特別長。



# example 3.4 of section 3.1.1 
# (example 3.4 of section 3.1.1)  : Exploring data : Using summary statistics to spot problems : Typical problems revealed by data summaries 
# Title: Looking at the data range of a variable 
summary(custdata$income)
##    Min. 1st Qu.  Median    Mean 3rd Qu.   Max.
##   -8700   14600   35000   53500   67000   615000
# Note: 1 收入從零到一百萬美元以上；範圍很廣。



# example 3.5 of section 3.1.1 
# (example 3.5 of section 3.1.1)  : Exploring data : Using summary statistics to spot problems : Typical problems revealed by data summaries 
# Title: Checking units sounds silly, but mistakes can lead to spectacular errors if not caught 
Income = custdata$income/1000
summary(Income)                                	# Note: 1 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    -8.7    14.6    35.0    53.5    67.0   615.0
## Note 1: 變量收入定義為收入= 收入/ 1000。這些值可以合理地解釋為“小時工資”或“以1000美元為單位的年收入”。



# example 3.6 of section 3.2.1 
# (example 3.6 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Plotting a histogram 
library(ggplot2)
ggplot(custdata) + geom_histogram(aes(x=age),binwidth=5, fill="gray") 	
# Note: binwidth參數binwidth參數告訴geom_histogram調用如何製作五年間隔的bin（默認為datarange / 30）。 fill參數指定直方圖條的顏色（默認值：黑色）。



# example 3.7 of section 3.2.1 
# (example 3.7 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Producing a density plot 
library(scales) 	# Note: 1 比例軟件包引入了美元比例符號。
ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_continuous(labels=dollar) 	# Note: 2 將x軸標籤設置為美元。



# example 3.8 of section 3.2.1 
# (example 3.8 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Creating a log-scaled density plot 
ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) +  	# Note: 1  將x軸設置為log10刻度，並手動將刻度點和標籤設置為美元。
  annotation_logticks(sides="bt")  	# Note: 2在圖表的頂部和底部添加對數刻度刻度線。



# informalexample 3.2 of section 3.2.1 
# (informalexample 3.2 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")



# example 3.9 of section 3.2.1 
# (example 3.9 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Producing a horizontal bar chart 
ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill="gray") +  	  # Note: 1 繪製條形圖：state.of.res在x軸上，計數在y軸上。
  coord_flip() + 	                                  # Note: 2 翻轉x和y軸
  theme(axis.text.y=element_text(size=rel(0.8)))  	# Note: 3 為了清晰起見，請將y軸刻度標籤的大小減小為默認大小的80％。



# example 3.10 of section 3.2.1 
# (example 3.10 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 
# Title: Producing a bar chart with sorted categories 
statesums <- table(custdata$state.of.res)    	# Note: 1 table（）命令按居住狀態匯總數據-正是條形圖繪製的信息。
statef <- as.data.frame(statesums) 	          # Note: 2 將表轉換為數據框。默認列名稱為Var1和Freq。
colnames(statef)<-c("state.of.res", "count") 	# Note: 3 重命名列以提高可讀性。
summary(statef)  	                            # Note: 4 state.of.res變量的默認順序為字母順序。
## state.of.res     count
## Alabama   : 1    Min.   :  1.00
## Alaska    : 1    1st Qu.:  5.00
## Arizona   : 1    Median : 12.00
## Arkansas  : 1    Mean   : 20.00
## California: 1    3rd Qu.: 26.25
## Colorado  : 1    Max.   :100.00
## (Other)   :44
statef <- transform(statef,
                    state.of.res=reorder(state.of.res, count)) 	
# Note: 5 使用reorder（）函數將state.of.res變量設置為要進行計數排序。使用transform（）函數將轉換應用於state.of.res數據幀。
summary(statef)                       	
# Note: 6 現在，state.of.res變量是按計數順序排列的。
## state.of.res     count
## Delaware    : 1    Min.   :  1.00
## North Dakota: 1    1st Qu.:  5.00
## Wyoming     : 1    Median : 12.00
## Rhode Island: 1    Mean   : 20.00
## Alaska      : 1    3rd Qu.: 26.25
## Montana     : 1    Max.   :100.00
## (Other)     :44
ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
                         stat="identity",              	# Note: 7 由於數據已預先聚合傳遞到geom_bar，因此請同時指定x和y變量，並使用stat =“ identity”完全按照給定的方式繪製數據。
                         fill="gray") +
  coord_flip() +                                       	# Note: 8 像以前一樣翻轉軸並減小標籤文本的大小。
  theme(axis.text.y=element_text(size=rel(0.8)))



# example 3.11 of section 3.2.2 
# (example 3.11 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Producing a line plot 
x <- runif(100)   	# Note: 1 
y <- x^2 + 0.2*x   	# Note: 2 
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()  	# Note: 3

# Note 1: 
#   First, generate the data for this example. 
#   The x variable is uniformly randomly distributed 
#   between 0 and 1. 

# Note 2: 
#   The y variable is a 
#   quadratic function of x. 

# Note 3: 
#   Plot the line plot. 





# example 3.12 of section 3.2.2 
# (example 3.12 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Examining the correlation between age and income 

custdata2 <- subset(custdata,
                    (custdata$age > 0 & custdata$age < 100
                     & custdata$income > 0))                  	# Note: 1 

cor(custdata2$age, custdata2$income) 	# Note: 2 

## [1] -0.02240845 	# Note: 3

# Note 1: 
#   Only consider a subset of data with 
#   reasonable age and income values. 

# Note 2: 
#   Get correlation of age and income. 

# Note 3: 
#   Resulting correlation. 





# informalexample 3.3 of section 3.2.2 
# (informalexample 3.3 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 

ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + ylim(0, 200000)




# informalexample 3.4 of section 3.2.2 
# (informalexample 3.4 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 

ggplot(custdata2, aes(x=age, y=income)) + geom_point() +
  stat_smooth(method="lm") +
  ylim(0, 200000)




# informalexample 3.5 of section 3.2.2 
# (informalexample 3.5 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 

ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + geom_smooth() +
  ylim(0, 200000)




# example 3.13 of section 3.2.2 
# (example 3.13 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting the distribution of health.ins as a function of age 

ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) + 	# Note: 1 
  geom_point(position=position_jitter(w=0.05, h=0.05)) +  	# Note: 2 
  geom_smooth() 	# Note: 3

# Note 1: 
#   The Boolean variable health.ins must be 
#   converted to a 0/1 variable using as.numeric. 

# Note 2: 
#   Since y values can 
#   only be 0 or 1, add a small jitter to get a sense of data 
#   density. 

# Note 3: 
#   Add smoothing curve. 




# example 3.14 of section 3.2.2 
# (example 3.14 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Producing a hexbin plot 

library(hexbin) 	# Note: 1 

ggplot(custdata2, aes(x=age, y=income)) +
  geom_hex(binwidth=c(5, 10000)) +   	# Note: 2 
  geom_smooth(color="white", se=F) +  	# Note: 3 
  ylim(0,200000)

# Note 1: 
#   Load hexbin library. 

# Note 2: 
#   Create hexbin with age binned into 5-year 
#   increments, income in increments of $10,000. 

# Note 3: 
#   Add smoothing curve in white; suppress 
#   standard error ribbon (se=F). 




# example 3.15 of section 3.2.2 
# (example 3.15 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Specifying different styles of bar chart 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins)) 	# Note: 1 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="dodge")      	# Note: 2 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="fill")        	# Note: 3

# Note 1: 
#   Stacked bar chart, the 
#   default 

# Note 2: 
#   Side-by-side bar chart 

# Note 3: 
#   Filled bar chart 



# example 3.16 of section 3.2.2 
# (example 3.16 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting data with a rug 

ggplot(custdata, aes(x=marital.stat)) +
  geom_bar(aes(fill=health.ins), position="fill") +
  geom_point(aes(y=-0.05), size=0.75, alpha=0.3, 	# Note: 1 
             position=position_jitter(h=0.01)) 	# Note: 2

# Note 1: 
#   Set the points just under the y-axis, 
#   three-quarters of default size, and make them slightly transparent with 
#   the alpha parameter. 

# Note 2: 
#   Jitter the points slightly for 
#   legibility. 

# example 3.17 of section 3.2.2 
# (example 3.17 of section 3.2.2)  : Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Plotting a bar chart with and without facets 

ggplot(custdata2) +                                          	# Note: 1 
  geom_bar(aes(x=housing.type, fill=marital.stat ),
           position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   	# Note: 2 

ggplot(custdata2) +                                          	# Note: 3 
  geom_bar(aes(x=marital.stat), position="dodge",
           fill="darkgray") +
  facet_wrap(~housing.type, scales="free_y") +               	# Note: 4 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   	# Note: 5

# Note 1: 
#   Side-by-side bar chart. 

# Note 2: 
#   coord_flip commandTilt the x-axis labels so they 
#   don’t overlap. You can also use coord_flip() to rotate the graph, as we 
#   saw previously. Some prefer coord_flip() because the theme() layer is 
#   complicated to use. 

# Note 3: 
#   The faceted bar chart. 

# Note 4: 
#   Facet the graph by housing.type. The scales="free_y" argument specifies that each facet has 
#   an independently scaled y-axis (the default is that all facets have 
#   the same scales on both axes). The argument free_x would free the 
#   x-axis scaling, and the argument free frees both axes. 

# Note 5: 
#   As of this writing, 
#   facet_wrap is incompatible with coord_flip, so we have to tilt the 
#   x-axis labels. 

