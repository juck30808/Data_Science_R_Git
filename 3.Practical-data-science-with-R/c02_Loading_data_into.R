# example 2.1 of section 2.1.1 
# (example 2.1 of section 2.1.1)  : Loading data into R : Working with data from files : Working with well-structured data from files or URLs 
# Title: Reading the UCI car data 
setwd("/Users/juck30808/Documents/Github/USC_R_Git/3.Manning/data")

uciCar <- read.table(  	# Note: 1  讀取文件或URL並將結果存儲在名為uciCar的新數據框對像中的命令。
   'car.data.csv', 	# Note: 2  要從中獲取數據的文件名或URL。
   sep=',', 	# Note: 3  將列或字段分隔符指定為逗號。
   header=T 	# Note: 4  期望標題行定義數據列名稱。
   );uciCar



# example 2.2 of section 2.1.1 
# (example 2.2 of section 2.1.1)  : Loading data into R : Working with data from files : Working with well-structured data from files or URLs 
# Title: Exploring the car data 
class(uciCar); dim(uciCar)     ## [1] "data.frame" 	# uciCar的類型為data.frame 有1728行和7列
summary(uciCar)
# buying             maint              doors             persons            lug_boot            safety             rating         
# Length:1728        Length:1728        Length:1728        Length:1728        Length:1728        Length:1728        Length:1728       
# Class :character   Class :character   Class :character   Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  



# example 2.3 of section 2.1.2 
# (example 2.3 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Loading the credit dataset 
d <- read.table('german.data',stringsAsFactors=F,header=F);d
# d <- read.table(paste('http://archive.ics.uci.edu/ml/',
#                       'machine-learning-databases/statlog/german/german.data',sep=''),
#                 stringsAsFactors=F,header=F)d
print(d[1:3,])



# example 2.4 of section 2.1.2 
# (example 2.4 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Setting column names 
colnames(d) <- c('Status.of.existing.checking.account',
                 'Duration.in.month',  'Credit.history', 'Purpose',
                 'Credit.amount', 'Savings account/bonds',
                 'Present.employment.since',
                 'Installment.rate.in.percentage.of.disposable.income',
                 'Personal.status.and.sex', 'Other.debtors/guarantors',
                 'Present.residence.since', 'Property', 'Age.in.years',
                 'Other.installment.plans', 'Housing',
                 'Number.of.existing.credits.at.this.bank', 'Job',
                 'Number.of.people.being.liable.to.provide.maintenance.for',
                 'Telephone', 'foreign.worker', 'Good.Loan')
d$Good.Loan <- as.factor(ifelse(d$Good.Loan==1,'GoodLoan','BadLoan'))
print(d[1:3,])



# example 2.5 of section 2.1.2 
# (example 2.5 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Building a map to interpret loan use codes 
mapping <- list(
  'A40'='car (new)',
  'A41'='car (used)',
  'A42'='furniture/equipment',
  'A43'='radio/television',
  'A44'='domestic appliances'
);mapping



# example 2.6 of section 2.1.2 
# (example 2.6 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Transforming the car data 

for(i in 1:(dim(d))[2]) {             	# Note: 1 （dim（d））[2]是數據幀d中的列數。
  if(class(d[,i])=='character') {
    d[,i] <- as.factor(as.character(mapping[d[,i]]))  	
    # Note: 2 請注意，索引運算符[]是向量化的。 for循環中的每個步驟都會通過我們的列表重新映射整個數據列。
  }
}



# example 2.7 of section 2.1.2 
# (example 2.7 of section 2.1.2)  : Loading data into R : Working with data from files : Using R on less-structured data 
# Title: Summary of Good.Loan and Purpose 
table(d$Purpose,d$Good.Loan) 
##                       BadLoan GoodLoan
##   business                 34       63
##   car (new)                89      145
##   car (used)               17       86
##   domestic appliances       4        8
##   education                22       28
##   furniture/equipment      58      123
##   others                    5        7
##   radio/television         62      218
##   repairs                   8       14
##   retraining                1        8



# example 2.11 of section 2.2.2 
# (example 2.11 of section 2.2.2)  : Loading data into R : Working with relational databases : Loading data from a database into R 
# Title: Loading data into R from a relational database 
options( java.parameters = "-Xmx2g" )  	# Note: 1 在裝入數據庫驅動程序之前，將Java選項設置為額外的內存。
library(RJDBC)
drv <- JDBC("org.h2.Driver",      	# Note: 2 指定數據庫驅動程序的名稱，與我們的XML數據庫配置中的名稱相同。
            "h2-1.3.176.jar", 	    # Note: 3 指定在何處查找數據庫驅動程序的實現。
            identifier.quote="'") 	# Note: 4 必須使用大小寫混合，特殊字符或與保留字衝突的SQL列名稱。我們將單引號指定為在引用列名時將使用的引號。

options<-";LOG=0;CACHE_SIZE=65536;LOCK_MODE=0;UNDO_LOG=0"
conn <- dbConnect(drv,paste("jdbc:h2:./H2DB",options,sep=''),"u","u")
dhus <- dbGetQuery(conn,"SELECT * FROM hus WHERE ORIGRANDGROUP<=1") 	
# Note: 5從數據庫表hus中的*（所有內容）中創建一個名為dhus的數據幀，
# 僅接收ORGINRANGGROUP <= 1的行。ORGINRANDGROUP列是0到999之間的隨機整數，
# SQL Screwdriver在數據加載期間將其添加到行中方便抽樣。在這種情況下，我們要從數據行的2/1000中抽取一個小樣本。

dpus <- dbGetQuery(conn,"SELECT pus.* FROM pus WHERE pus.SERIALNO IN \
   (SELECT DISTINCT hus.SERIALNO FROM hus \
   WHERE hus.ORIGRANDGROUP<=1)") 	
# Note: 6 從數據庫表pus中創建一個名為dpus的數據框，僅從我們從家庭表hus中選擇的家庭ID集中僅記錄具有家庭ID的記錄。

dbDisconnect(conn) 	# Note: 7  斷開數據庫連接。
save(dhus,dpus,file='phsample.RData') 	# Note: 8 將兩個數據幀保存到名為phsample.RData的文件中



# example 2.12 of section 2.2.3 
# (example 2.12 of section 2.2.3)  : Loading data into R : Working with relational databases : Working with the PUMS data 
# Title: Selecting a subset of the Census data 
load('phsample.RData')
psub = subset(dpus,with(dpus,(PINCP>1000)&(ESR==1)&
                          (PINCP<=250000)&(PERNP>1000)&(PERNP<=250000)&
                          (WKHP>=40)&(AGEP>=20)&(AGEP<=50)&
                          (PWGTP1>0)&(COW %in% (1:7))&(SCHL %in% (1:24)))) 	# Note: 1 符合詳細僱用條件的數據行子集



# example 2.13 of section 2.2.3 
# (example 2.13 of section 2.2.3)  : Loading data into R : Working with relational databases : Working with the PUMS data 
# Title: Recoding variables 
psub$SEX = as.factor(ifelse(psub$SEX==1,'M','F')) 	# Note: 1  將性別從1/2重新編碼為M / F。
psub$SEX = relevel(psub$SEX,'M')                  	# Note: 2  將參考性別設為M，因此F在模型中編碼為M。
cowmap <- c("Employee of a private for-profit",
            "Private not-for-profit employee",
            "Local government employee",
            "State government employee",
            "Federal government employee",
            "Self-employed not incorporated",
            "Self-employed incorporated")
psub$COW = as.factor(cowmap[psub$COW])      	# Note: 3 將工作人員信息類重新編碼為更易讀的形式。
psub$COW = relevel(psub$COW,cowmap[1])
schlmap = c(                                 	# Note: 4 將教育信息重新編碼為更具可讀性的形式，並減少層級（將高中以下的所有層級合併為相同的編碼）。
  rep("no high school diploma",15),
  "Regular high school diploma",
  "GED or alternative credential",
  "some college credit, no degree",
  "some college credit, no degree",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Professional degree",
  "Doctorate degree")
psub$SCHL = as.factor(schlmap[psub$SCHL])
psub$SCHL = relevel(psub$SCHL,schlmap[1])
dtrain = subset(psub,ORIGRANDGROUP >= 500)  	# Note: 5  模型訓練的數據行的子集。
dtest = subset(psub,ORIGRANDGROUP < 500)    	# Note: 6  模型測試的數據行的子集。

summary(dtrain$COW)
## Employee of a private for-profit      Federal government employee
##                              423                               21
##        Local government employee  Private not-for-profit employee
##                               39                               55
##       Self-employed incorporated   Self-employed not incorporated
##                               17                               16
##        State government employee
##                               24
